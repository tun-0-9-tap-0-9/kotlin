/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.fir.analysis.checkers.declaration

import org.jetbrains.kotlin.descriptors.ClassKind
import org.jetbrains.kotlin.descriptors.Modality
import org.jetbrains.kotlin.descriptors.Visibilities
import org.jetbrains.kotlin.fir.analysis.checkers.*
import org.jetbrains.kotlin.fir.analysis.checkers.context.CheckerContext
import org.jetbrains.kotlin.fir.analysis.diagnostics.DiagnosticReporter
import org.jetbrains.kotlin.fir.analysis.diagnostics.FirErrors
import org.jetbrains.kotlin.fir.analysis.diagnostics.reportOn
import org.jetbrains.kotlin.fir.analysis.diagnostics.withSuppressedDiagnostics
import org.jetbrains.kotlin.fir.declarations.*
import org.jetbrains.kotlin.fir.resolve.fullyExpandedType
import org.jetbrains.kotlin.fir.resolve.toSymbol
import org.jetbrains.kotlin.fir.symbols.AbstractFirBasedSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirRegularClassSymbol
import org.jetbrains.kotlin.fir.types.*
import org.jetbrains.kotlin.name.StandardClassIds
import org.jetbrains.kotlin.utils.addToStdlib.safeAs

object FirSupertypesChecker : FirClassChecker() {
    override fun check(declaration: FirClass<*>, context: CheckerContext, reporter: DiagnosticReporter) {
        val isInterface = declaration.classKind == ClassKind.INTERFACE
        var nullableSupertypeReported = false
        var extensionFunctionSupertypeReported = false
        var interfaceWithSuperclassReported = !isInterface
        var finalSupertypeReported = false
        var singletonInSupertypeReported = false
        var classAppeared = false
        val superClassSymbols = hashSetOf<FirRegularClassSymbol>()
        for (superTypeRef in declaration.superTypeRefs) {
            withSuppressedDiagnostics(superTypeRef, context) {
                val coneType = superTypeRef.coneType
                if (!nullableSupertypeReported && coneType.nullability == ConeNullability.NULLABLE) {
                    reporter.reportOn(superTypeRef.source, FirErrors.NULLABLE_SUPERTYPE, context)
                    nullableSupertypeReported = true
                }
                if (!extensionFunctionSupertypeReported && coneType.isExtensionFunctionType) {
                    reporter.reportOn(superTypeRef.source, FirErrors.SUPERTYPE_IS_EXTENSION_FUNCTION_TYPE, context)
                    extensionFunctionSupertypeReported = true
                }
                val lookupTag = coneType.safeAs<ConeClassLikeType>()?.lookupTag ?: return@withSuppressedDiagnostics
                val superTypeFir = lookupTag.toSymbol(context.session)?.fir

                if (superTypeFir is FirRegularClass) {
                    if (!superClassSymbols.add(superTypeFir.symbol)) {
                        reporter.reportOn(superTypeRef.source, FirErrors.SUPERTYPE_APPEARS_TWICE, context)
                    }
                    if (superTypeFir.classKind != ClassKind.INTERFACE) {
                        if (classAppeared) {
                            reporter.reportOn(superTypeRef.source, FirErrors.MANY_CLASSES_IN_SUPERTYPE_LIST, context)
                        } else {
                            classAppeared = true
                        }
                        if (!interfaceWithSuperclassReported) {
                            reporter.reportOn(superTypeRef.source, FirErrors.INTERFACE_WITH_SUPERCLASS, context)
                            interfaceWithSuperclassReported = true
                        }
                    }
                    val isObject = superTypeFir.classKind == ClassKind.OBJECT
                    if (!finalSupertypeReported && !isObject && superTypeFir.modality == Modality.FINAL) {
                        reporter.reportOn(superTypeRef.source, FirErrors.FINAL_SUPERTYPE, context)
                        finalSupertypeReported = true
                    }
                    if (!singletonInSupertypeReported && isObject) {
                        reporter.reportOn(superTypeRef.source, FirErrors.SINGLETON_IN_SUPERTYPE, context)
                        singletonInSupertypeReported = true
                    }
                }

                checkAnnotationOnSuperclass(superTypeRef, context, reporter)

                val fullyExpandedType = coneType.fullyExpandedType(context.session)
                val symbol = fullyExpandedType.toSymbol(context.session)

                checkClassCannotBeExtendedDirectly(symbol, reporter, superTypeRef, context)

                if (coneType.typeArguments.isNotEmpty()) {
                    checkProjectionInImmediateArgumentToSupertype(coneType, superTypeRef, reporter, context)
                } else {
                    checkExpandedTypeCannotBeInherited(symbol, fullyExpandedType, reporter, superTypeRef, coneType, context)
                }
            }
        }

        checkDelegationNotToInterface(declaration, context, reporter)

        if (declaration is FirRegularClass && declaration.superTypeRefs.size > 1) {
            checkInconsistentTypeParameters(listOf(Pair(null, declaration)), context, reporter, declaration.source, true)
        }
    }

    private fun checkAnnotationOnSuperclass(
        superTypeRef: FirTypeRef,
        context: CheckerContext,
        reporter: DiagnosticReporter
    ) {
        for (annotation in superTypeRef.annotations) {
            withSuppressedDiagnostics(annotation, context) {
                if (annotation.useSiteTarget != null) {
                    reporter.reportOn(annotation.source, FirErrors.ANNOTATION_ON_SUPERCLASS, context)
                }
            }
        }
    }

    private fun checkClassCannotBeExtendedDirectly(
        symbol: AbstractFirBasedSymbol<*>?,
        reporter: DiagnosticReporter,
        superTypeRef: FirTypeRef,
        context: CheckerContext
    ) {
        if (symbol is FirRegularClassSymbol && symbol.classId == StandardClassIds.Enum) {
            reporter.reportOn(superTypeRef.source, FirErrors.CLASS_CANNOT_BE_EXTENDED_DIRECTLY, symbol, context)
        }
    }

    private fun checkProjectionInImmediateArgumentToSupertype(
        coneType: ConeKotlinType,
        superTypeRef: FirTypeRef,
        reporter: DiagnosticReporter,
        context: CheckerContext
    ) {
        for ((index, typeArgument) in coneType.typeArguments.withIndex()) {
            if (typeArgument.isConflictingOrNotInvariant) {
                val (_, argSource) = extractArgumentTypeRefAndSource(superTypeRef, index) ?: continue
                reporter.reportOn(
                    argSource ?: superTypeRef.source,
                    FirErrors.PROJECTION_IN_IMMEDIATE_ARGUMENT_TO_SUPERTYPE,
                    context
                )
            }
        }
    }

    private fun checkExpandedTypeCannotBeInherited(
        symbol: AbstractFirBasedSymbol<*>?,
        fullyExpandedType: ConeKotlinType,
        reporter: DiagnosticReporter,
        superTypeRef: FirTypeRef,
        coneType: ConeKotlinType,
        context: CheckerContext
    ) {
        if (symbol is FirRegularClassSymbol && symbol.fir.classKind == ClassKind.INTERFACE) {
            for (typeArgument in fullyExpandedType.typeArguments) {
                if (typeArgument.isConflictingOrNotInvariant) {
                    reporter.reportOn(superTypeRef.source, FirErrors.EXPANDED_TYPE_CANNOT_BE_INHERITED, coneType, context)
                    break
                }
            }
        }
    }

    private fun checkDelegationNotToInterface(
        declaration: FirClass<*>,
        context: CheckerContext,
        reporter: DiagnosticReporter
    ) {
        for (subDeclaration in declaration.declarations) {
            if (subDeclaration is FirField) {
                if (subDeclaration.visibility == Visibilities.Local &&
                    subDeclaration.name.isSpecial &&
                    subDeclaration.name.isDelegated
                ) {
                    val subDeclFir = subDeclaration.returnTypeRef.toRegularClass(context.session)
                    if (subDeclFir is FirRegularClass && subDeclFir.classKind != ClassKind.INTERFACE) {
                        reporter.reportOn(subDeclaration.returnTypeRef.source, FirErrors.DELEGATION_NOT_TO_INTERFACE, context)
                    }
                }
            }
        }
    }
}

/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.resolve.checkers

import org.jetbrains.kotlin.config.LanguageFeature
import org.jetbrains.kotlin.descriptors.DeclarationDescriptor
import org.jetbrains.kotlin.diagnostics.Errors
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.psi.*
import org.jetbrains.kotlin.psi.psiUtil.containingClass
import org.jetbrains.kotlin.resolve.BindingContext

object ConstexprDeclarationChecker : DeclarationChecker {
    private val compileTimeAnnotationName = FqName("kotlin.CompileTimeCalculation")

    override fun check(declaration: KtDeclaration, descriptor: DeclarationDescriptor, context: DeclarationCheckerContext) {
        if (!context.languageVersionSettings.supportsFeature(LanguageFeature.CompileTimeCalculations)) return
        if (!descriptor.annotations.hasAnnotation(compileTimeAnnotationName)) return

        val ktClass = when (declaration) {
            is KtClass -> declaration
            is KtPrimaryConstructor -> declaration.getContainingClassOrObject()
            else -> return
        }

        val overriddenDeclaration = ktClass.declarations
            .filterIsInstance<KtNamedDeclaration>()
            .filter { it.hasModifier(KtTokens.OVERRIDE_KEYWORD) }
        for (ktDeclaration in overriddenDeclaration) {
            val annotationEntries = ktDeclaration.annotationEntries
            val classAnnotationEntries = ktDeclaration.containingClass()?.annotationEntries

            fun List<KtAnnotationEntry>?.containsCompileTimeAnnotation(): Boolean {
                this ?: return false
                return this.any { context.trace.bindingContext[BindingContext.ANNOTATION, it]?.fqName == compileTimeAnnotationName }
            }

            if (!annotationEntries.containsCompileTimeAnnotation() && !classAnnotationEntries.containsCompileTimeAnnotation()) {
                context.trace.report(Errors.COMPILE_TIME_MEMBER_NOT_IMPLEMENTED.on(ktDeclaration, ktClass, ktDeclaration))
            }
        }
    }
}
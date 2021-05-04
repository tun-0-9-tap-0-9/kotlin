/*
 * Copyright 2010-2021 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.backend.common.descriptors

import org.jetbrains.kotlin.backend.common.CommonBackendContext
import org.jetbrains.kotlin.backend.common.ir.Symbols
import org.jetbrains.kotlin.builtins.BuiltInsPackageFragment
import org.jetbrains.kotlin.builtins.KotlinBuiltIns
import org.jetbrains.kotlin.builtins.PrimitiveType
import org.jetbrains.kotlin.descriptors.SimpleFunctionDescriptor
import org.jetbrains.kotlin.incremental.components.NoLookupLocation
import org.jetbrains.kotlin.ir.IrBuiltIns
import org.jetbrains.kotlin.ir.ObsoleteDescriptorBasedAPI
import org.jetbrains.kotlin.ir.descriptors.IrBuiltInsOverDescriptors
import org.jetbrains.kotlin.ir.symbols.IrClassSymbol
import org.jetbrains.kotlin.ir.symbols.IrClassifierSymbol
import org.jetbrains.kotlin.ir.symbols.IrSimpleFunctionSymbol
import org.jetbrains.kotlin.ir.util.SymbolTable
import org.jetbrains.kotlin.ir.util.referenceClassifier
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.resolve.calls.components.isVararg
import org.jetbrains.kotlin.util.capitalizeDecapitalize.toLowerCaseAsciiOnly

abstract class SymbolsOverDescriptors<out T : CommonBackendContext>(context: T, irBuiltIns: IrBuiltIns, symbolTable: SymbolTable) :
    Symbols<T>(context, irBuiltIns, symbolTable)
{
    @OptIn(ObsoleteDescriptorBasedAPI::class)
    protected val builtIns: KotlinBuiltIns = (irBuiltIns as IrBuiltInsOverDescriptors).builtIns

    protected fun builtInsPackage(vararg packageNameSegments: String) =
        builtIns.builtInsModule.getPackage(FqName.fromSegments(listOf(*packageNameSegments))).memberScope

    // NOTE: the comment is probab;y obsolete
    // consider making this public so it can be used to easily locate stdlib functions from any place (in particular, plugins and lowerings)
    @OptIn(ObsoleteDescriptorBasedAPI::class)
    private fun getSimpleFunction(
        name: Name,
        vararg packageNameSegments: String = arrayOf("kotlin"),
        condition: (SimpleFunctionDescriptor) -> Boolean
    ): IrSimpleFunctionSymbol =
        irBuiltIns.findFunctions(name, *packageNameSegments).first { condition(it.descriptor as SimpleFunctionDescriptor) }

    override val getProgressionLastElementByReturnType: Map<IrClassifierSymbol?, IrSimpleFunctionSymbol> =
        builtInsPackage("kotlin", "internal")
            .getContributedFunctions(Name.identifier("getProgressionLastElement"), NoLookupLocation.FROM_BACKEND)
            .filter { it.containingDeclaration !is BuiltInsPackageFragment }
            .map { d ->
                val klass = d.returnType?.constructor?.declarationDescriptor?.let { symbolTable.referenceClassifier(it) }
                val function = symbolTable.referenceSimpleFunction(d)
                klass to function
            }
            .toMap()

    override val toUIntByExtensionReceiver: Map<IrClassifierSymbol, IrSimpleFunctionSymbol> =
        builtInsPackage("kotlin").getContributedFunctions(
            Name.identifier("toUInt"),
            NoLookupLocation.FROM_BACKEND
        ).filter { it.containingDeclaration !is BuiltInsPackageFragment && it.extensionReceiverParameter != null }
            .map {
                val klass = symbolTable.referenceClassifier(it.extensionReceiverParameter!!.type.constructor.declarationDescriptor!!)
                val function = symbolTable.referenceSimpleFunction(it)
                klass to function
            }.toMap()

    override val toULongByExtensionReceiver: Map<IrClassifierSymbol, IrSimpleFunctionSymbol> =
        builtInsPackage("kotlin").getContributedFunctions(
            Name.identifier("toULong"),
            NoLookupLocation.FROM_BACKEND
        ).filter { it.containingDeclaration !is BuiltInsPackageFragment && it.extensionReceiverParameter != null }
            .map {
                val klass = symbolTable.referenceClassifier(it.extensionReceiverParameter!!.type.constructor.declarationDescriptor!!)
                val function = symbolTable.referenceSimpleFunction(it)
                klass to function
            }.toMap()

    override val extensionToString: IrSimpleFunctionSymbol = getSimpleFunction(Name.identifier("toString")) {
        it.dispatchReceiverParameter == null && it.extensionReceiverParameter != null &&
                KotlinBuiltIns.isNullableAny(it.extensionReceiverParameter!!.type) && it.valueParameters.size == 0
    }

    override val stringPlus: IrSimpleFunctionSymbol = getSimpleFunction(Name.identifier("plus")) {
        it.dispatchReceiverParameter == null && it.extensionReceiverParameter != null &&
                KotlinBuiltIns.isStringOrNullableString(it.extensionReceiverParameter!!.type) && it.valueParameters.size == 1 &&
                KotlinBuiltIns.isNullableAny(it.valueParameters.first().type)
    }
}
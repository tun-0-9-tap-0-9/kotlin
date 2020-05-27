/*
 * Copyright 2010-2021 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.ir.backend.jvm.serialization

import org.jetbrains.kotlin.backend.common.serialization.*
import org.jetbrains.kotlin.backend.common.serialization.encodings.BinaryNameAndType
import org.jetbrains.kotlin.backend.common.serialization.proto.*
import org.jetbrains.kotlin.backend.common.serialization.proto.IrFile
import org.jetbrains.kotlin.descriptors.ModuleDescriptor
import org.jetbrains.kotlin.ir.ObsoleteDescriptorBasedAPI
import org.jetbrains.kotlin.ir.declarations.*
import org.jetbrains.kotlin.ir.declarations.impl.IrVariableImpl
import org.jetbrains.kotlin.ir.expressions.IrBody
import org.jetbrains.kotlin.ir.symbols.IrPropertySymbol
import org.jetbrains.kotlin.ir.symbols.IrSymbol
import org.jetbrains.kotlin.ir.util.DeclarationStubGenerator
import org.jetbrains.kotlin.ir.util.IdSignature
import org.jetbrains.kotlin.ir.util.withScope
import org.jetbrains.kotlin.library.IrLibrary
import org.jetbrains.kotlin.load.kotlin.JvmPackagePartSource
import org.jetbrains.kotlin.resolve.descriptorUtil.classId
import org.jetbrains.kotlin.resolve.descriptorUtil.fqNameSafe
import org.jetbrains.kotlin.serialization.deserialization.descriptors.DeserializedClassDescriptor
import org.jetbrains.kotlin.serialization.deserialization.descriptors.DeserializedSimpleFunctionDescriptor
import org.jetbrains.kotlin.backend.common.serialization.proto.IrAnonymousInit as ProtoAnonymousInit
import org.jetbrains.kotlin.backend.common.serialization.proto.IrConstructor as ProtoConstructor
import org.jetbrains.kotlin.backend.common.serialization.proto.IrConstructorCall as ProtoConstructorCall
import org.jetbrains.kotlin.backend.common.serialization.proto.IrEnumEntry as ProtoEnumEntry
import org.jetbrains.kotlin.backend.common.serialization.proto.IrFunction as ProtoFunction
import org.jetbrains.kotlin.backend.common.serialization.proto.IrProperty as ProtoProperty
import org.jetbrains.kotlin.backend.common.serialization.proto.IrField as ProtoField
import org.jetbrains.kotlin.backend.common.serialization.proto.IrClass as ProtoClass
import org.jetbrains.kotlin.backend.common.serialization.proto.IrValueParameter as ProtoValueParameter
import org.jetbrains.kotlin.backend.common.serialization.proto.IrVariable as ProtoVariable
import org.jetbrains.kotlin.backend.common.serialization.proto.IrFunctionBase as ProtoFunctionBase

class KlibModuleDeserializer(linker: KotlinIrLinker, moduleDescriptor: ModuleDescriptor, klib: IrLibrary, val stubGenerator: DeclarationStubGenerator) :
    BasicIrModuleDeserializer(linker, moduleDescriptor, klib, DeserializationStrategy.ONLY_REFERENCED) {

    override fun createFileDeserializationState(
        file: org.jetbrains.kotlin.ir.declarations.IrFile,
        fileReader: IrLibraryFile,
        fileProto: IrFile,
        fileIndex: Int,
        moduleDeserializer: IrModuleDeserializer,
        allowErrorNodes: Boolean
    ): FileDeserializationState {
        return KlibFileDeserializationState(
            stubGenerator,
            linker,
            file,
            fileReader,
            fileProto,
            strategy.needBodies,
            allowErrorNodes,
            strategy.inlineBodies,
            moduleDeserializer,
            linker::handleNoModuleDeserializerFound
        )
    }

    fun declare(irSymbol: IrSymbol) {
        val idSig = irSymbol.signature!!
        assert(idSig.isPublic)

        val fileLocalDeserializationState = scheduleTopLevelSignatureDeserialization(idSig)

        if (idSig != idSig.topLevelSignature()) {
            super.declareIrSymbol(irSymbol)
        } else {
            val symbol = fileLocalDeserializationState.fileDeserializer.symbolDeserializer.deserializedSymbols[idSig]
            if (symbol == null) {
                fileLocalDeserializationState.fileDeserializer.deserializeDeclaration(idSig)
            }
        }
    }

    fun getBodies(): Map<IdSignature, IrBody> {
        return mutableMapOf<IdSignature, IrBody>().apply {
            fileToDeserializerMap.forEach { putAll((it.value.declarationDeserializer as KlibDeclarationDeserializer).bodies) }
        }
    }
}


open class KlibFileDeserializationState(
    val stubGenerator: DeclarationStubGenerator,
    linker: KotlinIrLinker,
    file: org.jetbrains.kotlin.ir.declarations.IrFile,
    fileReader: IrLibraryFile,
    fileProto: IrFile,
    deserializeBodies: Boolean,
    allowErrorNodes: Boolean,
    deserializeInlineFunctions: Boolean,
    moduleDeserializer: IrModuleDeserializer,
    handleNoModuleDeserializerFound: (IdSignature, ModuleDescriptor, Collection<IrModuleDeserializer>) -> IrModuleDeserializer,
) : FileDeserializationState(linker, file, fileReader, fileProto, deserializeBodies, allowErrorNodes, deserializeInlineFunctions, moduleDeserializer, handleNoModuleDeserializerFound) {
    override val declarationDeserializer = KlibDeclarationDeserializer(stubGenerator, linker, fileReader, file, allowErrorNodes, deserializeInlineFunctions, deserializeBodies, symbolDeserializer)
}

class KlibDeclarationDeserializer(
    val stubGenerator: DeclarationStubGenerator,
    linker: KotlinIrLinker,
    fileReader: IrLibraryFile,
    file: org.jetbrains.kotlin.ir.declarations.IrFile,
    allowErrorNodes: Boolean,
    deserializeInlineFunctions: Boolean,
    deserializeBodies: Boolean,
    symbolDeserializer: IrSymbolDeserializer
) : IrDeclarationDeserializer(
    linker.builtIns,
    linker.symbolTable,
    linker.symbolTable.irFactory,
    fileReader,
    file,
    allowErrorNodes,
    deserializeInlineFunctions,
    deserializeBodies,
    symbolDeserializer,
    linker.fakeOverrideBuilder.platformSpecificClassFilter,
    linker.fakeOverrideBuilder,
) {
    val bodies = mutableMapOf<IdSignature, IrBody>()

    override fun deserializeIrValueParameter(proto: ProtoValueParameter, index: Int): IrValueParameter {
        val (symbol, _) = symbolDeserializer.deserializeIrSymbolToDeclare(proto.base.symbol)

        if (symbol.isBound) {
            return (symbol.owner as IrValueParameter).apply {
                if (proto.hasDefaultValue()) defaultValue = deserializeExpressionBody(proto.defaultValue)
            }
        }

        return super.deserializeIrValueParameter(proto, index)
    }

    @OptIn(ObsoleteDescriptorBasedAPI::class)
    override fun deserializeIrClass(proto: ProtoClass): IrClass {
        val (symbol, _) = symbolDeserializer.deserializeIrSymbolToDeclare(proto.base.symbol)

        val classId = (symbol.takeIf { symbol.hasDescriptor }?.descriptor as? DeserializedClassDescriptor)?.classId
        val packageName = classId?.packageFqName?.toString()
        val className = classId?.relativeClassName?.toString()
        // unsigned and reflect classes are deserialized to early (with Any), but their owner's must be created with stub generator
        val isReflect = packageName == "kotlin.reflect" && (className == "KClass" || className == "KCallable" || className == "KFunction")
        val isUnsigned = packageName == "kotlin" && (className == "UShort" || className == "UByte" || className == "UInt" || className == "ULong")
        if (symbol.isBound || isReflect || isUnsigned) {
            val owner = if (symbol.isBound) symbol.owner else stubGenerator.generateMemberStub(symbol.descriptor)
            return (owner as IrClass).apply {
                usingParent {
                    typeParameters = deserializeTypeParameters(proto.typeParameterList, true)
                    proto.declarationList.forEach { deserializeDeclaration(it) }
                }
            }
        }

        return super.deserializeIrClass(proto)
    }

    private fun <T : IrFunction> T.deserializeIrFunctionBase(proto: ProtoFunctionBase) =
        this.usingParent {
            typeParameters = deserializeTypeParameters(proto.typeParameterList, false)
            valueParameters = deserializeValueParameters(proto.valueParameterList)

            val nameType = BinaryNameAndType.decode(proto.nameType)
            returnType = deserializeIrType(nameType.typeIndex)

            withBodyGuard {
                if (proto.hasDispatchReceiver())
                    dispatchReceiverParameter = deserializeIrValueParameter(proto.dispatchReceiver, -1)
                if (proto.hasExtensionReceiver())
                    extensionReceiverParameter = deserializeIrValueParameter(proto.extensionReceiver, -1)
                if (proto.hasBody()) {
                    body = deserializeStatementBody(proto.body) as IrBody
                }
            }
        }

    private fun deserializeFunctionBase(symbol: IrSymbol, proto: ProtoFunctionBase): IrFunction {
        val irFunction = symbol.owner as IrFunction
        try {
            recordDelegatedSymbol(symbol)
            val oldBody = irFunction.body   // must save old values in case if JvmIrLinker already created stub
            val oldReturnType = irFunction.returnType
            val result = symbolTable.withScope(symbol) {
                irFunction.deserializeIrFunctionBase(proto)
            }
            result.body?.let { bodies[symbol.signature!!] = it }
            result.annotations += deserializeAnnotations(proto.base.annotationList)
            result.body = oldBody
            irFunction.returnType = oldReturnType
            return result
        } finally {
            eraseDelegatedSymbol(symbol)
        }
    }

    @OptIn(ObsoleteDescriptorBasedAPI::class)
    override fun deserializeIrFunction(proto: ProtoFunction): IrSimpleFunction {
        val (symbol, _) = symbolDeserializer.deserializeIrSymbolToDeclare(proto.base.base.symbol)

        if (!symbol.isBound && symbol.hasDescriptor && (symbol.descriptor as? DeserializedSimpleFunctionDescriptor)?.containerSource is JvmPackagePartSource) {
            // cover case when symbol is presented in files and owner must be created by JvmIrLinker
            return stubGenerator.generateMemberStub(symbol.descriptor).let {
                deserializeFunctionBase(symbol, proto.base) as IrSimpleFunction
            }
        }
        if (symbol.isBound) {
            return deserializeFunctionBase(symbol, proto.base) as IrSimpleFunction
        }

        return super.deserializeIrFunction(proto)
    }

    override fun deserializeIrConstructor(proto: ProtoConstructor): IrConstructor {
        val (symbol, _) = symbolDeserializer.deserializeIrSymbolToDeclare(proto.base.base.symbol)

        if (symbol.isBound) {
            return deserializeFunctionBase(symbol, proto.base) as IrConstructor
        }

        return super.deserializeIrConstructor(proto)
    }

    override fun deserializeIrVariable(proto: ProtoVariable): IrVariable {
        val (symbol, _) = symbolDeserializer.deserializeIrSymbolToDeclare(proto.base.symbol)

        if (symbol.isBound) return (symbol.owner as IrVariableImpl).apply {
            if (proto.hasInitializer()) initializer = bodyDeserializer.deserializeExpression(proto.initializer)
        }

        return super.deserializeIrVariable(proto)
    }

    override fun deserializeIrEnumEntry(proto: ProtoEnumEntry): IrEnumEntry {
        val (symbol, _) = symbolDeserializer.deserializeIrSymbolToDeclare(proto.base.symbol)

        if (symbol.isBound) return (symbol.owner as IrEnumEntry).apply {
            if (proto.hasCorrespondingClass()) correspondingClass = deserializeIrClass(proto.correspondingClass)
            if (proto.hasInitializer()) initializerExpression = deserializeExpressionBody(proto.initializer)
        }

        return super.deserializeIrEnumEntry(proto)
    }

    override fun deserializeIrAnonymousInit(proto: ProtoAnonymousInit): IrAnonymousInitializer {
        val (symbol, _) = symbolDeserializer.deserializeIrSymbolToDeclare(proto.base.symbol)

        if (symbol.isBound) {
            return symbol.owner.apply { deserializeStatementBody(proto.body) } as IrAnonymousInitializer
        }

        return super.deserializeIrAnonymousInit(proto)
    }

    override fun deserializeIrField(proto: ProtoField): IrField {
        val (symbol, _) = symbolDeserializer.deserializeIrSymbolToDeclare(proto.base.symbol)

        if (symbol.isBound) return (symbol.owner as IrField).apply {
            if (proto.hasInitializer()) initializer = deserializeExpressionBody(proto.initializer)
        }

        return super.deserializeIrField(proto)
    }

    override fun deserializeIrProperty(proto: ProtoProperty): IrProperty {
        val (symbol, _) = symbolDeserializer.deserializeIrSymbolToDeclare(proto.base.symbol)

        if (symbol.isBound) return (symbol.owner as IrProperty).apply {
            if (proto.hasGetter()) {
                getter = deserializeIrFunction(proto.getter).also {
                    it.correspondingPropertySymbol = symbol as IrPropertySymbol
                }
            }
            if (proto.hasSetter()) {
                setter = deserializeIrFunction(proto.setter).also {
                    it.correspondingPropertySymbol = symbol as IrPropertySymbol
                }
            }
            if (proto.hasBackingField()) {
                backingField = deserializeIrField(proto.backingField)
            }
        }

        return super.deserializeIrProperty(proto)
    }
}
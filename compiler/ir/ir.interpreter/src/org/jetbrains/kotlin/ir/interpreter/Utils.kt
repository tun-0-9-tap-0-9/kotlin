/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.ir.interpreter

import org.jetbrains.kotlin.builtins.PrimitiveType
import org.jetbrains.kotlin.builtins.StandardNames
import org.jetbrains.kotlin.builtins.UnsignedType
import org.jetbrains.kotlin.descriptors.ClassKind
import org.jetbrains.kotlin.ir.IrBuiltIns
import org.jetbrains.kotlin.ir.UNDEFINED_OFFSET
import org.jetbrains.kotlin.ir.declarations.*
import org.jetbrains.kotlin.ir.expressions.*
import org.jetbrains.kotlin.ir.expressions.impl.IrConstImpl
import org.jetbrains.kotlin.ir.expressions.impl.IrErrorExpressionImpl
import org.jetbrains.kotlin.ir.interpreter.proxy.Proxy
import org.jetbrains.kotlin.ir.interpreter.proxy.wrap
import org.jetbrains.kotlin.ir.interpreter.state.*
import org.jetbrains.kotlin.ir.interpreter.state.reflection.KTypeState
import org.jetbrains.kotlin.ir.symbols.*
import org.jetbrains.kotlin.ir.types.*
import org.jetbrains.kotlin.ir.types.impl.buildSimpleType
import org.jetbrains.kotlin.ir.types.impl.makeTypeProjection
import org.jetbrains.kotlin.ir.util.*
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.types.Variance
import org.jetbrains.kotlin.util.capitalizeDecapitalize.capitalizeAsciiOnly
import java.lang.invoke.MethodType

val compileTimeAnnotation = FqName("kotlin.CompileTimeCalculation")
val evaluateIntrinsicAnnotation = FqName("kotlin.EvaluateIntrinsic")
val contractsDslAnnotation = FqName("kotlin.internal.ContractsDsl")

internal fun IrFunction.getDispatchReceiver(): IrValueParameterSymbol? = this.dispatchReceiverParameter?.symbol

internal fun IrFunction.getExtensionReceiver(): IrValueParameterSymbol? = this.extensionReceiverParameter?.symbol

internal fun IrFunction.getReceiver(): IrSymbol? = this.getDispatchReceiver() ?: this.getExtensionReceiver()

internal fun IrFunctionAccessExpression.getThisReceiver(): IrValueSymbol = this.symbol.owner.parentAsClass.thisReceiver!!.symbol

internal fun State.toIrExpression(expression: IrExpression): IrExpression {
    val start = expression.startOffset
    val end = expression.endOffset
    val type = expression.type.makeNotNull()
    return when (this) {
        is Primitive<*> ->
            when {
                this.value == null -> this.value.toIrConst(type, start, end)
                type.isPrimitiveType() || type.isString() -> this.value.toIrConst(type, start, end)
                else -> expression // TODO support for arrays
            }
        is ExceptionState -> {
            IrErrorExpressionImpl(expression.startOffset, expression.endOffset, expression.type, "\n" + this.getFullDescription())
        }
        is Complex -> {
            val stateType = this.irClass.defaultType
            when {
                stateType.isUnsignedType() -> (this.fields.single().state as Primitive<*>).value.toIrConst(type, start, end)
                else -> expression
            }
        }
        else -> expression // TODO support
    }
}

/**
 * Convert object from outer world to state
 */
internal fun Any?.toState(irType: IrType): State {
    return when (this) {
        is Proxy -> this.state
        is State -> this
        is Boolean, is Char, is Byte, is Short, is Int, is Long, is String, is Float, is Double, is Array<*>, is ByteArray,
        is CharArray, is ShortArray, is IntArray, is LongArray, is FloatArray, is DoubleArray, is BooleanArray -> Primitive(this, irType)
        null -> Primitive(this, irType)
        else -> irType.classOrNull?.owner?.let { Wrapper(this, it) } ?: Wrapper(this)
    }
}

fun Any?.toIrConstOrNull(irType: IrType, startOffset: Int = UNDEFINED_OFFSET, endOffset: Int = UNDEFINED_OFFSET): IrConst<*>? {
    if (this == null) return IrConstImpl.constNull(startOffset, endOffset, irType)

    val constType = irType.makeNotNull()
    return when (irType.getPrimitiveType()) {
        PrimitiveType.BOOLEAN -> IrConstImpl.boolean(startOffset, endOffset, constType, this as Boolean)
        PrimitiveType.CHAR -> IrConstImpl.char(startOffset, endOffset, constType, this as Char)
        PrimitiveType.BYTE -> IrConstImpl.byte(startOffset, endOffset, constType, (this as Number).toByte())
        PrimitiveType.SHORT -> IrConstImpl.short(startOffset, endOffset, constType, (this as Number).toShort())
        PrimitiveType.INT -> IrConstImpl.int(startOffset, endOffset, constType, (this as Number).toInt())
        PrimitiveType.FLOAT -> IrConstImpl.float(startOffset, endOffset, constType, (this as Number).toFloat())
        PrimitiveType.LONG -> IrConstImpl.long(startOffset, endOffset, constType, (this as Number).toLong())
        PrimitiveType.DOUBLE -> IrConstImpl.double(startOffset, endOffset, constType, (this as Number).toDouble())
        null -> when (constType.getUnsignedType()) {
            UnsignedType.UBYTE -> IrConstImpl.byte(startOffset, endOffset, constType, (this as Number).toByte())
            UnsignedType.USHORT -> IrConstImpl.short(startOffset, endOffset, constType, (this as Number).toShort())
            UnsignedType.UINT -> IrConstImpl.int(startOffset, endOffset, constType, (this as Number).toInt())
            UnsignedType.ULONG -> IrConstImpl.long(startOffset, endOffset, constType, (this as Number).toLong())
            null -> when {
                constType.isString() -> IrConstImpl.string(startOffset, endOffset, constType, this as String)
                else -> null
            }
        }
    }
}

fun Any?.toIrConst(irType: IrType, startOffset: Int = UNDEFINED_OFFSET, endOffset: Int = UNDEFINED_OFFSET): IrConst<*> =
    toIrConstOrNull(irType, startOffset, endOffset)
        ?: throw UnsupportedOperationException("Unsupported const element type ${irType.makeNotNull().render()}")

fun Any?.toIrConst(
    irType: IrType, irBuiltIns: IrBuiltIns,
    startOffset: Int = UNDEFINED_OFFSET, endOffset: Int = UNDEFINED_OFFSET
): IrConst<*> =
    toIrConstOrNull(irType, startOffset, endOffset) ?: run {
        if (irType == irBuiltIns.stringType) IrConstImpl.string(startOffset, endOffset, irType.makeNotNull(), this as String)
        else throw UnsupportedOperationException("Unsupported const element type ${irType.makeNotNull().render()}")
    }

@Suppress("UNCHECKED_CAST")
internal fun <T> IrConst<T>.toPrimitive(): Primitive<T> = when {
    type.isByte() -> Primitive((value as Number).toByte() as T, type)
    type.isShort() -> Primitive((value as Number).toShort() as T, type)
    else -> Primitive(value, type)
}

fun IrAnnotationContainer?.hasAnnotation(annotation: FqName): Boolean {
    this ?: return false
    if (this.annotations.isNotEmpty()) {
        return this.annotations.any { it.symbol.owner.parentAsClass.fqNameWhenAvailable == annotation }
    }
    return false
}

fun IrAnnotationContainer.getAnnotation(annotation: FqName): IrConstructorCall {
    return this.annotations.firstOrNull { it.symbol.owner.parentAsClass.fqNameWhenAvailable == annotation }
        ?: ((this as IrFunction).parent as IrClass).annotations.first { it.symbol.owner.parentAsClass.fqNameWhenAvailable == annotation }
}

internal fun IrAnnotationContainer.getEvaluateIntrinsicValue(): String? {
    if (this is IrClass && this.fqNameWhenAvailable?.startsWith(Name.identifier("java")) == true) return this.fqNameWhenAvailable?.asString()
    if (!this.hasAnnotation(evaluateIntrinsicAnnotation)) return null
    return (this.getAnnotation(evaluateIntrinsicAnnotation).getValueArgument(0) as IrConst<*>).value.toString()
}

internal fun getPrimitiveClass(irType: IrType, asObject: Boolean = false): Class<*>? =
    when (irType.getPrimitiveType()) {
        PrimitiveType.BOOLEAN -> if (asObject) Boolean::class.javaObjectType else Boolean::class.java
        PrimitiveType.CHAR -> if (asObject) Char::class.javaObjectType else Char::class.java
        PrimitiveType.BYTE -> if (asObject) Byte::class.javaObjectType else Byte::class.java
        PrimitiveType.SHORT -> if (asObject) Short::class.javaObjectType else Short::class.java
        PrimitiveType.INT -> if (asObject) Int::class.javaObjectType else Int::class.java
        PrimitiveType.FLOAT -> if (asObject) Float::class.javaObjectType else Float::class.java
        PrimitiveType.LONG -> if (asObject) Long::class.javaObjectType else Long::class.java
        PrimitiveType.DOUBLE -> if (asObject) Double::class.javaObjectType else Double::class.java
        else -> when {
            irType.isString() -> String::class.java
            else -> null
        }
    }

fun IrFunction.getLastOverridden(): IrFunction {
    if (this !is IrSimpleFunction) return this

    return generateSequence(listOf(this)) { it.firstOrNull()?.overriddenSymbols?.map { it.owner } }.flatten().last()
}

internal fun List<Any?>.toPrimitiveStateArray(type: IrType): Primitive<*> {
    return when {
        type.isByteArray() -> Primitive(ByteArray(size) { i -> (this[i] as Number).toByte() }, type)
        type.isCharArray() -> Primitive(CharArray(size) { i -> this[i] as Char }, type)
        type.isShortArray() -> Primitive(ShortArray(size) { i -> (this[i] as Number).toShort() }, type)
        type.isIntArray() -> Primitive(IntArray(size) { i -> (this[i] as Number).toInt() }, type)
        type.isLongArray() -> Primitive(LongArray(size) { i -> (this[i] as Number).toLong() }, type)
        type.isFloatArray() -> Primitive(FloatArray(size) { i -> (this[i] as Number).toFloat() }, type)
        type.isDoubleArray() -> Primitive(DoubleArray(size) { i -> (this[i] as Number).toDouble() }, type)
        type.isBooleanArray() -> Primitive(BooleanArray(size) { i -> (this[i] as Boolean) }, type)
        else -> Primitive<Array<*>>(this.toTypedArray(), type)
    }
}

fun IrFunctionAccessExpression.getVarargType(index: Int): IrType? {
    val varargType = this.symbol.owner.valueParameters[index].varargElementType ?: return null
    varargType.classOrNull?.let { return this.symbol.owner.valueParameters[index].type }
    val type = this.symbol.owner.valueParameters[index].type as? IrSimpleType ?: return null
    return type.buildSimpleType {
        val typeParameter = varargType.classifierOrFail.owner as IrTypeParameter
        arguments = listOf(makeTypeProjection(this@getVarargType.getTypeArgument(typeParameter.index)!!, Variance.OUT_VARIANCE))
    }
}

internal fun IrFunction.getCapitalizedFileName() = this.file.name.replace(".kt", "Kt").capitalizeAsciiOnly()

internal fun IrType.isUnsigned() = this.getUnsignedType() != null
internal fun IrType.isFunction() = this.getClass()?.fqNameWhenAvailable?.asString()?.startsWith("kotlin.Function") ?: false
internal fun IrType.isKFunction() = this.getClass()?.fqNameWhenAvailable?.asString()?.startsWith("kotlin.reflect.KFunction") ?: false
internal fun IrType.isTypeParameter() = classifierOrNull is IrTypeParameterSymbol
internal fun IrType.isThrowable() = this.getClass()?.fqNameWhenAvailable?.asString() == "kotlin.Throwable"

internal fun IrType.isUnsignedArray(): Boolean {
    if (this !is IrSimpleType || classifier !is IrClassSymbol) return false
    val fqName = (classifier.owner as IrDeclarationWithName).fqNameWhenAvailable?.asString()
    return fqName in setOf("kotlin.UByteArray", "kotlin.UShortArray", "kotlin.UIntArray", "kotlin.ULongArray")
}

internal fun IrType.isPrimitiveArray(): Boolean {
    return this.getClass()?.fqNameWhenAvailable?.toUnsafe()?.let { StandardNames.isPrimitiveArray(it) } ?: false
}

internal fun IrClass.internalName(): String {
    val internalName = StringBuilder(this.name.asString())
    generateSequence(this as? IrDeclarationParent) { (it as? IrDeclaration)?.parent }
        .drop(1)
        .forEach {
            when (it) {
                is IrClass -> internalName.insert(0, it.name.asString() + "$")
                is IrPackageFragment -> it.fqName.asString().takeIf { it.isNotEmpty() }?.let { internalName.insert(0, "$it.") }
            }
        }
    return internalName.toString()
}

internal inline fun withExceptionHandler(environment: IrInterpreterEnvironment, block: () -> Unit) {
    try {
        block()
    } catch (e: Throwable) {
        e.handleUserException(environment)
    }
}

internal fun Throwable.handleUserException(environment: IrInterpreterEnvironment) {
    val exceptionName = this::class.java.simpleName
    val irExceptionClass = environment.irExceptions.firstOrNull { it.name.asString() == exceptionName }
        ?: environment.irBuiltIns.throwableClass.owner
    environment.callStack.pushState(ExceptionState(this, irExceptionClass, environment.callStack.getStackTrace()))
    environment.callStack.dropFramesUntilTryCatch()
}

/**
 * This method is analog of `checkcast` jvm bytecode operation. Throw exception whenever actual type is not a subtype of expected.
 */
internal fun IrFunction?.checkCast(environment: IrInterpreterEnvironment): Boolean {
    this ?: return true
    val actualType = this.returnType
    if (actualType.classifierOrNull !is IrTypeParameterSymbol) return true

    // TODO expectedType can be missing for functions called as proxy
    val expectedType = (environment.callStack.getState(this.symbol) as? KTypeState)?.irType ?: return true
    if (expectedType.classifierOrFail is IrTypeParameterSymbol) return true

    val actualState = environment.callStack.peekState() ?: return true
    if (actualState is Primitive<*> && actualState.value == null) return true // this is handled in checkNullability

    if (!actualState.isSubtypeOf(expectedType)) {
        val convertibleClassName = environment.callStack.popState().irClass.fqNameWhenAvailable
        ClassCastException("$convertibleClassName cannot be cast to ${expectedType.render()}").handleUserException(environment)
        return false
    }
    return true
}

internal fun IrFunction.getArgsForMethodInvocation(
    callInterceptor: CallInterceptor, methodType: MethodType, args: List<State>
): List<Any?> {
    val argsValues = args.wrap(callInterceptor, this, methodType).toMutableList()

    // TODO if vararg isn't last parameter
    // must convert vararg array into separated elements for correct invoke
    if (this.valueParameters.lastOrNull()?.varargElementType != null) {
        val varargValue = argsValues.last()
        argsValues.removeAt(argsValues.size - 1)
        argsValues.addAll(varargValue as Array<out Any?>)
    }

    return argsValues
}

internal fun IrType.getOnlyName(): String {
    if (this !is IrSimpleType) return this.render()
    return (this.classifierOrFail.owner as IrDeclarationWithName).name.asString() + (if (this.hasQuestionMark) "?" else "")
}

internal fun IrFieldAccessExpression.accessesTopLevelOrObjectField(): Boolean {
    return this.receiver == null || (this.receiver?.type?.classifierOrNull?.owner as? IrClass)?.isObject == true
}
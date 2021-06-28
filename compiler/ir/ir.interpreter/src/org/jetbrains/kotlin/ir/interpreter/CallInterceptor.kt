/*
 * Copyright 2010-2021 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.ir.interpreter

import org.jetbrains.kotlin.ir.IrElement
import org.jetbrains.kotlin.ir.UNDEFINED_OFFSET
import org.jetbrains.kotlin.ir.declarations.*
import org.jetbrains.kotlin.ir.IrBuiltIns
import org.jetbrains.kotlin.ir.expressions.*
import org.jetbrains.kotlin.ir.expressions.impl.IrCallImpl
import org.jetbrains.kotlin.ir.expressions.impl.IrConstructorCallImpl
import org.jetbrains.kotlin.ir.interpreter.builtins.interpretBinaryFunction
import org.jetbrains.kotlin.ir.interpreter.builtins.interpretTernaryFunction
import org.jetbrains.kotlin.ir.interpreter.builtins.interpretUnaryFunction
import org.jetbrains.kotlin.ir.interpreter.exceptions.InterpreterError
import org.jetbrains.kotlin.ir.interpreter.intrinsics.IntrinsicEvaluator
import org.jetbrains.kotlin.ir.interpreter.proxy.wrap
import org.jetbrains.kotlin.ir.interpreter.stack.CallStack
import org.jetbrains.kotlin.ir.interpreter.stack.Variable
import org.jetbrains.kotlin.ir.interpreter.state.*
import org.jetbrains.kotlin.ir.interpreter.state.reflection.KFunctionState
import org.jetbrains.kotlin.ir.interpreter.state.reflection.KTypeState
import org.jetbrains.kotlin.ir.interpreter.state.reflection.ReflectionState
import org.jetbrains.kotlin.ir.symbols.IrConstructorSymbol
import org.jetbrains.kotlin.ir.symbols.IrSimpleFunctionSymbol
import org.jetbrains.kotlin.ir.types.*
import org.jetbrains.kotlin.ir.util.*
import org.jetbrains.kotlin.ir.util.hasAnnotation
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name
import java.lang.invoke.MethodHandle

internal interface CallInterceptor {
    val environment: IrInterpreterEnvironment
    val irBuiltIns: IrBuiltIns
    val interpreter: IrInterpreter

    fun interceptProxy(irFunction: IrFunction, valueArguments: List<Variable>, expectedResultClass: Class<*> = Any::class.java): Any?
    fun interceptCall(call: IrCall, irFunction: IrFunction, receiver: State?, args: List<State>, defaultAction: () -> Unit)
    fun interceptConstructor(constructorCall: IrFunctionAccessExpression, receiver: State, args: List<State>, defaultAction: () -> Unit)
    fun interceptGetObjectValue(expression: IrGetObjectValue, defaultAction: () -> Unit)
    fun interceptEnumEntry(enumEntry: IrEnumEntry, defaultAction: () -> Unit)
    fun interceptJavaStaticField(expression: IrGetField)
}

internal class DefaultCallInterceptor(override val interpreter: IrInterpreter) : CallInterceptor {
    override val environment: IrInterpreterEnvironment = interpreter.environment
    private val callStack: CallStack = environment.callStack
    override val irBuiltIns: IrBuiltIns = environment.irBuiltIns
    private val bodyMap: Map<IdSignature, IrBody> = interpreter.bodyMap

    override fun interceptProxy(irFunction: IrFunction, valueArguments: List<Variable>, expectedResultClass: Class<*>): Any? {
        val irCall = IrCallImpl.fromSymbolOwner(
            UNDEFINED_OFFSET, UNDEFINED_OFFSET, irFunction.returnType, irFunction.symbol as IrSimpleFunctionSymbol
        )
        return interpreter.withNewCallStack(irCall) {
            this@withNewCallStack.environment.callStack.addInstruction(SimpleInstruction(irCall))
            valueArguments.forEach { this@withNewCallStack.environment.callStack.addVariable(it) }
        }.wrap(this@DefaultCallInterceptor, remainArraysAsIs = true, extendFrom = expectedResultClass)
    }

    override fun interceptCall(call: IrCall, irFunction: IrFunction, receiver: State?, args: List<State>, defaultAction: () -> Unit) {
        val isInlineOnly = irFunction.hasAnnotation(FqName("kotlin.internal.InlineOnly"))
        when {
            receiver is Wrapper && !isInlineOnly -> receiver.getMethod(irFunction).invokeMethod(irFunction, args)
            Wrapper.mustBeHandledWithWrapper(irFunction) -> Wrapper.getStaticMethod(irFunction).invokeMethod(irFunction, args)
            handleIntrinsicMethods(irFunction) -> return
            receiver is KFunctionState && call.symbol.owner.name.asString() == "invoke" -> handleInvoke(call, receiver, args)
            receiver is ReflectionState -> Wrapper.getReflectionMethod(irFunction).invokeMethod(irFunction, args)
            receiver is Primitive<*> -> calculateBuiltIns(irFunction, args) // check for js char, js long and get field for primitives
            irFunction.body == null ->
                irFunction.trySubstituteFunctionBody() ?: irFunction.tryCalculateLazyConst() ?: calculateBuiltIns(irFunction, args)
            else -> defaultAction()
        }
    }

    // TODO unite this logic with interpretCall
    private fun handleInvoke(call: IrCall, functionState: KFunctionState, args: List<State>) {
        val invokedFunction = functionState.irFunction

        var index = 1 // drop first arg that is reference to function
        val dispatchReceiver = invokedFunction.dispatchReceiverParameter?.let { functionState.getField(it.symbol) ?: args[index++] }
        val extensionReceiver = invokedFunction.extensionReceiverParameter?.let { functionState.getField(it.symbol) ?: args[index++] }
        val valueArguments = invokedFunction.valueParameters.map { args[index++] }

        val function = when (val symbol = invokedFunction.symbol) {
            is IrSimpleFunctionSymbol -> {
                val irCall = IrCallImpl.fromSymbolOwner(
                    UNDEFINED_OFFSET, UNDEFINED_OFFSET, invokedFunction.returnType, invokedFunction.symbol as IrSimpleFunctionSymbol
                )
                val actualFunction = dispatchReceiver?.getIrFunctionByIrCall(irCall) ?: invokedFunction
                callStack.newFrame(actualFunction)
                callStack.addInstruction(SimpleInstruction(actualFunction))
                callStack.addVariable(Variable(actualFunction.symbol, KTypeState(call.type, irBuiltIns.anyClass.owner)))

                actualFunction
            }
            is IrConstructorSymbol -> {
                val irConstructorCall = IrConstructorCallImpl.fromSymbolOwner(invokedFunction.returnType, symbol)
                callStack.newSubFrame(irConstructorCall)
                callStack.addInstruction(SimpleInstruction(irConstructorCall))
                callStack.addVariable(Variable(invokedFunction.parentAsClass.thisReceiver!!.symbol, Common(invokedFunction.parentAsClass)))

                invokedFunction
            }
            else -> TODO("unsupported symbol $symbol for invoke")
        }

        function.dispatchReceiverParameter?.let { callStack.addVariable(Variable(it.symbol, dispatchReceiver!!)) }
        function.extensionReceiverParameter?.let { callStack.addVariable(Variable(it.symbol, extensionReceiver!!)) }
        function.valueParameters.forEachIndexed { i, param -> callStack.addVariable(Variable(param.symbol, valueArguments[i])) }

        callStack.loadUpValues(functionState)
        if (extensionReceiver is StateWithClosure) callStack.loadUpValues(extensionReceiver)
        if (dispatchReceiver is Complex && function.parentClassOrNull?.isInner == true) {
            generateSequence(dispatchReceiver.outerClass) { (it.state as? Complex)?.outerClass }.forEach { callStack.addVariable(it) }
        }

        if (invokedFunction.symbol is IrConstructorSymbol) return
        this.interceptCall(call, function, dispatchReceiver, listOfNotNull(dispatchReceiver, extensionReceiver) + valueArguments) {
            callStack.addInstruction(CompoundInstruction(function))
        }
    }

    override fun interceptConstructor(
        constructorCall: IrFunctionAccessExpression, receiver: State, args: List<State>, defaultAction: () -> Unit
    ) {
        val irConstructor = constructorCall.symbol.owner
        val irClass = irConstructor.parentAsClass
        when {
            Wrapper.mustBeHandledWithWrapper(irClass) || irClass.fqNameWhenAvailable!!.startsWith(Name.identifier("java")) -> {
                Wrapper.getConstructorMethod(irConstructor).invokeMethod(irConstructor, args)
                if (constructorCall !is IrConstructorCall) (receiver as Common).superWrapperClass = callStack.popState() as Wrapper
            }
            irClass.defaultType.isArray() || irClass.defaultType.isPrimitiveArray() -> {
                // array constructor doesn't have body so must be treated separately
                callStack.addVariable(Variable(irConstructor.symbol, KTypeState(constructorCall.type, irBuiltIns.anyClass.owner)))
                assert(handleIntrinsicMethods(irConstructor)) { "Unsupported intrinsic constructor: ${irConstructor.render()}" }
            }
            irClass.defaultType.isUnsignedType() -> {
                val propertySymbol = irClass.declarations.single { it is IrProperty }.symbol
                callStack.pushState(receiver.apply { fields += Variable(propertySymbol, args.single()) })
            }
            else -> defaultAction()
        }
    }

    override fun interceptGetObjectValue(expression: IrGetObjectValue, defaultAction: () -> Unit) {
        val objectClass = expression.symbol.owner
        when {
            Wrapper.mustBeHandledWithWrapper(objectClass) -> {
                val result = Wrapper.getCompanionObject(objectClass)
                environment.mapOfObjects[expression.symbol] = result
                callStack.pushState(result)
            }
            else -> defaultAction()
        }
    }

    override fun interceptEnumEntry(enumEntry: IrEnumEntry, defaultAction: () -> Unit) {
        val enumClass = enumEntry.symbol.owner.parentAsClass
        when {
            Wrapper.mustBeHandledWithWrapper(enumClass) -> {
                val enumEntryName = enumEntry.name.asString().toState(environment.irBuiltIns.stringType)
                val valueOfFun = enumClass.declarations.single { it.nameForIrSerialization.asString() == "valueOf" } as IrFunction
                Wrapper.getEnumEntry(enumClass).invokeMethod(valueOfFun, listOf(enumEntryName))
                environment.mapOfEnums[enumEntry.symbol] = callStack.popState() as Complex
            }
            else -> defaultAction()
        }
    }

    override fun interceptJavaStaticField(expression: IrGetField) {
        val field = expression.symbol.owner
        assert(field.origin == IrDeclarationOrigin.IR_EXTERNAL_JAVA_DECLARATION_STUB && field.isStatic)
        assert(field.initializer?.expression !is IrConst<*>)
        callStack.pushState(Wrapper.getStaticGetter(field).invokeWithArguments().toState(field.type))
    }

    private fun MethodHandle?.invokeMethod(irFunction: IrFunction, args: List<State>) {
        this ?: return assert(handleIntrinsicMethods(irFunction)) { "Unsupported intrinsic function: ${irFunction.render()}" }
        val argsForMethodInvocation = irFunction.getArgsForMethodInvocation(this@DefaultCallInterceptor, this.type(), args)
        withExceptionHandler(environment) {
            val result = this.invokeWithArguments(argsForMethodInvocation) // TODO if null return Unit
            callStack.pushState(result.toState(result.getType(irFunction.returnType)))
        }
    }

    private fun handleIntrinsicMethods(irFunction: IrFunction): Boolean {
        val instructions = IntrinsicEvaluator.unwindInstructions(irFunction, environment) ?: return false
        instructions.forEach { callStack.addInstruction(it) }
        return true
    }

    private fun calculateBuiltIns(irFunction: IrFunction, args: List<State>) {
        val methodName = when (val property = (irFunction as? IrSimpleFunction)?.correspondingPropertySymbol) {
            null -> irFunction.name.asString()
            else -> property.owner.name.asString()
        }

        val receiverType = irFunction.dispatchReceiverParameter?.type ?: irFunction.extensionReceiverParameter?.type
        val argsType = listOfNotNull(receiverType) + irFunction.valueParameters.map { it.type }
        val argsValues = args.wrap(this, irFunction)

        // TODO replace unary, binary, ternary functions with vararg
        withExceptionHandler(environment) {
            val result = when (argsType.size) {
                1 -> interpretUnaryFunction(methodName, argsType[0].getOnlyName(), argsValues[0])
                2 -> when (methodName) {
                    "rangeTo" -> return calculateRangeTo(irFunction.returnType, args)
                    else -> interpretBinaryFunction(
                        methodName, argsType[0].getOnlyName(), argsType[1].getOnlyName(), argsValues[0], argsValues[1]
                    )
                }
                3 -> interpretTernaryFunction(
                    methodName, argsType[0].getOnlyName(), argsType[1].getOnlyName(), argsType[2].getOnlyName(),
                    argsValues[0], argsValues[1], argsValues[2]
                )
                else -> throw InterpreterError("Unsupported number of arguments for invocation as builtin function: $methodName")
            }
            // TODO check "result is Unit"
            callStack.pushState(result.toState(result.getType(irFunction.returnType)))
        }
    }

    private fun calculateRangeTo(type: IrType, args: List<State>) {
        val constructor = type.classOrNull!!.owner.constructors.first()
        val constructorCall = IrConstructorCallImpl.fromSymbolOwner(constructor.returnType, constructor.symbol)
        val constructorValueParameters = constructor.valueParameters.map { it.symbol }

        val primitiveValueParameters = args.map { it as Primitive<*> }
        primitiveValueParameters.forEachIndexed { index, primitive ->
            constructorCall.putValueArgument(index, primitive.value.toIrConst(constructorValueParameters[index].owner.type))
        }

        callStack.addInstruction(CompoundInstruction(constructorCall))
    }

    private fun Any?.getType(defaultType: IrType): IrType {
        return when (this) {
            is Boolean -> irBuiltIns.booleanType
            is Char -> irBuiltIns.charType
            is Byte -> irBuiltIns.byteType
            is Short -> irBuiltIns.shortType
            is Int -> irBuiltIns.intType
            is Long -> irBuiltIns.longType
            is String -> irBuiltIns.stringType
            is Float -> irBuiltIns.floatType
            is Double -> irBuiltIns.doubleType
            null -> irBuiltIns.nothingNType
            else -> defaultType
        }
    }

    private fun IrFunction.trySubstituteFunctionBody(): IrElement? {
        val signature = this.symbol.signature ?: return null
        this.body = bodyMap[signature] ?: return null
        callStack.addInstruction(CompoundInstruction(this))
        return body
    }

    // TODO fix in FIR2IR; const val getter must have body with IrGetField node
    private fun IrFunction.tryCalculateLazyConst(): IrExpression? {
        if (this !is IrSimpleFunction) return null
        val expression = this.correspondingPropertySymbol?.owner?.backingField?.initializer?.expression
        return expression?.apply { callStack.addInstruction(CompoundInstruction(this)) }
    }
}
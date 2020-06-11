/*
 * Copyright 2010-2019 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.backend.common.lower

import org.jetbrains.kotlin.backend.common.CommonBackendContext
import org.jetbrains.kotlin.backend.common.FileLoweringPass
import org.jetbrains.kotlin.ir.interpreter.IrCompileTimeChecker
import org.jetbrains.kotlin.ir.interpreter.*
import org.jetbrains.kotlin.config.CommonConfigurationKeys
import org.jetbrains.kotlin.config.LanguageFeature
import org.jetbrains.kotlin.config.languageVersionSettings
import org.jetbrains.kotlin.ir.IrStatement
import org.jetbrains.kotlin.ir.declarations.*
import org.jetbrains.kotlin.ir.expressions.*
import org.jetbrains.kotlin.ir.util.IdSignature
import org.jetbrains.kotlin.ir.visitors.IrElementTransformerVoid

class CompileTimeCalculationLowering(val context: CommonBackendContext) : FileLoweringPass {
    @Suppress("UNCHECKED_CAST")
    private val bodyMap = context.configuration[CommonConfigurationKeys.IR_BODY_MAP] as? Map<IdSignature, IrBody> ?: emptyMap()
    private val isTest = context.configuration[CommonConfigurationKeys.MODULE_NAME] == "<test-module>"
    private val interpreter = IrInterpreter(context.ir.irModule.irBuiltins, bodyMap)

    override fun lower(irFile: IrFile) {
        if (!context.configuration.languageVersionSettings.supportsFeature(LanguageFeature.CompileTimeCalculations)) return
        if (irFile.fileEntry.name.contains("/kotlin/libraries/")) return
        irFile.transformChildren(Transformer(irFile), null)
    }

    private inner class Transformer(private val irFile: IrFile) : IrElementTransformerVoid() {
        private fun IrExpression.report(original: IrExpression): IrExpression {
            if (this == original) return this
            val isError = this is IrErrorExpression && isTest
            val message = when (this) {
                is IrConst<*> -> this.value.toString()
                is IrErrorExpression -> this.description
                else -> TODO("unsupported type ${this::class.java}")
            }
            context.report(original, irFile, message, isError)
            return if (this !is IrErrorExpression) this else original
        }

        override fun visitCall(expression: IrCall): IrExpression {
            if (expression.accept(IrCompileTimeChecker(), null)) {
                return interpreter.interpret(expression).report(expression)
            }
            return super.visitCall(expression)
        }

        override fun visitField(declaration: IrField): IrStatement {
            val initializer = declaration.initializer
            val expression = initializer?.expression ?: return declaration
            if (expression is IrConst<*>) return declaration
            // must pass declaration symbol as initial containing declaration because of complex constructions such as try block or safe call
            val isCompileTimeComputable = expression.accept(IrCompileTimeChecker(declaration), null)
            val isConst = declaration.correspondingPropertySymbol?.owner?.isConst == true
            if (isConst && !isCompileTimeComputable) {
                context.report(expression, irFile, "Const property is used only with functions annotated as CompileTimeCalculation", true)
            } else if (isCompileTimeComputable) {
                initializer.expression = interpreter.interpret(expression).report(expression)
            }
            return declaration
        }

        //todo annotation call
    }

}

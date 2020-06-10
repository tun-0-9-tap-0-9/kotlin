/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.resolve.calls.checkers

import com.intellij.psi.PsiElement
import org.jetbrains.kotlin.builtins.functions.FunctionInvokeDescriptor
import org.jetbrains.kotlin.config.LanguageFeature
import org.jetbrains.kotlin.descriptors.*
import org.jetbrains.kotlin.descriptors.impl.AnonymousFunctionDescriptor
import org.jetbrains.kotlin.descriptors.impl.TypeAliasConstructorDescriptor
import org.jetbrains.kotlin.diagnostics.Errors
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.psi.KtAnnotationEntry
import org.jetbrains.kotlin.resolve.calls.model.ResolvedCall
import org.jetbrains.kotlin.resolve.descriptorUtil.isAnnotationConstructor
import org.jetbrains.kotlin.resolve.scopes.LexicalScope
import org.jetbrains.kotlin.utils.addToStdlib.safeAs

object ConstexprCallChecker : CallChecker {
    private val compileTimeAnnotationName = FqName("kotlin.CompileTimeCalculation")

    override fun check(resolvedCall: ResolvedCall<*>, reportOn: PsiElement, context: CallCheckerContext) {
        if (!context.languageVersionSettings.supportsFeature(LanguageFeature.CompileTimeCalculations)) return
        if (resolvedCall.call.callElement is KtAnnotationEntry || resolvedCall.candidateDescriptor.isAnnotationConstructor()) return
        if (hasEnclosingIntrinsicDeclaration(context)) return

        val isConst = (context.scope.ownerDescriptor as? PropertyDescriptor)?.isConst == true
        val isInsideCompileTimeFun = hasEnclosingConstDeclaration(context)
        if (!isConst && !isInsideCompileTimeFun) return

        val isCompileTime = isCompileTime(resolvedCall, context.scope)
        if (isConst && !isCompileTime) {
            context.trace.report(Errors.CONST_VAL_WITH_NON_CONST_INITIALIZER.on(resolvedCall.call.calleeExpression!!))
        } else if (isInsideCompileTimeFun && !isCompileTime) {
            context.trace.report(Errors.NON_COMPILE_TIME_EXPRESSION_IN_COMPILE_TIME_DECLARATION.on(resolvedCall.call.calleeExpression!!))
        }
    }

    private fun isCompileTime(resolvedCall: ResolvedCall<*>, scope: LexicalScope): Boolean {
        return when (val descriptor = resolvedCall.resultingDescriptor) {
            is TypeAliasConstructorDescriptor -> descriptor.typeAliasDescriptor.isCompileTime()
            is PropertyAccessorDescriptor ->
                descriptor.correspondingProperty.isCompileTime() && (descriptor.isCompileTime() || descriptor.isDefault)
            is FunctionDescriptor ->
                descriptor.isCompileTime() || descriptor.isSpecial() || descriptor.overriddenDescriptors.any { it.isCompileTime() }
            is PropertyDescriptor -> descriptor.isCompileTime() || descriptor.isConst
            is ValueParameterDescriptor, is ReceiverParameterDescriptor, is VariableDescriptor ->
                scope.ownerDescriptor.isCompileTime()
            else -> false
        }
    }

    private fun DeclarationDescriptor.isMarkedWith(annotation: FqName): Boolean {
        if (this.annotations.hasAnnotation(annotation)) return true
        if (this is FunctionInvokeDescriptor) return true
        if (this is ClassDescriptor && this.isCompanionObject) return false
        return (this.containingDeclaration as? ClassDescriptor)?.isMarkedWith(annotation) ?: false
    }

    private fun DeclarationDescriptor.isCompileTime(): Boolean {
        if (this is AnonymousFunctionDescriptor) return this.containingDeclaration.isCompileTime()
        return this.isMarkedWith(compileTimeAnnotationName) || this.safeAs<PropertyDescriptor>()?.isConst == true
    }

    private fun hasEnclosingConstDeclaration(context: CallCheckerContext): Boolean {
        return context.scope.ownerDescriptor.isCompileTime()
    }

    private fun hasEnclosingIntrinsicDeclaration(context: CallCheckerContext): Boolean {
        return context.scope.ownerDescriptor.isMarkedWith(FqName("kotlin.EvaluateIntrinsic"))
    }

    // TODO add annotation to special functions?
    private fun CallableDescriptor.isSpecial(): Boolean {
        return this.name.asString().contains("SPECIAL-FUNCTION")
    }
}
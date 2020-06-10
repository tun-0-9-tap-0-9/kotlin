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
import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.psi.KtAnnotationEntry
import org.jetbrains.kotlin.psi.KtParameter
import org.jetbrains.kotlin.psi.KtPrimaryConstructor
import org.jetbrains.kotlin.resolve.BindingContext
import org.jetbrains.kotlin.resolve.calls.model.ResolvedCall
import org.jetbrains.kotlin.resolve.descriptorUtil.fqNameSafe
import org.jetbrains.kotlin.resolve.descriptorUtil.isAnnotationConstructor
import org.jetbrains.kotlin.resolve.findTopMostOverriddenDescriptors
import org.jetbrains.kotlin.resolve.scopes.receivers.AbstractReceiverValue
import org.jetbrains.kotlin.resolve.source.getPsi
import org.jetbrains.kotlin.types.AbbreviatedType
import org.jetbrains.kotlin.utils.addToStdlib.safeAs

object ConstexprCallChecker : CallChecker {
    private val compileTimeAnnotationName = FqName("kotlin.CompileTimeCalculation")
    private val compileTimeTypeAliases = setOf(
        "java.lang.StringBuilder", "java.lang.IllegalArgumentException", "java.util.NoSuchElementException"
    )

    override fun check(resolvedCall: ResolvedCall<*>, reportOn: PsiElement, context: CallCheckerContext) {
        if (!context.languageVersionSettings.supportsFeature(LanguageFeature.CompileTimeCalculations)) return
        if (resolvedCall.call.callElement is KtAnnotationEntry || resolvedCall.candidateDescriptor.isAnnotationConstructor()) return
        if (hasEnclosingIntrinsicDeclaration(context)) return

        val isConst = (context.scope.ownerDescriptor as? PropertyDescriptor)?.isConst == true
        val isInsideCompileTimeFun = hasEnclosingConstDeclaration(context)
        if (!isConst && !isInsideCompileTimeFun) return

        val isCompileTime = isCompileTime(resolvedCall.resultingDescriptor, context) || isCompileTimeTypeAlias(resolvedCall)
        if (isConst && !isCompileTime) {
            context.trace.report(Errors.CONST_VAL_WITH_NON_CONST_INITIALIZER.on(resolvedCall.call.calleeExpression!!))
        } else if (isInsideCompileTimeFun && !isCompileTime) {
            context.trace.report(Errors.NON_COMPILE_TIME_EXPRESSION_IN_COMPILE_TIME_DECLARATION.on(resolvedCall.call.calleeExpression!!))
        }
    }

    private fun isCompileTimeTypeAlias(resolvedCall: ResolvedCall<*>): Boolean {
        if (!resolvedCall.resultingDescriptor.containingDeclaration.fqNameSafe.startsWith(Name.identifier("java"))) return false
        val type = (resolvedCall.dispatchReceiver as AbstractReceiverValue).type
        if (type !is AbbreviatedType) return false

        return type.abbreviation.constructor.declarationDescriptor?.isMarkedAsCompileTime() ?: false
    }

    private fun isCompileTime(descriptor: CallableDescriptor, context: CallCheckerContext): Boolean {
        return when (descriptor) {
            is TypeAliasConstructorDescriptor -> descriptor.typeAliasDescriptor.isMarkedAsCompileTime()
            is PropertyAccessorDescriptor ->
                isCompileTime(descriptor.correspondingProperty, context) && (descriptor.isMarkedAsCompileTime() || descriptor.isDefault)
            is FunctionDescriptor ->
                descriptor.isMarkedAsCompileTime() || descriptor.isSpecial() || descriptor.overriddenDescriptors.any { it.isMarkedAsCompileTime() }
            is PropertyDescriptor ->
                descriptor.isMarkedAsCompileTime() || descriptor.isConst || descriptor.hasCompileTimePrimaryConstructor(context.trace.bindingContext)
            is ValueParameterDescriptor, is ReceiverParameterDescriptor, is VariableDescriptor ->
                context.scope.ownerDescriptor.isMarkedAsCompileTime()
            else -> false
        }
    }

    private fun DeclarationDescriptor.isMarkedWith(annotation: FqName): Boolean {
        if (this.annotations.hasAnnotation(annotation)) return true
        if (this is FunctionInvokeDescriptor) return true
        if (this is ClassDescriptor && this.fqNameSafe.asString() in compileTimeTypeAliases) return true
        if (this is ClassDescriptor && this.isCompanionObject) return false
        if (this is AnonymousFunctionDescriptor) return this.containingDeclaration.isMarkedWith(annotation)
        return (this.containingDeclaration as? ClassDescriptor)?.isMarkedWith(annotation) ?: false
    }

    private fun DeclarationDescriptor.isMarkedAsCompileTime(): Boolean {
        return this.isMarkedWith(compileTimeAnnotationName) || this.safeAs<PropertyDescriptor>()?.isConst == true
    }

    private fun PropertyDescriptor.hasCompileTimePrimaryConstructor(bindingContext: BindingContext): Boolean {
        val property = this.findTopMostOverriddenDescriptors().first()
        val ktParameter = property.source.getPsi() as? KtParameter ?: return false
        val primaryConstructor = ktParameter.ownerFunction as? KtPrimaryConstructor ?: return false
        val classOwner = primaryConstructor.getContainingClassOrObject()
        val annotations = primaryConstructor.annotationEntries.mapNotNull { bindingContext[BindingContext.ANNOTATION, it] } +
                classOwner.annotationEntries.mapNotNull { bindingContext[BindingContext.ANNOTATION, it] }
        return annotations.any { it.fqName == compileTimeAnnotationName }
    }

    private fun hasEnclosingConstDeclaration(context: CallCheckerContext): Boolean {
        return context.scope.ownerDescriptor.isMarkedAsCompileTime()
    }

    private fun hasEnclosingIntrinsicDeclaration(context: CallCheckerContext): Boolean {
        return context.scope.ownerDescriptor.isMarkedWith(FqName("kotlin.EvaluateIntrinsic"))
    }

    // TODO add annotation to special functions?
    private fun CallableDescriptor.isSpecial(): Boolean {
        return this.name.asString().contains("SPECIAL-FUNCTION")
    }
}
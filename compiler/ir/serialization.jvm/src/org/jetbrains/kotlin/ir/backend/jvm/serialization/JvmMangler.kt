/*
 * Copyright 2010-2019 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.ir.backend.jvm.serialization

import org.jetbrains.kotlin.backend.common.serialization.mangle.KotlinExportChecker
import org.jetbrains.kotlin.backend.common.serialization.mangle.KotlinMangleComputer
import org.jetbrains.kotlin.backend.common.serialization.mangle.MangleConstant
import org.jetbrains.kotlin.backend.common.serialization.mangle.MangleMode
import org.jetbrains.kotlin.backend.common.serialization.mangle.descriptor.DescriptorBasedKotlinManglerImpl
import org.jetbrains.kotlin.backend.common.serialization.mangle.descriptor.DescriptorExportCheckerVisitor
import org.jetbrains.kotlin.backend.common.serialization.mangle.descriptor.DescriptorMangleComputer
import org.jetbrains.kotlin.backend.common.serialization.mangle.ir.IrBasedKotlinManglerImpl
import org.jetbrains.kotlin.backend.common.serialization.mangle.ir.IrExportCheckerVisitor
import org.jetbrains.kotlin.backend.common.serialization.mangle.ir.IrMangleComputer
import org.jetbrains.kotlin.descriptors.DeclarationDescriptor
import org.jetbrains.kotlin.descriptors.FunctionDescriptor
import org.jetbrains.kotlin.descriptors.PropertyDescriptor
import org.jetbrains.kotlin.idea.MainFunctionDetector
import org.jetbrains.kotlin.ir.declarations.*
import org.jetbrains.kotlin.ir.util.fqNameWhenAvailable
import org.jetbrains.kotlin.ir.util.isFromJava
import org.jetbrains.kotlin.load.java.descriptors.JavaMethodDescriptor
import org.jetbrains.kotlin.load.java.lazy.descriptors.isJavaField

abstract class AbstractJvmManglerIr : IrBasedKotlinManglerImpl() {

    companion object {
        private val exportChecker = JvmIrExportChecker()
    }

    private class JvmIrExportChecker : IrExportCheckerVisitor() {
        override fun IrDeclaration.isPlatformSpecificExported(): Boolean {
            // We need to handle references to file-level declarations from classes declared in those files.
            // Thus we give them all public signatures
            return (parent as? IrDeclaration)?.origin?.let {
                it == IrDeclarationOrigin.FILE_CLASS || it == IrDeclarationOrigin.JVM_MULTIFILE_CLASS
            } == true ||
                    // We also need public signatures for monitorEnter/monitorExit
                    (this as? IrSimpleFunction)?.let {
                        name.asString().let { it == "monitorEnter" || it == "monitorExit" } &&
                                fqNameWhenAvailable?.toString()?.startsWith("kotlin.jvm.internal.unsafe") == true
                    } == true
        }

        override fun visitField(declaration: IrField, data: Nothing?): Boolean {
            if (declaration.origin == IrDeclarationOrigin.IR_EXTERNAL_JAVA_DECLARATION_STUB) return true
            return super.visitField(declaration, data)
        }
    }

    private class JvmIrManglerComputer(builder: StringBuilder, mode: MangleMode) : IrMangleComputer(builder, mode) {
        override fun copy(newMode: MangleMode): IrMangleComputer =
            JvmIrManglerComputer(builder, newMode)

        override fun addReturnTypeSpecialCase(irFunction: IrFunction): Boolean =
            irFunction.isFromJava()
    }

    override fun getExportChecker(): KotlinExportChecker<IrDeclaration> = exportChecker

    override fun getMangleComputer(mode: MangleMode): KotlinMangleComputer<IrDeclaration> {
        return JvmIrManglerComputer(StringBuilder(256), mode)
    }
}

object JvmManglerIr : AbstractJvmManglerIr()

abstract class AbstractJvmDescriptorMangler(private val mainDetector: MainFunctionDetector?) : DescriptorBasedKotlinManglerImpl() {

    companion object {
        private val exportChecker = JvmDescriptorExportChecker()
    }

    private class JvmDescriptorExportChecker : DescriptorExportCheckerVisitor() {
        override fun DeclarationDescriptor.isPlatformSpecificExported() = false
    }

    private class JvmDescriptorManglerComputer(
        builder: StringBuilder,
        private val mainDetector: MainFunctionDetector?,
        mode: MangleMode
    ) : DescriptorMangleComputer(builder, mode) {
        override fun addReturnTypeSpecialCase(functionDescriptor: FunctionDescriptor): Boolean =
            functionDescriptor is JavaMethodDescriptor

        override fun copy(newMode: MangleMode): DescriptorMangleComputer =
            JvmDescriptorManglerComputer(builder, mainDetector, newMode)

        private fun isMainFunction(descriptor: FunctionDescriptor): Boolean = mainDetector?.isMain(descriptor) ?: false

        override fun FunctionDescriptor.platformSpecificSuffix(): String? {
            return if (isMainFunction(this)) {
                return source.containingFile.name
            } else null
        }

        override fun PropertyDescriptor.platformSpecificSuffix(): String? {
            // Since LV 1.4 there is a feature PreferJavaFieldOverload which allows to have java and kotlin
            // properties with the same signature on the same level.
            // For more details see JvmPlatformOverloadsSpecificityComparator.kt
            return if (isJavaField) MangleConstant.JAVA_FIELD_SUFFIX else null
        }
    }

    override fun getExportChecker(): KotlinExportChecker<DeclarationDescriptor> = exportChecker

    override fun getMangleComputer(mode: MangleMode): KotlinMangleComputer<DeclarationDescriptor> {
        return JvmDescriptorManglerComputer(StringBuilder(256), mainDetector, mode)
    }
}

class JvmManglerDesc(mainDetector: MainFunctionDetector? = null) : AbstractJvmDescriptorMangler(mainDetector)
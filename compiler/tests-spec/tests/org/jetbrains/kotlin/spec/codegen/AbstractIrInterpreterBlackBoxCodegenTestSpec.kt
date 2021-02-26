/*
 * Copyright 2010-2021 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.spec.codegen

import com.intellij.openapi.util.io.FileUtil
import org.jetbrains.kotlin.spec.utils.parsers.CommonPatterns.packagePattern
import org.jetbrains.kotlin.test.Constructor
import org.jetbrains.kotlin.test.builders.TestConfigurationBuilder
import org.jetbrains.kotlin.test.directives.AdditionalFilesDirectives
import org.jetbrains.kotlin.test.directives.JvmEnvironmentConfigurationDirectives
import org.jetbrains.kotlin.test.directives.model.DirectivesContainer
import org.jetbrains.kotlin.test.directives.model.RegisteredDirectives
import org.jetbrains.kotlin.test.model.BackendInputHandler
import org.jetbrains.kotlin.test.model.TestFile
import org.jetbrains.kotlin.test.model.TestModule
import org.jetbrains.kotlin.test.runners.ir.interpreter.AbstractIrInterpreterBlackBoxTest
import org.jetbrains.kotlin.test.runners.ir.interpreter.IrInterpreterBoxHandler
import org.jetbrains.kotlin.test.services.AdditionalSourceProvider
import org.jetbrains.kotlin.test.services.TestServices
import org.jetbrains.kotlin.test.services.moduleStructure
import org.junit.jupiter.api.Assumptions
import java.io.File

open class AbstractIrInterpreterBlackBoxCodegenTestSpec : AbstractIrInterpreterBlackBoxTest() {
    override val handler: Constructor<BackendInputHandler<*>>
        get() = { IrInterpreterSpecBoxHandler(it) }

    override fun configure(builder: TestConfigurationBuilder) {
        super.configure(builder)
        with(builder) {
            defaultDirectives {
                +AdditionalFilesDirectives.SPEC_HELPERS
                +JvmEnvironmentConfigurationDirectives.WITH_REFLECT
            }

            useAdditionalSourceProviders(::InterpreterSpecHelpersSourceFilesProvider)
        }
    }
}

class IrInterpreterSpecBoxHandler(testServices: TestServices) : IrInterpreterBoxHandler(testServices) {
    override fun additionalIgnores(modules: List<TestModule>) {}
}

class InterpreterSpecHelpersSourceFilesProvider(testServices: TestServices) : AdditionalSourceProvider(testServices) {
    companion object {
        private const val HELPERS_DIR_PATH = "compiler/tests-spec/testData/codegen/box/helpers"
        private const val HELPERS_PACKAGE_VARIABLE = "<!PACKAGE!>"
    }

    override val directives: List<DirectivesContainer> =
        listOf(AdditionalFilesDirectives)

    private fun addPackageDirectiveToHelperFile(helperContent: String, packageName: String?) =
        helperContent.replace(HELPERS_PACKAGE_VARIABLE, if (packageName == null) "" else "package $packageName")

    override fun produceAdditionalFiles(globalDirectives: RegisteredDirectives, module: TestModule): List<TestFile> {
        if (AdditionalFilesDirectives.SPEC_HELPERS !in module.directives) return emptyList()

        val fileContent = module.files.first().originalContent
        val packageName = packagePattern.matcher(fileContent).let {
            if (it.find()) it.group("packageName") else null
        }

        return File(HELPERS_DIR_PATH).walkTopDown().mapNotNull {
            if (it.isDirectory) return@mapNotNull null
            val helperContent = FileUtil.loadFile(it, true)
            TestFile(
                it.name,
                addPackageDirectiveToHelperFile(helperContent, packageName),
                it,
                startLineNumberInOriginalFile = 0,
                isAdditional = true,
                directives = RegisteredDirectives.Empty
            )
        }.toList()
    }
}
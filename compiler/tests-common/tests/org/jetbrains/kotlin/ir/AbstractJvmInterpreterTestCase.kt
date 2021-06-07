/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.ir

import org.jetbrains.kotlin.cli.common.CLIConfigurationKeys
import org.jetbrains.kotlin.cli.jvm.compiler.EnvironmentConfigFiles
import org.jetbrains.kotlin.cli.jvm.compiler.KotlinCoreEnvironment
import org.jetbrains.kotlin.codegen.CodegenTestCase
import org.jetbrains.kotlin.codegen.GenerationUtils
import org.jetbrains.kotlin.config.CommonConfigurationKeys
import org.jetbrains.kotlin.config.JVMConfigurationKeys
import org.jetbrains.kotlin.diagnostics.DiagnosticWithParameters1
import org.jetbrains.kotlin.test.ConfigurationKind
import org.jetbrains.kotlin.test.KotlinTestUtils
import org.jetbrains.kotlin.test.TargetBackend
import org.jetbrains.kotlin.test.TestJdkKind
import java.io.File

abstract class AbstractJvmInterpreterTestCase : CodegenTestCase() {

    private val fullRuntimeKlib = "build/js-ir-runtime/klib"
    private val messageCollector = AbstractJsInterpreterTestCase.TestMessageCollector()

    override val backend: TargetBackend = TargetBackend.JVM_IR

    override fun doMultiFileTest(wholeFile: File, files: List<TestFile>) {
        setupEnvironment(files)
        loadMultiFiles(files)

        val state = GenerationUtils.compileFiles(myFiles.psiFiles, myEnvironment)
        val diagnostics = state.collectedExtraJvmDiagnostics.all()
        val actual = diagnostics.joinToString(separator = "\n\n") { (it as DiagnosticWithParameters1<*, *>).a.toString() }

        val expectedPath = wholeFile.absolutePath.replace(".kt", ".txt")
        KotlinTestUtils.assertEqualsToFile(File(expectedPath), actual)
    }

    private fun setupEnvironment(testFiles: List<TestFile>) {
        val configuration = createConfiguration(
            ConfigurationKind.ALL, TestJdkKind.FULL_JDK, TargetBackend.JVM_IR, listOf(), listOfNotNull(writeJavaFiles(testFiles)), testFiles
        )

        configuration.put(CommonConfigurationKeys.MODULE_NAME, "<test-module>")
        configuration.put(CLIConfigurationKeys.MESSAGE_COLLECTOR_KEY, messageCollector)
        configuration.put(JVMConfigurationKeys.KLIB_PATH_FOR_COMPILE_TIME, fullRuntimeKlib)

        myEnvironment = KotlinCoreEnvironment.createForTests(testRootDisposable, configuration, EnvironmentConfigFiles.JVM_CONFIG_FILES)
    }

}
/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.ir

import org.jetbrains.kotlin.cli.common.CLIConfigurationKeys
import org.jetbrains.kotlin.cli.common.arguments.K2JSCompilerArguments
import org.jetbrains.kotlin.cli.common.createPhaseConfig
import org.jetbrains.kotlin.cli.common.messages.*
import org.jetbrains.kotlin.cli.js.messageCollectorLogger
import org.jetbrains.kotlin.cli.jvm.compiler.EnvironmentConfigFiles
import org.jetbrains.kotlin.cli.jvm.compiler.KotlinCoreEnvironment
import org.jetbrains.kotlin.config.CommonConfigurationKeys
import org.jetbrains.kotlin.ir.backend.js.MainModule
import org.jetbrains.kotlin.ir.backend.js.compile
import org.jetbrains.kotlin.ir.backend.js.jsPhases
import org.jetbrains.kotlin.ir.backend.js.jsResolveLibraries
import org.jetbrains.kotlin.ir.declarations.impl.IrFactoryImpl
import org.jetbrains.kotlin.js.config.JSConfigurationKeys
import org.jetbrains.kotlin.serialization.js.ModuleKind
import org.jetbrains.kotlin.test.ConfigurationKind
import org.jetbrains.kotlin.test.KotlinTestUtils
import org.jetbrains.kotlin.test.TargetBackend
import org.jetbrains.kotlin.test.TestJdkKind
import java.io.File

abstract class AbstractJsInterpreterTestCase : AbstractIrJsTextTestCase() {

    private val fullRuntimeKlib = "build/js-ir-runtime/klib"
    private val messageCollector = TestMessageCollector()

    class TestMessageCollector : MessageCollector {
        data class Message(val severity: CompilerMessageSeverity, val message: String, val location: CompilerMessageSourceLocation?)

        val messages = arrayListOf<Message>()

        override fun clear() {
            messages.clear()
        }

        override fun report(severity: CompilerMessageSeverity, message: String, location: CompilerMessageSourceLocation?) {
            if (severity == CompilerMessageSeverity.INFO || severity == CompilerMessageSeverity.EXCEPTION) {
                messages.add(Message(severity, message, location))
            }
        }

        override fun hasErrors(): Boolean = false

        override fun toString(): String {
            return messages.joinToString("\n\n") { "${it.severity}: ${it.message}${it.location?.let{" at $it"} ?: ""}" }
        }
    }

    override fun doTest(wholeFile: File, testFiles: List<TestFile>) {
        setupEnvironment(testFiles)
        loadMultiFiles(testFiles)

        val runtimeKlibs = listOf(fullRuntimeKlib)
        val allKlibPaths = runtimeKlibs.map { File(it).absolutePath }
        val resolvedLibraries = jsResolveLibraries(allKlibPaths, emptyList(), messageCollectorLogger(MessageCollector.NONE))

        val mainModule = MainModule.SourceFiles(myFiles.psiFiles)
        val configuration = myEnvironment.configuration
        val analyzer = AnalyzerWithCompilerReport(configuration)
        val phaseConfig = createPhaseConfig(jsPhases, K2JSCompilerArguments(), messageCollector)
        compile(
            myEnvironment.project,
            mainModule,
            analyzer,
            configuration,
            phaseConfig,
            IrFactoryImpl,
            resolvedLibraries,
            listOf(),
            null,
            propertyLazyInitialization = false
        )

        val expectedPath = wholeFile.absolutePath.replace(".kt", ".txt")
        KotlinTestUtils.assertEqualsToFile(File(expectedPath), messageCollector.toString())
    }

    private fun setupEnvironment(testFiles: List<TestFile>) {
        val configuration = createConfiguration(
            ConfigurationKind.ALL, TestJdkKind.FULL_JDK, TargetBackend.JS_IR, listOf(), listOfNotNull(writeJavaFiles(testFiles)), testFiles
        )
        //needs this to be able to compile code
        configuration.put(CommonConfigurationKeys.MODULE_NAME, "<test-module>")
        configuration.put(JSConfigurationKeys.MODULE_KIND, ModuleKind.PLAIN)
        configuration.put(CLIConfigurationKeys.MESSAGE_COLLECTOR_KEY, messageCollector)

        myEnvironment = KotlinCoreEnvironment.createForTests(testRootDisposable, configuration, EnvironmentConfigFiles.JS_CONFIG_FILES)
    }
}
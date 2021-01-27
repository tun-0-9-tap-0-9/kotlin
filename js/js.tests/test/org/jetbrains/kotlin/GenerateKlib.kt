/*
 * Copyright 2010-2021 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin

import com.intellij.openapi.vfs.StandardFileSystems
import com.intellij.openapi.vfs.VirtualFileManager
import com.intellij.psi.PsiManager
import org.jetbrains.kotlin.analyzer.AnalysisResult
import org.jetbrains.kotlin.backend.common.serialization.signature.IdSignatureDescriptor
import org.jetbrains.kotlin.cli.jvm.compiler.EnvironmentConfigFiles
import org.jetbrains.kotlin.cli.jvm.compiler.KotlinCoreEnvironment
import org.jetbrains.kotlin.config.*
import org.jetbrains.kotlin.ir.backend.js.lower.serialization.ir.JsIrLinker
import org.jetbrains.kotlin.ir.backend.js.lower.serialization.ir.JsManglerDesc
import org.jetbrains.kotlin.ir.backend.js.serializeModuleIntoKlib
import org.jetbrains.kotlin.ir.declarations.IrModuleFragment
import org.jetbrains.kotlin.ir.declarations.persistent.PersistentIrFactory
import org.jetbrains.kotlin.ir.descriptors.IrFunctionFactory
import org.jetbrains.kotlin.ir.util.IrMessageLogger
import org.jetbrains.kotlin.ir.util.SymbolTable
import org.jetbrains.kotlin.js.analyze.TopDownAnalyzerFacadeForJS
import org.jetbrains.kotlin.js.config.ErrorTolerancePolicy
import org.jetbrains.kotlin.js.config.JSConfigurationKeys
import org.jetbrains.kotlin.progress.ProgressIndicatorAndCompilationCanceledStatus
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi2ir.Psi2IrConfiguration
import org.jetbrains.kotlin.psi2ir.Psi2IrTranslator
import org.jetbrains.kotlin.resolve.CompilerEnvironment
import org.jetbrains.kotlin.resolve.multiplatform.isCommonSource
import org.jetbrains.kotlin.serialization.js.ModuleKind
import java.io.File

fun main() {
    val commonPath = listOf(
        "libraries/kotlin.test/common/src/main/",
        "libraries/kotlin.test/annotations-common/src/main/",
        "libraries/kotlin.test/js/src/main",
        "libraries/stdlib/jvm/runtime/kotlin/KotlinNullPointerException.kt",
        "libraries/stdlib/jvm/src/kotlin/reflect/KAnnotatedElement.kt",
        "libraries/stdlib/jvm/src/kotlin/reflect/KParameter.kt",
        "libraries/stdlib/jvm/src/kotlin/reflect/KVisibility.kt",
        "libraries/stdlib/jvm/src/kotlin/reflect/KDeclarationContainer.kt"
    )
    val commonPsis = commonPath.map { File(it) }.flatMap { it.listAllFiles() }.mapNotNull { createPsiFile(it.path, true) }

    val files = fullRuntimeSourceSet + commonPsis
    val analysisResult = doFrontEnd(files)
    val rawModuleFragment = doPsi2Ir(files, analysisResult)

    serializeModuleIntoKlib(
        moduleName,
        project,
        configuration,
        IrMessageLogger.None,
        analysisResult.bindingContext,
        files,
        "build/js-ir-runtime/klib",
        emptyList(),
        rawModuleFragment,
        mutableMapOf(),
        emptyList(),
        nopack = true,
        perFile = false,
        jsOutputName = null
    )
}

private fun buildConfiguration(environment: KotlinCoreEnvironment): CompilerConfiguration {
    val runtimeConfiguration = environment.configuration.copy()
    runtimeConfiguration.put(CommonConfigurationKeys.MODULE_NAME, "JS_IR_RUNTIME")
    runtimeConfiguration.put(JSConfigurationKeys.MODULE_KIND, ModuleKind.UMD)

    runtimeConfiguration.languageVersionSettings = LanguageVersionSettingsImpl(
        LanguageVersion.LATEST_STABLE, ApiVersion.LATEST_STABLE,
        specificFeatures = mapOf(
            LanguageFeature.AllowContractsForCustomFunctions to LanguageFeature.State.ENABLED,
            LanguageFeature.MultiPlatformProjects to LanguageFeature.State.ENABLED
        ),
        analysisFlags = mapOf(
            AnalysisFlags.useExperimental to listOf(
                "kotlin.ExperimentalStdlibApi", "kotlin.contracts.ExperimentalContracts",
                "kotlin.Experimental", "kotlin.ExperimentalMultiplatform"
            ),
            AnalysisFlags.allowResultReturnType to true
        )
    )

    return runtimeConfiguration
}

private val environment = KotlinCoreEnvironment.createForTests({ }, CompilerConfiguration(), EnvironmentConfigFiles.JS_CONFIG_FILES)
private val configuration = buildConfiguration(environment)
private val project = environment.project
private val languageVersionSettings = configuration.languageVersionSettings
private val moduleName = configuration[CommonConfigurationKeys.MODULE_NAME]!!
private val fullRuntimeSourceSet = createPsiFileFromDir("libraries/stdlib/js-ir", "builtins", "runtime", "src")

fun createPsiFile(fileName: String, isCommon: Boolean): KtFile? {
    val psiManager = PsiManager.getInstance(environment.project)
    val fileSystem = VirtualFileManager.getInstance().getFileSystem(StandardFileSystems.FILE_PROTOCOL)

    val file = fileSystem.findFileByPath(fileName) ?: error("File not found: $fileName")

    val psiFile = psiManager.findFile(file)

    return (psiFile as? KtFile)?.apply { isCommonSource = isCommon }
}

private fun File.listAllFiles(): List<File> {
    return if (isDirectory) listFiles().flatMap { it.listAllFiles() }
    else listOf(this)
}

private fun createPsiFileFromDir(path: String, vararg extraDirs: String): List<KtFile> {
    val dir = File(path)
    val buildPath = File(dir, "build")
    val commonPath = File(buildPath, "commonMainSources")
    val extraPaths = extraDirs.map { File(dir, it) }
    val jsPaths = listOf(File(buildPath, "jsMainSources")) + extraPaths
    val commonPsis = commonPath.listAllFiles().mapNotNull { createPsiFile(it.path, true) }
    val jsPsis = jsPaths.flatMap { d -> d.listAllFiles().mapNotNull { createPsiFile(it.path, false) } }
    return commonPsis + jsPsis
}

private fun doFrontEnd(files: List<KtFile>): AnalysisResult {
    val analysisResult =
        TopDownAnalyzerFacadeForJS.analyzeFiles(
            files,
            project,
            configuration,
            emptyList(),
            friendModuleDescriptors = emptyList(),
            CompilerEnvironment,
            thisIsBuiltInsModule = true,
            customBuiltInsModule = null
        )

    ProgressIndicatorAndCompilationCanceledStatus.checkCanceled()
    TopDownAnalyzerFacadeForJS.checkForErrors(files, analysisResult.bindingContext, ErrorTolerancePolicy.NONE)

    return analysisResult
}

private fun doPsi2Ir(files: List<KtFile>, analysisResult: AnalysisResult): IrModuleFragment {
    val psi2Ir = Psi2IrTranslator(languageVersionSettings, Psi2IrConfiguration())
    val symbolTable = SymbolTable(IdSignatureDescriptor(JsManglerDesc), PersistentIrFactory())
    val psi2IrContext = psi2Ir.createGeneratorContext(analysisResult.moduleDescriptor, analysisResult.bindingContext, symbolTable)

    val irBuiltIns = psi2IrContext.irBuiltIns
    val functionFactory = IrFunctionFactory(irBuiltIns, psi2IrContext.symbolTable)
    irBuiltIns.functionFactory = functionFactory

    val irLinker = JsIrLinker(
        psi2IrContext.moduleDescriptor,
        IrMessageLogger.None,
        psi2IrContext.irBuiltIns,
        psi2IrContext.symbolTable,
        functionFactory,
        null
    )

    val irProviders = listOf(irLinker)

    val psi2IrTranslator = Psi2IrTranslator(languageVersionSettings, psi2IrContext.configuration)
    return psi2IrTranslator.generateModuleFragment(psi2IrContext, files, irProviders, emptyList(), null)
}
/*
 * Copyright 2010-2021 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.fir.analysis.checkers.declaration

import org.jetbrains.kotlin.fir.analysis.checkers.context.CheckerContext
import org.jetbrains.kotlin.fir.analysis.diagnostics.DiagnosticReporter
import org.jetbrains.kotlin.fir.analysis.diagnostics.FirErrors
import org.jetbrains.kotlin.fir.analysis.diagnostics.reportOn
import org.jetbrains.kotlin.fir.declarations.FirFile
import org.jetbrains.kotlin.fir.declarations.FirStatusOwner
import org.jetbrains.kotlin.fir.declarations.FirTypeAlias

object FirTopLevelTypeAliasChecker : FirTypeAliasChecker() {
    override fun check(declaration: FirTypeAlias, context: CheckerContext, reporter: DiagnosticReporter) {
        if (context.containingDeclarations.lastOrNull() !is FirFile) {
            reporter.reportOn(declaration.source, FirErrors.TOPLEVEL_TYPEALIASES_ONLY, context)
        }
    }
}

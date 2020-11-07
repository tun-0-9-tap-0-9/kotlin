/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package kotlin.experimental

import kotlin.internal.InlineOnly

@InlineOnly
@CompileTimeCalculation
@EvaluateIntrinsic("")
//Will return file and line number there this function was called
inline fun sourceLocation(): String = throw NotImplementedError("Implemented as intrinsic")
/*
 * Copyright 2010-2018 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

@file:Suppress("ACTUAL_WITHOUT_EXPECT") // for building kotlin-stdlib-jvm-minimal-for-test

package kotlin

@CompileTimeCalculation
@EvaluateIntrinsic("java.lang.Error")
@SinceKotlin("1.1") public actual typealias Error = java.lang.Error

@CompileTimeCalculation
@EvaluateIntrinsic("java.lang.Exception")
@SinceKotlin("1.1") public actual typealias Exception = java.lang.Exception

@CompileTimeCalculation
@EvaluateIntrinsic("java.lang.RuntimeException")
@SinceKotlin("1.1") public actual typealias RuntimeException = java.lang.RuntimeException

@CompileTimeCalculation
@EvaluateIntrinsic("java.lang.IllegalArgumentException")
@SinceKotlin("1.1") public actual typealias IllegalArgumentException = java.lang.IllegalArgumentException

@CompileTimeCalculation
@EvaluateIntrinsic("java.lang.IllegalStateException")
@SinceKotlin("1.1") public actual typealias IllegalStateException = java.lang.IllegalStateException

@CompileTimeCalculation
@EvaluateIntrinsic("java.lang.IndexOutOfBoundsException")
@SinceKotlin("1.1") public actual typealias IndexOutOfBoundsException = java.lang.IndexOutOfBoundsException

@CompileTimeCalculation
@EvaluateIntrinsic("java.lang.UnsupportedOperationException")
@SinceKotlin("1.1") public actual typealias UnsupportedOperationException = java.lang.UnsupportedOperationException

@CompileTimeCalculation
@EvaluateIntrinsic("java.lang.ArithmeticException")
@SinceKotlin("1.3") public actual typealias ArithmeticException = java.lang.ArithmeticException

@CompileTimeCalculation
@EvaluateIntrinsic("java.lang.NumberFormatException")
@SinceKotlin("1.1") public actual typealias NumberFormatException = java.lang.NumberFormatException

@CompileTimeCalculation
@EvaluateIntrinsic("java.lang.NullPointerException")
@SinceKotlin("1.1") public actual typealias NullPointerException = java.lang.NullPointerException

@CompileTimeCalculation
@EvaluateIntrinsic("java.lang.ClassCastException")
@SinceKotlin("1.1") public actual typealias ClassCastException = java.lang.ClassCastException

@CompileTimeCalculation
@EvaluateIntrinsic("java.lang.AssertionError")
@SinceKotlin("1.1") public actual typealias AssertionError = java.lang.AssertionError

@CompileTimeCalculation
@EvaluateIntrinsic("java.util.NoSuchElementException")
@SinceKotlin("1.1") public actual typealias NoSuchElementException = java.util.NoSuchElementException

@CompileTimeCalculation
@EvaluateIntrinsic("java.util.ConcurrentModificationException")
@Suppress("NO_ACTUAL_CLASS_MEMBER_FOR_EXPECTED_CLASS")
@SinceKotlin("1.3") public actual typealias ConcurrentModificationException = java.util.ConcurrentModificationException

@CompileTimeCalculation
@EvaluateIntrinsic("java.util.Comparator")
@SinceKotlin("1.1") public actual typealias Comparator<T> = java.util.Comparator<T>

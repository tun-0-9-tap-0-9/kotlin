/*
 * Copyright 2010-2016 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package kotlin

import kotlin.internal.PureReifiable

/**
 * Returns a string representation of the object. Can be called with a null receiver, in which case
 * it returns the string "null".
 */
@CompileTimeCalculation
public fun Any?.toString(): String

/**
 * Concatenates this string with the string representation of the given [other] object. If either the receiver
 * or the [other] object are null, they are represented as the string "null".
 */
public operator fun String?.plus(other: Any?): String

/**
 * Returns an array of objects of the given type with the given [size], initialized with null values.
 */
@CompileTimeCalculation
public fun <reified @PureReifiable T> arrayOfNulls(size: Int): Array<T?>

/**
 * Returns an array containing the specified elements.
 */
@CompileTimeCalculation
public inline fun <reified @PureReifiable T> arrayOf(vararg elements: T): Array<T>

/**
 * Returns an array containing the specified [Double] numbers.
 */
@CompileTimeCalculation
public fun doubleArrayOf(vararg elements: Double): DoubleArray

/**
 * Returns an array containing the specified [Float] numbers.
 */
@CompileTimeCalculation
public fun floatArrayOf(vararg elements: Float): FloatArray

/**
 * Returns an array containing the specified [Long] numbers.
 */
@CompileTimeCalculation
public fun longArrayOf(vararg elements: Long): LongArray

/**
 * Returns an array containing the specified [Int] numbers.
 */
@CompileTimeCalculation
public fun intArrayOf(vararg elements: Int): IntArray

/**
 * Returns an array containing the specified characters.
 */
@CompileTimeCalculation
public fun charArrayOf(vararg elements: Char): CharArray

/**
 * Returns an array containing the specified [Short] numbers.
 */
@CompileTimeCalculation
public fun shortArrayOf(vararg elements: Short): ShortArray

/**
 * Returns an array containing the specified [Byte] numbers.
 */
@CompileTimeCalculation
public fun byteArrayOf(vararg elements: Byte): ByteArray

/**
 * Returns an array containing the specified boolean values.
 */
@CompileTimeCalculation
public fun booleanArrayOf(vararg elements: Boolean): BooleanArray

/**
 * Returns an array containing enum T entries.
 */
@SinceKotlin("1.1")
@CompileTimeCalculation
public inline fun <reified T : Enum<T>> enumValues(): Array<T>

/**
 * Returns an enum entry with specified name.
 */
@SinceKotlin("1.1")
@CompileTimeCalculation
public inline fun <reified T : Enum<T>> enumValueOf(name: String): T
// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
fun <T> getArray(array: Array<T>) = array

const val a1 = getArray(arrayOf(1, 2.0, "3")) as? Array<Int> == null
const val a2 = getArray(arrayOf(1, 2, 3)) as? Array<Int> == null
const val a3 = getArray(arrayOf(1, 2, 3)) as? Array<Double> == null
const val a4 = getArray(arrayOf(1, 2, 3)) as? Array<Number> == null

const val b1 = arrayOf(arrayOf(1, 2, 3)) as? Array<Array<String>> == null
const val b2 = arrayOf(arrayOf(1, 2, 3)) as? Array<Array<Int>> == null
const val b3 = arrayOf(arrayOf(1, 2, 3)) as? Array<Array<Number>> == null

const val c1 = arrayOf(arrayOf(1, 2, 3), arrayOf("1", "2", "3"))[0] as? Array<Int> == null
const val c2 = arrayOf(arrayOf(1, 2, 3), arrayOf("1", "2", "3"))[1] as? Array<String> == null

@CompileTimeCalculation
fun <T, E> combineArrays(array1: Array<T>, array2: Array<E>) = arrayOf(array1, array2)

const val d1 = combineArrays(arrayOf(1, 2, 3), arrayOf(1, 2, 3)) as? Array<Array<Int>> == null
const val d2 = combineArrays(arrayOf(1, 2, 3), arrayOf(1, 2, 3)) as? Array<Array<Number>> == null
const val d3 = combineArrays(arrayOf(1, 2, 3), arrayOf(1, 2, 3)) as? Array<Array<Any>> == null
const val d4 = combineArrays(arrayOf(1, 2, 3), arrayOf(1, 2, 3)) as? Array<Array<*>> == null
const val d5 = combineArrays(arrayOf(1, 2, 3), arrayOf("1", "2", "3"))[0] as? Array<Int> == null
const val d6 = combineArrays(arrayOf(1, 2, 3), arrayOf("1", "2", "3"))[1] as? Array<Int> == null
const val d7 = combineArrays(arrayOf(1, 2, 3), arrayOf("1", "2", "3"))[1] as? Array<String> == null

@CompileTimeCalculation
fun <T> echo(array: T) = array
const val e1 = echo<Any>(arrayOf(1, 2, 3)) as? Array<Int> == null
const val e2 = echo<Any>(arrayOf(arrayOf(1, 2, 3))) as? Array<Array<Int>> == null
const val e3 = echo<Any>(arrayOf(echo<Any>(1), echo<Any>(2), echo<Any>(3))) as? Array<Int> == null

// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
fun <T> foo(): String {
    return if (listOf<Int>() as? T == null) "Can't cast" else "Safe cast"
}

@CompileTimeCalculation
inline fun <reified T> bar(): String {
    return if (listOf<Int>() as? T == null) "Can't cast" else "Safe cast"
}

const val a1 = foo<Int>()
const val a2 = foo<Int?>()
const val a3 = foo<Double?>()
const val a4 = foo<List<*>>()
const val a5 = foo<Map<*,*>>()

const val b1 = bar<Int>()
const val b2 = bar<Int?>()
const val b3 = bar<Double?>()
const val b4 = bar<Map<*,*>>()
const val b5 = bar<List<Int>>()
const val b6 = bar<List<String>>()

const val c1 = arrayOf<Int>(1, 2, 3) as? Array<String> == null
const val c2 = arrayOf<Int>(1, 2, 3) as? Array<Number> == null
const val c3 = arrayOf<Any>(listOf(1, 2), listOf(2, 3)) as? Array<List<String>?> == null
const val c4 = arrayOf<List<Int>>(listOf(1, 2), listOf(2, 3)) as? Array<List<String>?> == null
const val c5 = arrayOf<List<Int>>(listOf(1, 2), listOf(2, 3)) as? Array<Set<Int>> == null
const val c6 = arrayOf<List<Int>>(listOf(1, 2), listOf(2, 3)) as? Array<Collection<String>> == null
const val c7 = Array<List<Int>>(3) { listOf(it, it + 1) } as? Array<List<String>?> == null
const val c8 = Array<List<Int>>(3) { listOf(it, it + 1) } as? Array<Set<Int>> == null

// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
fun classCastWithException(a: Any): String {
    return try {
        a as Int
        "Given value is $a and its doubled value is ${2 * a}"
    } catch (e: ClassCastException) {
        "Given value isnt't Int; Exception message: \"${e.message}\""
    }
}

@CompileTimeCalculation
fun safeClassCast(a: Any): Int {
    return (a as? String)?.length ?: -1
}

@CompileTimeCalculation
fun <T> unsafeClassCast(): T {
    return 1 as T
}

@CompileTimeCalculation
fun <T> getIntList() = listOf<Int>(1, 2) as T

@CompileTimeCalculation
fun <T> getStringNullableList() = listOf<String?>(null, "1") as T

@CompileTimeCalculation
fun getLength(str: String) = str.length

@CompileTimeCalculation
class A<T>() {
    fun unsafeCast(): T {
        return 1 as T
    }
}

const val a1 = classCastWithException(10)
const val a2 = classCastWithException("10")

const val b1 = safeClassCast(10)
const val b2 = safeClassCast("10")

// in this example all unsafe cast will be "successful", but will fall when trying to assign
const val c1 = unsafeClassCast<Int>()
const val c2 = unsafeClassCast<String>()

const val d1 = A<Int>().unsafeCast()
const val d2 = A<String>().unsafeCast()

const val stringList = getIntList<List<String>>().let {
    it[0].length
}
const val nullableStringList = getStringNullableList<List<String>>().let { it[0].length }
const val nullableStringLength = getLength(getStringNullableList<List<String>>()[0])
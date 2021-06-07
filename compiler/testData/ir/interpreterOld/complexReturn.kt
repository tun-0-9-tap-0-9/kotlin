// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
fun <T> firstNotNull(list: List<T?>): T {
    for (elem in list) {
        return (elem ?: continue)
    }

    throw NoSuchElementException("All elements are null")
}

const val a = firstNotNull(listOf(1, 2, 3))
const val b = firstNotNull(listOf(1, null, 3))
const val c = firstNotNull(listOf(null, 2, 3))
const val d = firstNotNull(listOf(null, null, 3))
val e = firstNotNull(listOf<Int?>(null, null, null))
// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
fun numberIsInArray(array: IntArray, number: Int): Boolean {
    return number in array
}

@CompileTimeCalculation
fun valueIsInArray(array: Array<Any>, value: Any?): Boolean {
    return value in array
}

const val a1 = numberIsInArray(intArrayOf(1, 2, 3), 1)
const val a2 = numberIsInArray(intArrayOf(1, 2, 3), -1)

const val b1 = valueIsInArray(arrayOf(1, 2, 3), 1)
const val b2 = valueIsInArray(arrayOf(1, 2, 3), -1)
const val b3 = valueIsInArray(arrayOf(1, 2.0f, "3"), "3")
const val b4 = valueIsInArray(arrayOf(1, 2.0f, "3"), null)
const val b5 = valueIsInArray(arrayOf(1, 2.0f, "3"), 1.0f)

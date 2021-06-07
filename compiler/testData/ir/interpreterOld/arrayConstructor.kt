// !LANGUAGE: +CompileTimeCalculations

const val intArray1 = IntArray(42).size
const val intArray2 = IntArray(42)[0]
const val intArray3 = IntArray(10) { 42 }[0]
const val intArray4 = IntArray(10) { it -> it }[7]

const val floatArray1 = FloatArray(42).size
const val floatArray2 = FloatArray(42)[0]
const val floatArray3 = FloatArray(10) { 42.5f }[0]
const val floatArray4 = FloatArray(10) { it -> it.toFloat() }[7]

const val array = Array<Any?>(4) {
    when(it) {
        0 -> 1
        1 -> 2.0
        2 -> "3"
        3 -> null
        else -> throw IllegalArgumentException("$it is wrong")
    }
}.let { it[0].toString() + " " + it[1] + " " + it[2] + " " + it[3] }

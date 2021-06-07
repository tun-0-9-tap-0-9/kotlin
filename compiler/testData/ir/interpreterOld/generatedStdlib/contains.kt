// !LANGUAGE: +CompileTimeCalculations

const val a = intArrayOf(1, 2, 3).contains(1)
const val b = intArrayOf(1, 2, 3).contains(4)
const val c = arrayOf(1, "2", 3.0).contains("2")
const val d = sequenceOf(1, 2, 3).contains(2)
// !LANGUAGE: +CompileTimeCalculations

const val zeroElementIntArrayToList = intArrayOf().toList().size
const val singleElementIntArrayToList = intArrayOf(1).toList().size
const val intArrayToList = intArrayOf(1, 2, 3).toList().size
const val customArrayToList = arrayOf(1, "2", 3.0, "Some other value").toList()[3] as String

const val listFromSet = setOf(1, 2, 2, 3, 3).toList().joinToString()
const val listFromIterable = mapOf(1 to "One", 2 to "Two", 3 to "Three").entries.toList().joinToString()

const val stringToList = "String".toList().joinToString()

const val sequenceToList = sequenceOf(3, 2, 1).toList().joinToString()
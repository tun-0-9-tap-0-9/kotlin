// !LANGUAGE: +CompileTimeCalculations

const val a = "abcs".first()
const val b = UIntArray(3) { it.toUInt() }.first()
const val c = listOf(1, "2", 3.0).first() as Int
const val d = listOf<Int>().first()
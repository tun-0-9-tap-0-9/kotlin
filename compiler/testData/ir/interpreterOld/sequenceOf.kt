// !LANGUAGE: +CompileTimeCalculations

const val a = sequenceOf(1, 2, 3).iterator().next()
const val b = sequenceOf(2, 3).iterator().next()
const val c = sequenceOf<Int>().iterator().hasNext()
const val d = generateSequence() { 42 }.iterator().next()
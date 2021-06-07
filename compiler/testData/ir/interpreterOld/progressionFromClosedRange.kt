// !LANGUAGE: +CompileTimeCalculations

const val a = IntProgression.fromClosedRange(0, 10, 1).first
const val b = LongProgression.fromClosedRange(0L, 10L, 1L).last
const val c = CharProgression.fromClosedRange('0', '9', 1).step
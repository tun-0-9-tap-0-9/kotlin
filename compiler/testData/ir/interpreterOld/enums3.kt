// !LANGUAGE: +CompileTimeCalculations

const val a = RegexOption.IGNORE_CASE.name
const val b = RegexOption.IGNORE_CASE.ordinal
const val c = RegexOption.MULTILINE.name
const val d = RegexOption.MULTILINE.ordinal
const val e = RegexOption.IGNORE_CASE == RegexOption.IGNORE_CASE
const val f = RegexOption.IGNORE_CASE != RegexOption.MULTILINE
const val g = RegexOption.IGNORE_CASE.toString()
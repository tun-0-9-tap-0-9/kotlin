// !LANGUAGE: +CompileTimeCalculations

const val trimmed = "  1  ".trim()
const val trimmedWithPredicate = ("  2  " as CharSequence).trim { it.isWhitespace() }.toString()
const val charSequenceTrim = ("  3  " as CharSequence).trim().toString()
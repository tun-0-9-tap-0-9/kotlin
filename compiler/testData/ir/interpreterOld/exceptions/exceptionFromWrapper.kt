// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
fun append(sb: StringBuilder, value: CharSequence, start: Int, end: Int): String {
    return sb.append(value, start, end).toString()
}

const val a = append(StringBuilder("Some string with not zero length"), "!!!", 0, 3)
const val b = append(StringBuilder("Some string with not zero length"), "!!!", -1, 0)
// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
fun getLenght(value: String?): Int = value?.length ?: -1

const val a1 = getLenght("Elvis")
const val a2 = getLenght(null)
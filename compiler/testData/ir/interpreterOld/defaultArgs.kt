// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
fun sum(a: Int = 1, b: Int = 2, c: Int = 3) = a + b + c

@CompileTimeCalculation
fun sumBasedOnPrevious(a: Int = 1, b: Int = a * 2, c: Int = b * 2) = a + b + c

const val sum1 = sum()
const val sum2 = sum(b = -3)
const val sum3 = sum(c = 1, a = 1, b = 1)

const val sumBasedOnPrevious1 = sumBasedOnPrevious()
const val sumBasedOnPrevious2 = sumBasedOnPrevious(b = 1, c = 1)

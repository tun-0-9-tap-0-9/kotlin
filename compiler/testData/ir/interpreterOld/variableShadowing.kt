// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
fun returnValueFromA(a: Int, b: Int): Int {
    val a = b
    return a
}

const val num = returnValueFromA(1, 2)
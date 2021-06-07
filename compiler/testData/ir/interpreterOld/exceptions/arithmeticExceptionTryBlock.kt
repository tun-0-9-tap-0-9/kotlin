// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
fun tryCatch(integer: Int): Boolean {
    try {
        val a = 10 / integer
        return true
    } catch (e: ArithmeticException) {
        return false
    }
}

const val a1 = tryCatch(0)
const val a2 = tryCatch(1)
const val a3 = tryCatch(100)

@CompileTimeCalculation
fun multiTryCatch(integer: Int): String {
    return try {
        val a = 10 / integer
        "Normal"
    } catch (e: AssertionError) {
        "AssertionError"
    } catch (e: ArithmeticException) {
        "ArithmeticException"
    }
}

const val b1 = multiTryCatch(0)
const val b2 = multiTryCatch(1)
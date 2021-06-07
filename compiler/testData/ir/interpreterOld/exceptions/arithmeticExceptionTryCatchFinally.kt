// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
fun tryCatch(integer: Int): String {
    var str = ""
    try {
        str += "Start dividing\n"
        val a = 10 / integer
        str += "Without exception\n"
    } catch (e: ArithmeticException) {
        str += "Exception\n"
    } finally {
        str += "Finally\n"
    }
    return str
}

const val a1 = tryCatch(0)
const val a2 = tryCatch(1)
const val a3 = tryCatch(100)
// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
fun divide(a: Int, b: Int) = a / b

@CompileTimeCalculation
fun echo(a : Int, b: Int) = divide(a, b)

@CompileTimeCalculation
fun getLengthOrThrowException(str: String?): Int {
    try {
        return str!!.length
    } catch (e: NullPointerException) {
        throw UnsupportedOperationException(e)
    }
}

const val a = echo(1, 0)
const val b = getLengthOrThrowException(null)

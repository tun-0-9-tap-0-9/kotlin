// https://docs.oracle.com/javase/1.5.0/docs/api/java/lang/Throwable.html#printStackTrace()
// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
class HighLevelException(cause: Throwable?) : Exception(cause)
@CompileTimeCalculation
class MidLevelException(cause: Throwable?) : Exception(cause)
@CompileTimeCalculation
class LowLevelException : Exception()

@CompileTimeCalculation
class Junk {
    public fun a(): Nothing {
        try {
            b()
        } catch (e: MidLevelException) {
            throw HighLevelException(e)
        }
    }

    private fun b(): Nothing = c()

    private fun c(): Nothing {
        try {
            d()
        } catch (e: LowLevelException) {
            throw MidLevelException(e)
        }
    }

    private fun d(): Nothing = e()

    private fun e(): Nothing = throw LowLevelException()
}

val exceptionWithCause: Nothing = Junk().a()
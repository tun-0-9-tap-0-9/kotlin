// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
fun foo(i: Int): Int = foo(i + 1)
const val overflow = foo(0)

@CompileTimeCalculation
fun withPossibleOverflow(x: Int): Int {
    if (x == 0) return 0
    return withPossibleOverflow(x - 1) + 1
}
const val notOverflow = withPossibleOverflow(5_000)
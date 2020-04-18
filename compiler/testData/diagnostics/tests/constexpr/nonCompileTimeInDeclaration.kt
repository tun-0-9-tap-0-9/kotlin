// !LANGUAGE: +CompileTimeCalculations

fun plus(a: Int, b: Int) = a + b

@CompileTimeCalculation
class A(val a: Int) {
    fun inc() = <!NON_COMPILE_TIME_EXPRESSION_IN_COMPILE_TIME_DECLARATION!>plus<!>(a, 1)
}

class B(val b: Int) {
    @CompileTimeCalculation
    fun get() = <!NON_COMPILE_TIME_EXPRESSION_IN_COMPILE_TIME_DECLARATION!>b<!>
}

class c(val c: Int) {
    @CompileTimeCalculation
    constructor(): <!NON_COMPILE_TIME_EXPRESSION_IN_COMPILE_TIME_DECLARATION!>this<!>(0)
}
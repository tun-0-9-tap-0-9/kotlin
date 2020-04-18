// !LANGUAGE: +CompileTimeCalculations

fun plus(a: Int, b: Int) = a + b

@CompileTimeCalculation
class A(val a: Int) {
    fun inc() = plus(a, 1)
}

class B(val b: Int) {
    @CompileTimeCalculation
    fun get() = b
}

class c(val c: Int) {
    @CompileTimeCalculation
    constructor(): this(0)
}
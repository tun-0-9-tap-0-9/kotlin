// !LANGUAGE: +CompileTimeCalculations

fun getNotConstExprInt() = 1
const val a = <!CONST_VAL_WITH_NON_CONST_INITIALIZER!>getNotConstExprInt<!>()

open class A(val a: Int)
const val get = <!CONST_VAL_WITH_NON_CONST_INITIALIZER!>A<!>(10).<!CONST_VAL_WITH_NON_CONST_INITIALIZER!>a<!>

class B @CompileTimeCalculation constructor(val b: Int): <!NON_COMPILE_TIME_EXPRESSION_IN_COMPILE_TIME_DECLARATION!>A<!>(b + 1)
const val getAFromB = B(3).<!CONST_VAL_WITH_NON_CONST_INITIALIZER!>a<!>

class C @CompileTimeCalculation constructor(val c: Int) {
    fun get() = c
}
const val getC = C(4).<!CONST_VAL_WITH_NON_CONST_INITIALIZER!>get<!>()
const val equals = C(4).equals(C(5))
const val toString = C(6).toString()

class D @CompileTimeCalculation constructor() {
    <!COMPILE_TIME_MEMBER_NOT_IMPLEMENTED!>override fun hashCode(): Int {
        return super.hashCode()
    }<!>
}
const val hashCode = D().hashCode()

class E @CompileTimeCalculation constructor() {
    val e: Int = 1
}
const val getE = E().<!CONST_VAL_WITH_NON_CONST_INITIALIZER!>e<!>
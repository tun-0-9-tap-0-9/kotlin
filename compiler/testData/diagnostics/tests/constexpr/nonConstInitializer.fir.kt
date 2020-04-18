// !LANGUAGE: +CompileTimeCalculations

fun getNotConstExprInt() = 1
const val a = getNotConstExprInt()

open class A(val a: Int)
const val get = A(10).a

class B @CompileTimeCalculation constructor(val b: Int): A(b + 1)
const val getAFromB = B(3).a

class C @CompileTimeCalculation constructor(val c: Int) {
    fun get() = c
}
const val getC = C(4).get()
const val equals = C(4).equals(C(5))
const val toString = C(6).toString()

class D @CompileTimeCalculation constructor() {
    override fun hashCode(): Int {
        return super.hashCode()
    }
}
const val hashCode = D().hashCode()

class E @CompileTimeCalculation constructor() {
    val e: Int = 1
}
const val getE = E().e
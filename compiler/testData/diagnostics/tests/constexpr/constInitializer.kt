// FIR_IDENTICAL
// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
fun getInt() = 1
const val a = getInt()

@CompileTimeCalculation
open class A(val a: Int)
const val get = A(10).a

class B @CompileTimeCalculation constructor(val b: Int): A(b + 1)
const val getB = B(3).b
const val getAFromB = B(3).a

@CompileTimeCalculation
class C(val c: Int) {
    fun get() = c
}
const val getC = C(4).get()
const val getCAsProperty = C(4).c
const val equals = C(4).equals(C(5))
const val toString = C(6).toString()

class D @CompileTimeCalculation constructor() {
    @CompileTimeCalculation
    override fun hashCode(): Int {
        return super.hashCode()
    }
}
const val hashCode = D().hashCode()

class E @CompileTimeCalculation constructor() {
    @CompileTimeCalculation val e: Int = 1
}
const val getE = E().e

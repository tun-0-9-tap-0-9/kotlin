// !LANGUAGE: +CompileTimeCalculations

open class A @CompileTimeCalculation constructor(var a: Int) {
    @CompileTimeCalculation
    fun get(): Int {
        return a
    }

    @CompileTimeCalculation
    open fun openGet(): Int {
        return a
    }

    @CompileTimeCalculation
    fun setA(a: Int): A {
        this.a = a
        return this
    }
}

open class B @CompileTimeCalculation constructor(val b: Int) : A(b + 1) {
    @CompileTimeCalculation
    fun getAFromB(): Int {
        return a
    }

    @CompileTimeCalculation
    fun getFromValueParameter(a: A): Int {
        return a.get()
    }
}

open class C @CompileTimeCalculation constructor(val c: Int) : B(c + 1) {
    @CompileTimeCalculation
    fun getAFromC(): Int {
        return a
    }

    @CompileTimeCalculation
    override fun openGet(): Int {
        return super.openGet()
    }
}

open class D @CompileTimeCalculation constructor(val d: Int) : C(d + 1) {

}

open class E @CompileTimeCalculation constructor(val e: Int) : D(e + 1) {

}

const val a1 = A(1).get()
const val a2 = A(1).setA(2).get()
const val a3 = A(1).openGet()
const val a4 = A(1).a       // property inherits compile-time annotation from primary constructor

const val b1 = B(1).getAFromB()
const val b2 = B(1).getFromValueParameter(A(10))
const val b3 = B(1).get()   //fake overridden
const val b4 = B(1).setA(-1).get()

const val c1 = C(1).getAFromC()
const val c2 = C(1).get()   //fake overridden
const val c3 = C(1).openGet()
const val c4 = C(1).getAFromB()
const val c5 = C(1).getFromValueParameter(A(10)) //method from B
const val c6 = C(1).setA(-2).get()

// test deep fake overridden
const val d1 = D(1).get()
const val e1 = E(1).get()

val nonConstButComputable = A(1).get()
//must not calculate
val mustNotCompute = nonConstButComputable + 1

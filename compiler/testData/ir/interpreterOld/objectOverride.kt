// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
interface Object {
    fun get(): String

    @CompileTimeCalculation
    fun defaultGet() = "Object"
}

object A : Object {
    @CompileTimeCalculation
    override fun get() = "A"
}

open class B : Object {
    @CompileTimeCalculation
    override fun get() = "B"
}

object C : B() {
    @CompileTimeCalculation
    override fun get() = "Default: " + super.defaultGet() + "; from super B: " + super.get() + "; from current: " + " companion C"
}

const val a1 = A.defaultGet()
const val a2 = A.get()
const val c1 = C.defaultGet()
const val c2 = C.get()
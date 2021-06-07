// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
class A {
    val a = { 10 }()

    companion object {
        @CompileTimeCalculation
        val static = { -10 }()

        val willNotCompute = { -10 }()

        @CompileTimeCalculation
        fun getStaticNumber(): Int {
            return Int.MAX_VALUE
        }
    }
}

object Obj {
    @CompileTimeCalculation
    val static = "Const val"

    val willNotCompute = "Will not compute"
}

object ObjectWithConst {
    const val a = 100
    const val b = concat("Value in a: ", a)
    val nonConst = concat("Non const, ", "still will be calculated")

    @CompileTimeCalculation fun concat(first: String, second: Any) = "$first$second"
}

const val num = A().a
const val numStatic = A.static
const val numStaticFromFun = A.getStaticNumber()
val willNotCompute = A.willNotCompute

const val valFromObject = Obj.static
val valFromObjectNotCompute = Obj.willNotCompute
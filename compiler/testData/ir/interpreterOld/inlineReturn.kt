// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
inline fun x(i: Int, w: () -> Int): Int {
    return i + w()
}

@CompileTimeCalculation
fun withNonLocalReturn(): String {
    x(10) { return "Non local" }
    return "Local"
}

@CompileTimeCalculation
fun withLocalReturn(): String {
    x(10) { return@x 1 }
    return "Local"
}

// will replace IrCall on IrConst inside body
fun foo() {
    val a = x(10) l@ { //this call will be replaced on 20
        x(9) {
            return@l 10
        }
    }
}

// will replace IrCall on IrConst inside body
fun bar(p: Int) {
    x(10) {
        val i = p
        x(11) { //this call will be replaced on 21
            val j = 0
            x(j) { 10 }
        }
    }
}

val a = withNonLocalReturn()
val b = withLocalReturn()

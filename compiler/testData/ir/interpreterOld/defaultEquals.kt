// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
class A

@CompileTimeCalculation
fun getTheSameValue(a: Any): Any = a

@CompileTimeCalculation
fun theSameObjectEquals(value: Any): Boolean {
    return value == getTheSameValue(value) && value === getTheSameValue(value)
}

const val equals1 = A().equals(A())
const val equals2 = theSameObjectEquals(A())

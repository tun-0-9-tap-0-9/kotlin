// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation fun evalWithVariables(): Double {
    val a = 1
    val b = 1.5
    return a + b
}

@CompileTimeCalculation fun evalWithVariablesLateinit(): Double {
    var a: Double
    var b: Double
    a = 1.5
    b = -3.75
    return a + b
}

@CompileTimeCalculation fun evalWithValueParameter(toAdd: Int): Int {
    var a: Int = toAdd
    a += 10
    a = a % 5
    a -= 2
    return a
}

class A @CompileTimeCalculation constructor(@CompileTimeCalculation val a: Int)

@CompileTimeCalculation fun createObject(a: Int): Int {
    val aObj = A(a)
    return aObj.a
}

const val a = evalWithVariables()
const val b = evalWithVariablesLateinit()
const val c = evalWithValueParameter(10)
const val d = createObject(c)
// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
class A(val a: Int)
const val size = mapOf(1 to "A(1)").size
const val first = mapOf(1 to "A(1)").entries.single().key
const val second = mapOf(1 to "A(1)").values.single()

// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
abstract class ConstExprClass {
    abstract fun getSomeValue(): Int
}

@CompileTimeCalculation
class A(val a: Int): ConstExprClass() {
    override fun getSomeValue() = a
}

class B @CompileTimeCalculation constructor(val b: Int): ConstExprClass() {
    <!COMPILE_TIME_MEMBER_NOT_IMPLEMENTED!>override fun getSomeValue() = b<!>
}

const val a = A(1).getSomeValue()
const val b = B(2).getSomeValue()

// !LANGUAGE: +CompileTimeCalculations

interface Base

@CompileTimeCalculation
open class A : Base

@CompileTimeCalculation
class B : A()

const val a1 = 1 is Int
const val a2 = 2 !is Int

const val b1 = A() is Base
const val b2 = A() !is Base
const val b3 = A() is A
const val b4 = A() !is A

const val c1 = B() is Base
const val c2 = B() !is Base
const val c3 = B() is A
const val c4 = B() !is A
const val c5 = B() is B
const val c6 = B() !is B
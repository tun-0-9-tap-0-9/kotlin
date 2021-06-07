// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
fun throwExample(a: Int, b: Int): Int {
    if (b == 0) throw ArithmeticException("Divide by zero")
    return a / b
}

@CompileTimeCalculation
fun throwNullMessage(a: Int, b: Int): Int {
    if (b == 0) throw ArithmeticException(null)
    return a / b
}

const val a1 = try {
    throwExample(10, 0)
    true
} catch (e: ArithmeticException) {
    false
}
const val a2 = try {
    throwExample(10, 1)
    true
} catch (e: ArithmeticException) {
    false
}

const val b1 = try {
    throwExample(10, 0)
    true
} catch (e: Exception) {
    false
}
const val b2 = try {
    throwExample(10, 1)
    true
} catch (e: Exception) {
    false
}
const val b3 = try {
    throwExample(10, 0)
    true
} catch (e: Throwable) {
    false
}

const val c1 = try {
    throwExample(10, 0)
    0
} catch (e: ArithmeticException) {
    1
} catch (e: Exception) {
    2
}
const val c2 = try {
    throwExample(10, 0)
    0
} catch (e: Exception) {
    1
} catch (e: ArithmeticException) {
    2
}
const val c3 = try {
    throwExample(10, 0)
    0
} catch (e: NullPointerException) {
    1
} catch (e: ArithmeticException) {
    2
}

const val d1 = try {
    throwExample(10, 0)
    "Without exception"
} catch (e: ArithmeticException) {
    e.message ?: "Exception without message"
}
const val d2 = try {
    throwExample(10, 1)
    "Without exception"
} catch (e: ArithmeticException) {
    e.message ?: "Exception without message"
}

const val nullMessage = try {
    throwNullMessage(10, 0)
    "Without exception"
} catch (e: ArithmeticException) {
    e.message ?: "Exception without message"
}
// !LANGUAGE: +CompileTimeCalculations

const val a = try {
    10 / 0
    0
} catch (e: RuntimeException) {
    1
}

const val b = 10 / 0

fun someFunWithCompileTimeInside() {
    val exceptionExpected = 1 / 0
}
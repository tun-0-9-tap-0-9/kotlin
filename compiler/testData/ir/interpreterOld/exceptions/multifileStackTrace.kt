// !LANGUAGE: +CompileTimeCalculations

// FILE: main.kt

@CompileTimeCalculation
fun callToOtherFile(mustThrowException: Boolean, message: String): Int {
    if (mustThrowException) throwException(message)
    return 0
}

const val a = callToOtherFile(true, "Exception from file1.kt")

// FILE: file1.kt

@CompileTimeCalculation
fun throwException(message: String) {
    throw Exception(message)
}

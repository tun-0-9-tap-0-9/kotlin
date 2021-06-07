// !LANGUAGE: +CompileTimeCalculations
// FILE: logIntrinsic.kt

import kotlin.experimental.*

const val thisFileInfo = sourceLocation()
const val otherFileInfo = getSomeInfo()

@CompileTimeCalculation
fun sumWithLog(a: Int, b: Int): String {
    val firstWord = log("Function start")
    val before = log("Start summation of $a and $b")
    val after = log("Result of summation is ${a + b}")
    val finalWord = log("Function end", "<WITHOUT FILE>")

    return "\n" + firstWord + "\n" + before + "\n" + after + "\n" + finalWord
}

const val sum = sumWithLog(2, 5)

// FILE: other.kt

import kotlin.experimental.*

@CompileTimeCalculation
fun getSomeInfo(): String {
    return sourceLocation()
}

@CompileTimeCalculation
inline fun log(info: String, fileNameAndLine: String = sourceLocation()): String {
    return info + " at " + fileNameAndLine
}
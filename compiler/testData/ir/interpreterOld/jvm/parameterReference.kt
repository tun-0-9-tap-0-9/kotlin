// !LANGUAGE: +CompileTimeCalculations

package test

import kotlin.reflect.KFunction

@CompileTimeCalculation
fun withParameters(a: Int, b: Double) = 0
@CompileTimeCalculation
fun String.withExtension(a: Int) = 0

@CompileTimeCalculation
class A {
    fun String.get(a: Int) = this
}

const val parameters1 = (::withParameters as KFunction<*>).parameters.joinToString()
const val parameters2 = (String::withExtension as KFunction<*>).parameters.joinToString()
const val parameters3 = A::class.members.toList()[0].parameters.joinToString()

// properties
@CompileTimeCalculation
class B(val b: Int) {
    val String.size: Int
        get() = this.length
}

const val property0Parameters = B(1)::b.parameters.toString()
const val property1Parameters = B::b.parameters.toString()
const val property2Parameters = B::class.members.toList()[1].parameters.toString()

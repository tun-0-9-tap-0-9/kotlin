// !LANGUAGE: +CompileTimeCalculations

package test

import kotlin.reflect.KFunction

@CompileTimeCalculation
class A(val a: Int, val b: String) {
    val String.propertyWithExtension: Int
        get() = this.length * a

    fun Int.funWithExtension(other: Int) = this + other
}

const val aSimpleName = A::class.simpleName!!
const val aQualifiedName = A::class.qualifiedName!!
const val aMembers = A::class.members.joinToString()
const val aConstructors = A::class.constructors.joinToString()
const val aVisibility = A::class.visibility.toString()
const val aSupertypes = A::class.supertypes.joinToString()

@CompileTimeCalculation
interface Base<T>

@CompileTimeCalculation
class B<T, E : T, D : Any>(val prop: T) : Base<T> {
    fun get(): T = prop

    fun getThis(): B<T, out E, in D> = this

    fun <E : Number> withTypeParameter(num: E) = num.toString()
}

const val bMembers = B::class.members.joinToString()
const val bTypeParameters = B::class.typeParameters.joinToString()
const val bSupertypes = B::class.supertypes.joinToString()
const val bReturnType1 = B::class.members.toList()[1].returnType.toString()
const val bReturnType2 = B::class.members.toList()[3].returnType.toString()

const val arguments1 = (B<Number, Double, Int>(1)::getThis as KFunction<*>).returnType.arguments.joinToString()
const val arguments2 = (arrayOf(1)::iterator as KFunction<*>).returnType.arguments.joinToString()

@CompileTimeCalculation
class C {
    val String.getLength
        get() = this.length
}

const val cMember = C::class.members.toList()[0].toString()
const val cMemberReturnType = C::class.members.toList()[0].returnType.classifier.toString()
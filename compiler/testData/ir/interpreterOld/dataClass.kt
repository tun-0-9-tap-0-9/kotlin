// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
data class Person(val name: String, val phone: Int)

@CompileTimeCalculation
fun Person.getAsString(): String {
    val (name, phone) = this
    return "Persom name is $name and his phone is $phone"
}

const val a1 = Person("John", 123456).name
const val a2 = Person("John", 123456).component1()
const val a3 = Person("John", 123456).phone
const val a4 = Person("John", 123456).component2()

const val b1 = Person("John", 789).copy("Adam").toString()
const val b2 = Person("John", 789).copy("Adam", 123).toString()

const val c = Person("John", 123456).equals(Person("John", 123456))
const val d = Person("John", 123456).getAsString()

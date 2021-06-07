// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
class Person(val name: String, val surname: String) {
    var age: Int
    val wholeName: String

    init {
        wholeName = name + " " + surname
    }

    init {
        age = -1
    }

    constructor(name: String) : this(name, "<NULL>") {}

    constructor() : this("<NOT_GIVEN>") {}

    constructor(name: String, age: Int): this(name) {
        this.age = age
    }
}

const val a1 = Person("Ivan", "Ivanov").age
const val a2 = Person("Ivan", "Ivanov").wholeName

const val b1 = Person("Ivan").age
const val b2 = Person("Ivan").wholeName

const val c1 = Person().age
const val c2 = Person().wholeName

const val d1 = Person("Ivan", 20).age
const val d2 = Person("Ivan", 20).wholeName
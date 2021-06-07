// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
enum class EnumClass {
    VALUE1, VALUE2
}

const val a = EnumClass.VALUE1.name
const val b = EnumClass.values().size
const val c = EnumClass.valueOf("VALUE1").ordinal
const val d = EnumClass.valueOf("VALUE3").ordinal

const val e1 = EnumClass.VALUE1.hashCode().let { it is Int && it > 0 && it == EnumClass.VALUE1.hashCode() }
const val e2 = EnumClass.VALUE1.toString()
const val e3 = EnumClass.VALUE1 == EnumClass.VALUE1
const val e4 = EnumClass.VALUE1 == EnumClass.VALUE2

const val f1 = enumValues<EnumClass>().size
const val f2 = enumValueOf<EnumClass>("VALUE1").name

const val j1 = enumValues<EnumClass>().joinToString { it.name }

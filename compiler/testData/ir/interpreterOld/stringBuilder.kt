// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
fun appendVararg(vararg strings: String): String {
    val sb = StringBuilder()
    for (string in strings) {
        sb.append(string)
    }
    return sb.toString()
}


const val simpleAppend = StringBuilder().append("str").toString()
const val withCapacity = StringBuilder(7).append("example").toString()
const val withContent = StringBuilder("first").append(" ").append("second").toString()
const val appendInFun = appendVararg("1", " ", "2", " ", "3")

const val length1 = StringBuilder(3).append("1").length
const val length2 = StringBuilder().append("123456789").length
const val get0 = StringBuilder().append("1234556789").get(0)
const val get1 = StringBuilder().append("1234556789").get(1)
const val subSequence1 = StringBuilder().append("123456789").subSequence(0, 2) as String
const val subSequence2 = StringBuilder().append("123456789").subSequence(2, 8) as String

const val appendPart = StringBuilder().append("123456789", 1, 3).toString()
const val appendNull = StringBuilder().append(null as Any?).toString()
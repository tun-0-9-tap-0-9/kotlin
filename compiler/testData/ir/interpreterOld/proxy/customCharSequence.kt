// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
class MyCharSequence(val str: String): CharSequence {
    override val length: Int = str.length

    override fun get(index: Int) = str[index]

    override fun subSequence(startIndex: Int, endIndex: Int) = str.subSequence(startIndex, endIndex)
}

const val sbSize = StringBuilder(MyCharSequence("MyString")).length
const val appendSize = StringBuilder().append(MyCharSequence("MyString")).length
const val subSequenceSize = StringBuilder(StringBuilder(MyCharSequence("MyString")).subSequence(0, 4)).length

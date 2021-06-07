// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
fun compareTo(first: Char, second: Char) = first.compareTo(second)

@CompileTimeCalculation
fun plus(first: Char, second: Int) = first.plus(second)

@CompileTimeCalculation
fun minus(first: Char, second: Char) = first.minus(second)
@CompileTimeCalculation
fun minus(first: Char, second: Int) = first.minus(second)

@CompileTimeCalculation
fun inc(first: Char) = first.inc()
@CompileTimeCalculation
fun dec(first: Char) = first.dec()

@CompileTimeCalculation
fun rangeTo(first: Char, second: Char) = first.rangeTo(second)

@CompileTimeCalculation
fun toByte(first: Char) = first.toByte()
@CompileTimeCalculation
fun toChar(first: Char) = first.toChar()
@CompileTimeCalculation
fun toShort(first: Char) = first.toShort()
@CompileTimeCalculation
fun toInt(first: Char) = first.toInt()
@CompileTimeCalculation
fun toLong(first: Char) = first.toLong()
@CompileTimeCalculation
fun toFloat(first: Char) = first.toFloat()
@CompileTimeCalculation
fun toDouble(first: Char) = first.toDouble()

@CompileTimeCalculation
fun toString(first: Char) = first.toString()
@CompileTimeCalculation
fun hashCode(first: Char) = first.hashCode()
@CompileTimeCalculation
fun equals(first: Char, second: Char) = first.equals(second)

const val a1 = compareTo('a', 'b')
const val a2 = plus('1', 2)
const val a3 = minus('9', '1')
const val a4 = minus('9', 1)
const val a5 = inc('1')
const val a6 = dec('1')
const val a7 = rangeTo('9', '1').last

const val b1 = toByte('1')
const val b2 = toChar('2')
const val b3 = toShort('3')
const val b4 = toInt('4')
const val b5 = toLong('5')
const val b6 = toFloat('6')
const val b7 = toDouble('7')

const val c1 = toString('q')
const val c2 = hashCode('q')
const val c3 = equals('1', '2')
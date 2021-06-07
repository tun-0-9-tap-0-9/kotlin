// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation fun compareTo(value: Int, other: Byte) = value.compareTo(other)
@CompileTimeCalculation fun compareTo(value: Int, other: Short) = value.compareTo(other)
@CompileTimeCalculation fun compareTo(value: Int, other: Int) = value.compareTo(other)
@CompileTimeCalculation fun compareTo(value: Int, other: Long) = value.compareTo(other)
@CompileTimeCalculation fun compareTo(value: Int, other: Float) = value.compareTo(other)
@CompileTimeCalculation fun compareTo(value: Int, other: Double) = value.compareTo(other)

@CompileTimeCalculation fun plus(value: Int, other: Byte) = value.plus(other)
@CompileTimeCalculation fun plus(value: Int, other: Short) = value.plus(other)
@CompileTimeCalculation fun plus(value: Int, other: Int) = value.plus(other)
@CompileTimeCalculation fun plus(value: Int, other: Long) = value.plus(other)
@CompileTimeCalculation fun plus(value: Int, other: Float) = value.plus(other)
@CompileTimeCalculation fun plus(value: Int, other: Double) = value.plus(other)

@CompileTimeCalculation fun minus(value: Int, other: Byte) = value.minus(other)
@CompileTimeCalculation fun minus(value: Int, other: Short) = value.minus(other)
@CompileTimeCalculation fun minus(value: Int, other: Int) = value.minus(other)
@CompileTimeCalculation fun minus(value: Int, other: Long) = value.minus(other)
@CompileTimeCalculation fun minus(value: Int, other: Float) = value.minus(other)
@CompileTimeCalculation fun minus(value: Int, other: Double) = value.minus(other)

@CompileTimeCalculation fun times(value: Int, other: Byte) = value.times(other)
@CompileTimeCalculation fun times(value: Int, other: Short) = value.times(other)
@CompileTimeCalculation fun times(value: Int, other: Int) = value.times(other)
@CompileTimeCalculation fun times(value: Int, other: Long) = value.times(other)
@CompileTimeCalculation fun times(value: Int, other: Float) = value.times(other)
@CompileTimeCalculation fun times(value: Int, other: Double) = value.times(other)

@CompileTimeCalculation fun div(value: Int, other: Byte) = value.div(other)
@CompileTimeCalculation fun div(value: Int, other: Short) = value.div(other)
@CompileTimeCalculation fun div(value: Int, other: Int) = value.div(other)
@CompileTimeCalculation fun div(value: Int, other: Long) = value.div(other)
@CompileTimeCalculation fun div(value: Int, other: Float) = value.div(other)
@CompileTimeCalculation fun div(value: Int, other: Double) = value.div(other)

@CompileTimeCalculation fun rem(value: Int, other: Byte) = value.rem(other)
@CompileTimeCalculation fun rem(value: Int, other: Short) = value.rem(other)
@CompileTimeCalculation fun rem(value: Int, other: Int) = value.rem(other)
@CompileTimeCalculation fun rem(value: Int, other: Long) = value.rem(other)
@CompileTimeCalculation fun rem(value: Int, other: Float) = value.rem(other)
@CompileTimeCalculation fun rem(value: Int, other: Double) = value.rem(other)

@CompileTimeCalculation fun inc(value: Int) = value.inc()
@CompileTimeCalculation fun dec(value: Int) = value.dec()

@CompileTimeCalculation fun unaryPlus(value: Int) = value.unaryPlus()
@CompileTimeCalculation fun unaryMinus(value: Int) = value.unaryMinus()

@CompileTimeCalculation fun rangeTo(value: Int, other: Byte) = value.rangeTo(other)
@CompileTimeCalculation fun rangeTo(value: Int, other: Short) = value.rangeTo(other)
@CompileTimeCalculation fun rangeTo(value: Int, other: Int) = value.rangeTo(other)
@CompileTimeCalculation fun rangeTo(value: Int, other: Long) = value.rangeTo(other)

@CompileTimeCalculation fun shl(value: Int, bitCount: Int) = value.shl(bitCount)
@CompileTimeCalculation fun shr(value: Int, bitCount: Int) = value.shr(bitCount)
@CompileTimeCalculation fun ushr(value: Int, bitCount: Int) = value.ushr(bitCount)

@CompileTimeCalculation fun and(value: Int, other: Int) = value.and(other)
@CompileTimeCalculation fun or(value: Int, other: Int) = value.or(other)
@CompileTimeCalculation fun xor(value: Int, other: Int) = value.xor(other)
@CompileTimeCalculation fun inv(value: Int) = value.inv()

@CompileTimeCalculation fun toByte(value: Int) = value.toByte()
@CompileTimeCalculation fun toChar(value: Int) = value.toChar()
@CompileTimeCalculation fun toShort(value: Int) = value.toShort()
@CompileTimeCalculation fun toInt(value: Int) = value.toInt()
@CompileTimeCalculation fun toLong(value: Int) = value.toLong()
@CompileTimeCalculation fun toFloat(value: Int) = value.toFloat()
@CompileTimeCalculation fun toDouble(value: Int) = value.toDouble()

@CompileTimeCalculation fun toString(value: Int) = value.toString()
@CompileTimeCalculation fun hashCode(value: Int) = value.hashCode()
@CompileTimeCalculation fun equals(value: Int, other: Int) = value.equals(other)

@CompileTimeCalculation fun echo(value: Int) = value

const val min = echo(Int.MIN_VALUE)
const val max = echo(Int.MAX_VALUE)
const val bytes = echo(Int.SIZE_BYTES)
const val bits = echo(Int.SIZE_BITS)

const val compare1 = compareTo(5, 1.toByte())
const val compare2 = compareTo(5, 2.toShort())
const val compare3 = compareTo(5, 3)
const val compare4 = compareTo(5, 4L)
const val compare5 = compareTo(5, 5.toFloat())
const val compare6 = compareTo(5, 6.toDouble())

const val plus1 = plus(5, 1.toByte())
const val plus2 = plus(5, 2.toShort())
const val plus3 = plus(5, 3)
const val plus4 = plus(5, 4L)
const val plus5 = plus(5, 5.toFloat())
const val plus6 = plus(5, 6.toDouble())

const val minus1 = minus(5, 1.toByte())
const val minus2 = minus(5, 2.toShort())
const val minus3 = minus(5, 3)
const val minus4 = minus(5, 4L)
const val minus5 = minus(5, 5.toFloat())
const val minus6 = minus(5, 6.toDouble())

const val times1 = times(5, 1.toByte())
const val times2 = times(5, 2.toShort())
const val times3 = times(5, 3)
const val times4 = times(5, 4L)
const val times5 = times(5, 5.toFloat())
const val times6 = times(5, 6.toDouble())

const val div1 = div(100, 1.toByte())
const val div2 = div(100, 2.toShort())
const val div3 = div(100, 4)
const val div4 = div(100, 10L)
const val div5 = div(100, 25.toFloat())
const val div6 = div(100, 50.toDouble())

const val rem1 = rem(5, 1.toByte())
const val rem2 = rem(5, 2.toShort())
const val rem3 = rem(5, 3)
const val rem4 = rem(5, 4L)
const val rem5 = rem(5, 5.toFloat())
const val rem6 = rem(5, 6.toDouble())

const val increment = inc(3)
const val decrement = dec(3)

const val unaryPlus = unaryPlus(3)
const val unaryMinus = unaryMinus(3)

const val rangeTo1 = rangeTo(5, 1.toByte()).last
const val rangeTo2 = rangeTo(5, 2.toShort()).last
const val rangeTo3 = rangeTo(5, 3).last
const val rangeTo4 = rangeTo(5, 4L).last

const val shiftLeft = shl(8, 1)
const val shiftRight = shr(8, 2)
const val unsignedShiftRight = ushr(-8, 3)

const val and = and(8, 1)
const val or = or(8, 2)
const val xor = xor(-8, 3)
const val inv = inv(8)

const val a1 = toByte(1)
const val a2 = toChar(2)
const val a3 = toShort(3)
const val a4 = toInt(4)
const val a5 = toLong(5)
const val a6 = toFloat(6)
const val a7 = toDouble(7)

const val b1 = toString(10)
const val b2 = hashCode(10)
const val b3 = equals(10, 11)
const val b4 = equals(1, 1.toInt())
const val b5 = equals(1, 1)
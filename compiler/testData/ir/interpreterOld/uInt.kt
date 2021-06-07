// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation fun compareTo(value: UInt, other: UByte) = value.compareTo(other)
@CompileTimeCalculation fun compareTo(value: UInt, other: UShort) = value.compareTo(other)
@CompileTimeCalculation fun compareTo(value: UInt, other: UInt) = value.compareTo(other)
@CompileTimeCalculation fun compareTo(value: UInt, other: ULong) = value.compareTo(other)

@CompileTimeCalculation fun plus(value: UInt, other: UByte) = value.plus(other)
@CompileTimeCalculation fun plus(value: UInt, other: UShort) = value.plus(other)
@CompileTimeCalculation fun plus(value: UInt, other: UInt) = value.plus(other)
@CompileTimeCalculation fun plus(value: UInt, other: ULong) = value.plus(other)

@CompileTimeCalculation fun minus(value: UInt, other: UByte) = value.minus(other)
@CompileTimeCalculation fun minus(value: UInt, other: UShort) = value.minus(other)
@CompileTimeCalculation fun minus(value: UInt, other: UInt) = value.minus(other)
@CompileTimeCalculation fun minus(value: UInt, other: ULong) = value.minus(other)

@CompileTimeCalculation fun times(value: UInt, other: UByte) = value.times(other)
@CompileTimeCalculation fun times(value: UInt, other: UShort) = value.times(other)
@CompileTimeCalculation fun times(value: UInt, other: UInt) = value.times(other)
@CompileTimeCalculation fun times(value: UInt, other: ULong) = value.times(other)

@CompileTimeCalculation fun div(value: UInt, other: UByte) = value.div(other)
@CompileTimeCalculation fun div(value: UInt, other: UShort) = value.div(other)
@CompileTimeCalculation fun div(value: UInt, other: UInt) = value.div(other)
@CompileTimeCalculation fun div(value: UInt, other: ULong) = value.div(other)

@CompileTimeCalculation fun rem(value: UInt, other: UByte) = value.rem(other)
@CompileTimeCalculation fun rem(value: UInt, other: UShort) = value.rem(other)
@CompileTimeCalculation fun rem(value: UInt, other: UInt) = value.rem(other)
@CompileTimeCalculation fun rem(value: UInt, other: ULong) = value.rem(other)

@CompileTimeCalculation fun inc(value: UInt) = value.inc()
@CompileTimeCalculation fun dec(value: UInt) = value.dec()

@CompileTimeCalculation fun rangeTo(value: UInt, other: UInt) = value.rangeTo(other)

@CompileTimeCalculation fun shl(value: UInt, bitCount: Int) = value.shl(bitCount)
@CompileTimeCalculation fun shr(value: UInt, bitCount: Int) = value.shr(bitCount)
@CompileTimeCalculation fun and(value: UInt, other: UInt) = value.and(other)
@CompileTimeCalculation fun or(value: UInt, other: UInt) = value.or(other)
@CompileTimeCalculation fun xor(value: UInt, other: UInt) = value.xor(other)
@CompileTimeCalculation fun inv(value: UInt) = value.inv()

@CompileTimeCalculation fun toByte(value: UInt) = value.toByte()
@CompileTimeCalculation fun toShort(value: UInt) = value.toShort()
@CompileTimeCalculation fun toInt(value: UInt) = value.toInt()
@CompileTimeCalculation fun toLong(value: UInt) = value.toLong()
@CompileTimeCalculation fun toUByte(value: UInt) = value.toUByte()
@CompileTimeCalculation fun toUShort(value: UInt) = value.toUShort()
@CompileTimeCalculation fun toUInt(value: UInt) = value.toUInt()
@CompileTimeCalculation fun toULong(value: UInt) = value.toULong()
@CompileTimeCalculation fun toFloat(value: UInt) = value.toFloat()
@CompileTimeCalculation fun toDouble(value: UInt) = value.toDouble()

@CompileTimeCalculation fun toString(value: UInt) = value.toString()
@CompileTimeCalculation fun hashCode(value: UInt) = value.hashCode()
@CompileTimeCalculation fun equals(value: UInt, other: Any) = value.equals(other)

@CompileTimeCalculation fun echo(value: Any) = value

const val min = echo(UInt.MIN_VALUE) as UInt
const val max = echo(UInt.MAX_VALUE) as UInt
const val bytes = echo(UInt.SIZE_BYTES) as Int
const val bits = echo(UInt.SIZE_BITS) as Int

const val uByte: UByte = 0u
const val uByteNonZero: UByte = 1u
const val uShort: UShort = 1u
const val uInt: UInt = 2u
const val uLong: ULong = 3uL

const val compare1 = compareTo(2u, uByte)
const val compare2 = compareTo(2u, uShort)
const val compare3 = compareTo(2u, uInt)
const val compare4 = compareTo(2u, uLong)

const val plus1 = plus(2u, uByte)
const val plus2 = plus(2u, uShort)
const val plus3 = plus(2u, uInt)
const val plus4 = plus(2u, uLong)

const val minus1 = minus(2u, uByte)
const val minus2 = minus(2u, uShort)
const val minus3 = minus(2u, uInt)
const val minus4 = minus(2u, uLong)

const val times1 = times(2u, uByte)
const val times2 = times(2u, uShort)
const val times3 = times(2u, uInt)
const val times4 = times(2u, uLong)

const val div1 = div(2u, uByteNonZero)
const val div2 = div(2u, uShort)
const val div3 = div(2u, uInt)
const val div4 = div(2u, uLong)

const val rem1 = rem(2u, uByteNonZero)
const val rem2 = rem(2u, uShort)
const val rem3 = rem(2u, uInt)
const val rem4 = rem(2u, uLong)

const val inc = inc(3u)
const val dec = dec(3u)

const val rangeTo = rangeTo(0u, 10u).last

const val shiftLeft = shl(8u, 1)
const val shiftRight = shr(8u, 2)

const val and = and(8u, 1u)
const val or = or(8u, 2u)
const val xor = xor(8u, 3u)
const val inv = inv(8u)

const val a1 = toByte(1u)
const val a2 = toShort(2u)
const val a3 = toInt(3u)
const val a4 = toLong(4u)
const val a5 = toUByte(5u)
const val a6 = toUShort(6u)
const val a7 = toUInt(7u)
const val a8 = toULong(8u)
const val a9 = toFloat(9u)
const val a10 = toDouble(10u)

const val b1 = toString(10u)
const val b2 = hashCode(10u)
const val b3 = equals(10u, 11u)
const val b4 = equals(1u, 1)
const val b5 = equals(3u, 3uL)
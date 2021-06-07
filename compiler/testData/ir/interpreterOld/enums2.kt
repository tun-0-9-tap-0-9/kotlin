// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
enum class Empty

@CompileTimeCalculation
enum class Color(val rgb: Int) {
    BLACK() { override fun getColorAsString() = "0x000000" },
    RED(0xFF0000) { override fun getColorAsString() = "0xFF0000" },
    GREEN(0x00FF00) { override fun getColorAsString() = "0x00FF00" },
    BLUE(0x0000FF) { override fun getColorAsString() = "0x0000FF" };

    constructor() : this(0x000000) {}

    abstract fun getColorAsString(): String

    fun getColorAsInt(): Int = rgb
}

const val a1 = Empty.values().size
const val a2 = enumValues<Empty>().size

const val b1 = Color.BLACK.name
const val b2 = Color.BLACK.getColorAsString()
const val b3 = Color.RED.getColorAsString()

const val c1 = Color.BLACK.getColorAsInt()
const val c2 = Color.RED.getColorAsInt()
// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
class Outer {
    private val bar: String = "bar"
    val num = 1

    fun foo() = "outer foo"

    inner class Middle {
        val num = 2

        fun foo() = "middle foo"

        inner class Inner {
            val num = 3

            fun foo() = "inner foo with outer bar = \"$bar\""

            fun getAllNums() = "From inner: " + this.num + "; from middle: " + this@Middle.num + "; from outer: " + this@Outer.num
        }
    }
}

val a1 = Outer().foo()
val a2 = Outer().Middle().foo()
val a3 = Outer().Middle().Inner().foo()

val b = Outer().Middle().Inner().getAllNums()

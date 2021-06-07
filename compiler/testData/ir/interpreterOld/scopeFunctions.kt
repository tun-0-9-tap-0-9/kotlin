// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
class A(var num: Int, var str: String) {
    fun setNewStr(newString: String) {
        this.str = newString
    }
}

@CompileTimeCalculation
fun <T> echo(value: T): T = value

const val a = run { echo("Run block") }

const val b = A(0, "Run with receiver").run { this.str + this.num }

const val c = with (A(1, "String")) {
    setNewStr("New String")
    this.str
}

const val d = A(2, "Apply test").apply { this.setNewStr("New apply str") }.str

const val e = mutableListOf("one", "two", "three").also { it.add("four") }.size
const val f1 = mutableListOf("one", "two", "three").let {
    it.add("four")
    it.size
}
const val f2 = 10.let { it + 10 }

val g1 = 1.takeIf { it % 2 == 0 }
val g2 = 2.takeIf { it % 2 == 0 }
val h1 = (-1).takeUnless { it > 0 }
val h2 = 1.takeUnless { it > 0 }
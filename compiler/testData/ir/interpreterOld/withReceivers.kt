// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
class Temp<T : Number>(val a: T) {
    fun <E : Number> plus(temp: Temp<E>): Double {
        return with(temp) {
            return@with this.a.toDouble() + this@Temp.a.toDouble()
        }
    }
}

@CompileTimeCalculation
fun <T : Number> plus(a: Temp<T>, b: Temp<T>): Double {
    return with(a) w1@{
        return@w1 with(b) w2@{
            return@w2 this@w1.a.toDouble() + this@w2.a.toDouble()
        }
    }
}

val a1 = Temp<Int>(1).plus(Temp<Double>(2.0))
val a2 = plus(Temp<Int>(3), Temp<Int>(4))
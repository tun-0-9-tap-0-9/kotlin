// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
fun withVararg(vararg nums: Int): String {
    var sum = 0
    for (num in nums) {
        sum += num
    }
    return "size = " + nums.size + "; sum = " + sum
}

@CompileTimeCalculation
fun <T> withVarargUnknownType(vararg args: T): String {
    var str = ""
    for (arg in args) {
        str += arg.toString() + " "
    }
    return "size = " + args.size + "; join = " + str
}

@CompileTimeCalculation
fun withVarargClassType(vararg args: A): String {
    var str = ""
    for (arg in args) {
        str += arg.i.toString() + " "
    }
    return "size = " + args.size + "; join = " + str
}

@CompileTimeCalculation
class A(val i: Int)

const val a = withVararg(1, 2, 3)
const val b = withVarargUnknownType(1, 2.0, "a")
const val c = withVarargClassType(A(1), A(2), A(3))

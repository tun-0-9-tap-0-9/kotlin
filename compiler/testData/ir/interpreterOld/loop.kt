// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
fun loop(toInc: Int, start: Int, end: Int): Int {
    var result = toInc
    for (i in start..end) {
        result += 1
    }
    return result
}

@CompileTimeCalculation
fun withInnerContinue(): Int {
    var cycles = 0
    var i = 1
    var j = 1
    L@while (i <= 5) {
        j = 1
        while (j <= 5) {
            if (i % 2 == 0) {
                i += 1
                continue@L
            }
            cycles += 1
            j += 1
        }
        i += 1
    }

    return cycles
}

@CompileTimeCalculation
fun withBreak(limit: Int): Int {
    var x = 0
    while(true) {
        if(x < limit) x++ else break
    }
    return x
}

@CompileTimeCalculation
fun earlyExit(end: Int, stop: Int): Int {
    for (i in 1..end) {
        if (i == stop) return i
    }
    return end
}

const val a = loop(0, 1, 10)
const val b1 = withInnerContinue()
const val b2 = withBreak(10)
const val c1 = earlyExit(10, 5)
const val c2 = earlyExit(10, 15)
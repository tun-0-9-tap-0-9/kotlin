// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
fun infinityWhile(): Int {
    while (true) {}
    return 0
}

const val a = infinityWhile()
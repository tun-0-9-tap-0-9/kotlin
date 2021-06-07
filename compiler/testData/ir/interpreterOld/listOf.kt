// !LANGUAGE: +CompileTimeCalculations

const val a = listOf(1, 2, 3).size
const val b = emptyList<Int>().size
const val c = listOf<Int>().hashCode()

@CompileTimeCalculation
fun getSum(list: List<Int>): Int {
    var sum: Int = 0
    for (element in list) {
        sum += element
    }
    return sum
}

const val sum = getSum(listOf(1, 3, 5, 7))

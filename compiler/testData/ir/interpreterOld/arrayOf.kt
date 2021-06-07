// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
class A(val value: Int)

@CompileTimeCalculation
fun changeAndReturnSum(intArray: IntArray, index: Int, newValue: Int): Int {
    intArray[index] = newValue
    var sum = 0
    for (i in intArray) sum += i
    return sum
}

@CompileTimeCalculation
fun changeAndReturnSumForObject(array: Array<A>, index: Int, newValue: A): Int {
    array[index] = newValue
    var sum = 0
    for (aObject in array) sum += aObject.value
    return sum
}

const val a = arrayOf(1, 2, 3).size
const val b = changeAndReturnSum(intArrayOf(1, 2, 3), 0, 10)
const val c = emptyArray<Int>().size
const val d = changeAndReturnSumForObject(arrayOf(A(1), A(2), A(3)), 0, A(10))

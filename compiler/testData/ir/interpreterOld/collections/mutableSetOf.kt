// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
fun testAdd(mutableSet: MutableSet<Int>, newElem: Int): String {
    mutableSet.add(newElem)
    return "After add new size is " + mutableSet.size
}

@CompileTimeCalculation
fun <T> testRemove(mutableSet: MutableSet<T>, toRemove: T): String {
    mutableSet.remove(toRemove)
    return "After remove new size is " + mutableSet.size
}

@CompileTimeCalculation
fun testAddAll(mutableSet: MutableSet<Double>, toAdd: Set<Double>): String {
    mutableSet.addAll(toAdd)
    return "After addAll new size is " + mutableSet.size
}

@CompileTimeCalculation
fun testIterator(mutableSet: MutableSet<Byte>): String {
    var sum = 0
    for (byte in mutableSet) {
        sum += byte
    }
    return "Sum = " + sum
}

const val emptyMutableSetSize = mutableSetOf<Any>().size
const val mutableSetSize = mutableSetOf(1, 2, 3).size
const val mutableSetAdd = testAdd(mutableSetOf(1, 2, 3), 4)
const val mutableSetRemove1 = testRemove(mutableSetOf("1", "2", "3"), "1")
const val mutableSetRemove2 = testRemove(mutableSetOf("1", "2", "3"), "4")
const val mutableSetAddAll = testAddAll(mutableSetOf(1.0, 2.0, 3.0), setOf(4.333, -5.5))
const val mutableSetSum = testIterator(mutableSetOf<Byte>(1, (-2).toByte(), 127, 10, 0))

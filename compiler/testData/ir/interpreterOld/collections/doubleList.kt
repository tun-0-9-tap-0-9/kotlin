// !LANGUAGE: +CompileTimeCalculations

val doubleListSize = listOf(
    listOf("1", "2", "3"),
    listOf("4", "5", "6"),
    listOf("7", "8", "9")
).size

val doubleListSizeOfList = listOf(
    listOf("1"),
    listOf("4", "5"),
    listOf("7", "8", "9")
)[2].size

val doubleListGetSingleElement = listOf(
    listOf("1"),
    listOf("4", "5"),
    listOf("7", "8", "9")
)[2][2]

val doubleListElements = listOf(
    listOf("1"),
    listOf("4", "5"),
    listOf("7", "8", "9")
).joinToString(separator = "; ") { it.joinToString() }

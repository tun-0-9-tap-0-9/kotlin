// !LANGUAGE: +CompileTimeCalculations

const val a = listOf(1, 2, 3).elementAtOrElse(1) { -1 }
const val b = listOf(1, 2, 3).elementAtOrElse(4) { -1 }
const val c = uintArrayOf(1u, 2u, 3u, 4u).elementAtOrElse(2) { 0u }
const val d = "abcd".elementAtOrElse(2) { '0' }
const val e = "abcd".elementAtOrElse(4) { '0' }
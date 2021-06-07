// !LANGUAGE: +CompileTimeCalculations

const val a = mapOf(1 to "1", 2 to "2", 3 to "3").size
const val b = emptyMap<Any, Any>().size

const val contains1 = mapOf(1 to "1", 2 to "2", 3 to "3").containsKey(1)
const val contains2 = mapOf(1 to "1", 2 to "2", 3 to "3").contains(1)
const val contains3 = mapOf(1 to "1", 2 to "2", 3 to "3").contains<Any, String>("1")
const val contains4 = mapOf(1 to "1", 2 to "2", 3 to "3").containsValue("1")

const val get1 = mapOf(1 to "1", 2 to "2", 3 to "3").get(1)!!
const val get2 = mapOf(1 to "1", 2 to "2", 3 to "3")[2]!!
const val get3 = mapOf(1 to "1", 2 to "2", 3 to "3")[0].toString()

const val keys = mapOf(1 to "1", 2 to "2", 3 to "3").keys.size
const val values = mapOf(1 to "1", 2 to "2", 3 to "3").values.size
const val entries = mapOf(1 to "1", 2 to "2", 3 to "3").entries.size

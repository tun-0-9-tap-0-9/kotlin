// !LANGUAGE: +CompileTimeCalculations

const val regConstructor1 = Regex("pattern").pattern
const val regConstructor2 = Regex("pattern", RegexOption.IGNORE_CASE).options.iterator().next().name
const val regConstructor3 = Regex("pattern", setOf(RegexOption.IGNORE_CASE, RegexOption.MULTILINE)).options.size

val matchEntire1 = Regex("pat").matchEntire("pat")?.value
val matchEntire2 = Regex("[abc]").matchEntire("a")?.range?.last
const val matches1 = Regex("str(1)?").matches("str1")
const val matches2 = Regex("str(1)?").matches("str2")
const val containsMatchIn1 = Regex("[0-9]").containsMatchIn("0")
const val containsMatchIn2 = Regex("[0-9]").containsMatchIn("!!0!!")
const val containsMatchIn3 = Regex("[0-9]").containsMatchIn("!!p!!")

//replace
const val replace1 = Regex("0").replace("There are 0 apples", "n")
const val replace2 = Regex("(red|green|blue)").replace("Roses are red, Violets are blue") { it.value + "!" }
const val replace3 = Regex("(red|green|blue)").replaceFirst("Roses are red, Violets are blue", "REPLACED")
const val split = Regex("\\W+").split("Roses are red, Violets are blue").size

//find
val find1 = Regex("p").find("p")?.value
val find2 = Regex("(red|green|blue)").find("Roses are red, Violets are blue")?.groups?.size
val find3 = Regex("(red|green|blue)").find("Roses are red, Violets are blue")?.destructured?.component1()
val find4 = Regex("(red|green|blue)").find("Roses are red, Violets are blue")?.next()?.value
val find5 = Regex("(red|green|blue)").find("Roses are red, Violets are blue", 15)?.value
val find6 = Regex("(red|green|blue)").findAll("Roses are red, Violets are blue").iterator().next()?.value
val find7 = Regex("(red|green|blue)").findAll("Roses are red, Violets are blue").iterator().next()?.next()?.value
val find8 = Regex("(red|green|blue)").findAll("Roses are red, Violets are blue").iterator().next()?.next()?.next()?.value

//companion
const val fromLiteral = Regex.fromLiteral("[a-z0-9]+").pattern
const val escape = Regex.escape("[a-z0-9]+")
const val escapeReplacement = Regex.escapeReplacement("[a-z0-9]+")

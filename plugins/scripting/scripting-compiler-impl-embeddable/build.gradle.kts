import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar

plugins { java }

description = "Kotlin Compiler Infrastructure for Scripting for embeddable compiler"

val packedJars by configurations.creating
dependencies {
    packedJars(project(":kotlin-scripting-compiler-impl")) { isTransitive = false }
    packedJars(commonDep("org.jetbrains.kotlinx", "kotlinx-coroutines-core")) { isTransitive = false }
    packedJars(commonDep("org.jetbrains.kotlinx", "kotlinx-coroutines-core-jvm")) { isTransitive = false }
    runtimeOnly(kotlinStdlib())
}

publish()

noDefaultJar()
runtimeJar(rewriteDepsToShadedCompiler(
    tasks.register<ShadowJar>("shadowJar")  {
        from(packedJars)
    }
))
sourcesJar()
javadocJar()

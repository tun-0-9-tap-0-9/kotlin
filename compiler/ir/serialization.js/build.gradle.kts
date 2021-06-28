plugins {
    kotlin("jvm")
    id("jps-compatible")
}

dependencies {
    api(project(":compiler:ir.psi2ir"))
    api(project(":compiler:ir.serialization.common"))
    api(project(":js:js.frontend"))
    implementation(project(":compiler:ir.backend.common"))

    compileOnly(intellijCoreDep()) { includeJars("intellij-core") }
}

sourceSets {
    "main" { projectDefault() }
}

tasks {
    val compileKotlin by existing(org.jetbrains.kotlin.gradle.tasks.KotlinCompile::class) {
        kotlinOptions {
            freeCompilerArgs += "-Xopt-in=org.jetbrains.kotlin.ir.ObsoleteDescriptorBasedAPI"
        }
    }
}

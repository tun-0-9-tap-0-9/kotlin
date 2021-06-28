// !LANGUAGE: +MultiPlatformProjects
// MODULE: m1-common
// FILE: common.kt

expect class B {
    class N {
        <!EXPECTED_DECLARATION_WITH_BODY!>fun body()<!> {}
        expect fun extraHeader()
    }
}

expect class C {
    expect class N
    expect enum class E
    expect inner class I
}

expect class D {
    class N
}

expect class E {
    class N
}

// MODULE: m1-jvm()()(m1-common)
// FILE: jvm.kt

actual class B {
    actual class N {
        actual fun body() {}
        actual fun extraHeader() {}
    }
}

actual class C {
    actual class N
    actual enum class E
    actual inner class I
}

<!NO_ACTUAL_CLASS_MEMBER_FOR_EXPECTED_CLASS!>actual class D<!>

actual class E {
    class N
}

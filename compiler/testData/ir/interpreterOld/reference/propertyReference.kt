// !LANGUAGE: +CompileTimeCalculations

@CompileTimeCalculation
class A(val a: Int)

const val propertyName = A::a.name
const val propertyGet = A::a.get(A(1))
const val propertyInvoke = A::a.invoke(A(2))

const val propertyWithReceiverName = A(10)::a.name
const val propertyWithReceiverGet = A(11)::a.get()
const val propertyWithReceiverInvoke = A(12)::a.invoke()

@CompileTimeCalculation
class B(var b: Int)

const val mutablePropertyName = B::b.name
const val mutablePropertyGet = B::b.get(B(1))
const val mutablePropertySet = B(2).apply { B::b.set(this, 3) }.b
const val mutablePropertyInvoke = B::b.invoke(B(4))

const val mutablePropertyWithReceiverName = B(10)::b.name
const val mutablePropertyWithReceiverGet = B(11)::b.get()
const val mutablePropertyWithReceiverSet = B(12).apply { this::b.set(13) }.b
const val mutablePropertyWithReceiverInvoke = B(14)::b.invoke()
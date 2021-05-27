/*
 * Copyright 2010-2021 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.fir.backend

import org.jetbrains.kotlin.backend.common.ir.createImplicitParameterDeclarationWithWrappedDescriptor
import org.jetbrains.kotlin.builtins.PrimitiveType
import org.jetbrains.kotlin.builtins.StandardNames
import org.jetbrains.kotlin.builtins.UnsignedType
import org.jetbrains.kotlin.config.LanguageVersionSettings
import org.jetbrains.kotlin.descriptors.ClassKind
import org.jetbrains.kotlin.descriptors.DescriptorVisibilities
import org.jetbrains.kotlin.descriptors.DescriptorVisibility
import org.jetbrains.kotlin.descriptors.Modality
import org.jetbrains.kotlin.fir.descriptors.FirModuleDescriptor
import org.jetbrains.kotlin.fir.resolve.symbolProvider
import org.jetbrains.kotlin.fir.symbols.impl.FirClassSymbol
import org.jetbrains.kotlin.ir.BuiltInOperatorNames
import org.jetbrains.kotlin.ir.IrBuiltIns
import org.jetbrains.kotlin.ir.UNDEFINED_OFFSET
import org.jetbrains.kotlin.ir.builders.declarations.*
import org.jetbrains.kotlin.ir.declarations.*
import org.jetbrains.kotlin.ir.declarations.impl.IrExternalPackageFragmentImpl
import org.jetbrains.kotlin.ir.symbols.IrClassSymbol
import org.jetbrains.kotlin.ir.symbols.IrClassifierSymbol
import org.jetbrains.kotlin.ir.symbols.IrConstructorSymbol
import org.jetbrains.kotlin.ir.symbols.IrSimpleFunctionSymbol
import org.jetbrains.kotlin.ir.symbols.impl.IrClassPublicSymbolImpl
import org.jetbrains.kotlin.ir.symbols.impl.IrConstructorPublicSymbolImpl
import org.jetbrains.kotlin.ir.symbols.impl.IrSimpleFunctionPublicSymbolImpl
import org.jetbrains.kotlin.ir.symbols.impl.IrTypeParameterSymbolImpl
import org.jetbrains.kotlin.ir.types.*
import org.jetbrains.kotlin.ir.types.impl.IrSimpleTypeImpl
import org.jetbrains.kotlin.ir.util.*
import org.jetbrains.kotlin.name.ClassId
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.types.Variance
import org.jetbrains.kotlin.types.checker.KotlinTypeChecker
import org.jetbrains.kotlin.util.OperatorNameConventions

class IrBuiltInsOverFir(
    private val components: Fir2IrComponents,
    override val languageVersionSettings: LanguageVersionSettings,
    private val moduleDescriptor: FirModuleDescriptor
) : IrBuiltIns() {

    override val irFactory: IrFactory = components.symbolTable.irFactory

    private val kotlinPackage = StandardNames.BUILT_INS_PACKAGE_FQ_NAME
    private val kotlinReflectPackage = StandardNames.KOTLIN_REFLECT_FQ_NAME

    //    private val javaLangPackage = FqName("java.lang")
    private val kotlinCollectionsPackage = StandardNames.COLLECTIONS_PACKAGE_FQ_NAME

    private val internalIrPackage = createPackage(KOTLIN_INTERNAL_IR_FQN)
    private val kotlinIrPackage = createPackage(kotlinPackage)
    private val kotlinCollectionsIrPackage = createPackage(kotlinCollectionsPackage)

    override lateinit var booleanNotSymbol: IrSimpleFunctionSymbol private set

    override val booleanType: IrType get() = booleanClass.defaultType
    override val booleanClass: IrClassSymbol = //referenceClassByFqname(kotlinPackage, "Boolean")!!
        createClass(StandardNames.getPrimitiveFqName(PrimitiveType.BOOLEAN), kotlinIrPackage).also { boolClass ->
            booleanNotSymbol = createFunction(kotlinPackage, "not", boolClass.defaultType, emptyArray(), boolClass.owner).also { fn ->
                boolClass.owner.declarations.add(fn)
                fn.addDispatchReceiver {
                    type = boolClass.defaultType
                    origin = boolClass.owner.origin
                }
            }.symbol
        }

    override val charType: IrType get() = charClass.defaultType
    override val charClass: IrClassSymbol = kotlinIrPackage.createClass("Char")

    override val byteType: IrType get() = byteClass.defaultType
    override val byteClass: IrClassSymbol = kotlinIrPackage.createNumberClass("Byte")
    override val shortType: IrType get() = shortClass.defaultType
    override val shortClass: IrClassSymbol = kotlinIrPackage.createNumberClass("Short")
    override val intType: IrType get() = intClass.defaultType
    override val intClass: IrClassSymbol = kotlinIrPackage.createNumberClass("Int")
    override val longType: IrType get() = longClass.defaultType
    override val longClass: IrClassSymbol = kotlinIrPackage.createNumberClass("Long")
    override val floatType: IrType get() = floatClass.defaultType
    override val floatClass: IrClassSymbol = kotlinIrPackage.createNumberClass("Float")
    override val doubleType: IrType get() = doubleClass.defaultType
    override val doubleClass: IrClassSymbol = kotlinIrPackage.createNumberClass("Double")

    override val stringClass: IrClassSymbol = kotlinIrPackage.createClass("String")
    override val stringType: IrType get() = stringClass.defaultType

    override val anyClass: IrClassSymbol = kotlinIrPackage.createClass("Any") {
        createConstructor()
        createMemberFunction("equals", booleanType, defaultType.withHasQuestionMark(true))
        createMemberFunction("hashCode", intType)
        createMemberFunction("toString", stringType)
    }
    override val anyType: IrType = anyClass.defaultType
    override val anyNType = anyType.withHasQuestionMark(true)

    override val unitClass: IrClassSymbol = kotlinIrPackage.createClass("Unit")
    override val unitType: IrType get() = unitClass.defaultType

    override val arrayClass: IrClassSymbol = kotlinIrPackage.createClass("Array") klass@ {
        val typeParameter = addTypeParameter("T", anyNType)
        addArrayMembers(typeParameter.defaultType)
    }

    override val numberClass: IrClassSymbol = kotlinIrPackage.createClass("Number")
    override val numberType: IrType get() = numberClass.defaultType

    override val nothingClass: IrClassSymbol = kotlinIrPackage.createClass("Nothing")
    override val nothingType: IrType get() = nothingClass.defaultType
    override val nothingNType: IrType = nothingType.withHasQuestionMark(true)

    override val charSequenceClass: IrClassSymbol = referenceClassByFqname(kotlinPackage, "CharSequence")!!

    override val collectionClass: IrClassSymbol by lazy { referenceClassByFqname(kotlinCollectionsPackage, "Collection")!! }
    override val setClass: IrClassSymbol by lazy { referenceClassByFqname(kotlinCollectionsPackage, "Set")!! }
    override val listClass: IrClassSymbol by lazy { referenceClassByFqname(kotlinCollectionsPackage, "List")!! }
    override val mapClass: IrClassSymbol by lazy { referenceClassByFqname(kotlinCollectionsPackage, "Map")!! }
    override val mapEntryClass: IrClassSymbol by lazy { referenceNestedClass(mapClass, "Entry")!! }

    override val iterableClass: IrClassSymbol by lazy { referenceClassByFqname(StandardNames.FqNames.iterable)!! }
    override val listIteratorClass: IrClassSymbol by lazy { referenceClassByFqname(StandardNames.FqNames.listIterator)!! }
    override val mutableCollectionClass: IrClassSymbol by lazy { referenceClassByFqname(StandardNames.FqNames.mutableCollection)!! }
    override val mutableSetClass: IrClassSymbol by lazy { referenceClassByFqname(StandardNames.FqNames.mutableSet)!! }
    override val mutableListClass: IrClassSymbol by lazy { referenceClassByFqname(StandardNames.FqNames.mutableList)!! }
    override val mutableMapClass: IrClassSymbol by lazy { referenceClassByFqname(StandardNames.FqNames.mutableMap)!! }
    override val mutableMapEntryClass: IrClassSymbol by lazy { referenceNestedClass(StandardNames.FqNames.mutableMapEntry)!! }
    override val mutableIterableClass: IrClassSymbol by lazy { referenceClassByFqname(StandardNames.FqNames.mutableIterable)!! }
    override val mutableIteratorClass: IrClassSymbol by lazy { referenceClassByFqname(StandardNames.FqNames.mutableIterator)!! }
    override val mutableListIteratorClass: IrClassSymbol by lazy { referenceClassByFqname(StandardNames.FqNames.mutableListIterator)!! }
    override val comparableClass: IrClassSymbol by lazy { referenceClassByFqname(StandardNames.FqNames.comparable)!! }
    override val throwableType: IrType by lazy { throwableClass.defaultType }
    override val throwableClass: IrClassSymbol by lazy { referenceClassByFqname(StandardNames.FqNames.throwable)!! }

    override val kCallableClass: IrClassSymbol by lazy { referenceClassByFqname(StandardNames.FqNames.kCallable.toSafe())!! }
    override val kPropertyClass: IrClassSymbol by lazy { referenceClassByFqname(StandardNames.FqNames.kPropertyFqName.toSafe())!! }
    override val kClassClass: IrClassSymbol by lazy { referenceClassByFqname(StandardNames.FqNames.kClass.toSafe())!! }
    override val kProperty0Class: IrClassSymbol by lazy { referenceClassByFqname(StandardNames.FqNames.kProperty0.toSafe())!! }
    override val kProperty1Class: IrClassSymbol by lazy { referenceClassByFqname(StandardNames.FqNames.kProperty1.toSafe())!! }
    override val kProperty2Class: IrClassSymbol by lazy { referenceClassByFqname(StandardNames.FqNames.kProperty2.toSafe())!! }
    override val kMutableProperty0Class: IrClassSymbol by lazy { referenceClassByFqname(StandardNames.FqNames.kMutableProperty0.toSafe())!! }
    override val kMutableProperty1Class: IrClassSymbol by lazy { referenceClassByFqname(StandardNames.FqNames.kMutableProperty1.toSafe())!! }
    override val kMutableProperty2Class: IrClassSymbol by lazy { referenceClassByFqname(StandardNames.FqNames.kMutableProperty2.toSafe())!! }

    override val functionClass: IrClassSymbol by lazy { referenceClassByFqname(kotlinPackage, "Function")!! }
    override val kFunctionClass: IrClassSymbol by lazy { referenceClassByFqname(kotlinReflectPackage, "KFunction")!! }

    override val primitiveTypeToIrType = mapOf(
        PrimitiveType.BOOLEAN to booleanType,
        PrimitiveType.CHAR to charType,
        PrimitiveType.BYTE to byteType,
        PrimitiveType.SHORT to shortType,
        PrimitiveType.INT to intType,
        PrimitiveType.LONG to longType,
        PrimitiveType.FLOAT to floatType,
        PrimitiveType.DOUBLE to doubleType
    )

    private val primitiveIntegralIrTypes = listOf(byteType, shortType, intType, longType)
    override val primitiveFloatingPointIrTypes = listOf(floatType, doubleType)
    private val primitiveNumericIrTypes = primitiveIntegralIrTypes + primitiveFloatingPointIrTypes
    override val primitiveIrTypesWithComparisons = listOf(charType) + primitiveNumericIrTypes
    override val primitiveIrTypes = listOf(booleanType) + primitiveIrTypesWithComparisons
    private val baseIrTypes = primitiveIrTypes + stringType

    private val primitiveArithmeticOperatorResultTypes = mapOf(
        charType to intType, byteType to intType, shortType to intType, intType to intType,
        longType to longType, floatType to floatType, doubleType to doubleType
    )

    init {
        for (primitive in primitiveNumericIrTypes) {
            with(primitive.getClass()!!) {
                for (targetPrimitive in primitiveIrTypesWithComparisons) {
                    createMemberFunction("to${targetPrimitive.classFqName!!.shortName().asString()}", targetPrimitive)
                }
                for (targetPrimitive in primitiveNumericIrTypes) {
                    createMemberFunction(OperatorNameConventions.COMPARE_TO.asString(), intType, targetPrimitive)
                    val targetArithmeticReturnType = primitiveArithmeticOperatorResultTypes[targetPrimitive]!!
                    for (op in arrayOf(OperatorNameConventions.PLUS, OperatorNameConventions.MINUS, OperatorNameConventions.TIMES, OperatorNameConventions.DIV, OperatorNameConventions.REM)) {
                        createMemberFunction(op.asString(), targetArithmeticReturnType, targetPrimitive)
                    }
                }
                val arithmeticReturnType = primitiveArithmeticOperatorResultTypes[primitive]!!
                createMemberFunction(OperatorNameConventions.UNARY_PLUS.asString(), arithmeticReturnType)
                createMemberFunction(OperatorNameConventions.UNARY_MINUS.asString(), arithmeticReturnType)
                createMemberFunction(OperatorNameConventions.INC.asString(), primitive)
                createMemberFunction(OperatorNameConventions.DEC.asString(), primitive)
            }
        }
        for (typeWithBitwiseOps in arrayOf(intType, longType)) {
            with (typeWithBitwiseOps.getClass()!!) {
                for (op in arrayOf(OperatorNameConventions.AND, OperatorNameConventions.OR, OperatorNameConventions.XOR)) {
                    createMemberFunction(op.asString(), typeWithBitwiseOps, typeWithBitwiseOps)
                }
                for (op in arrayOf(OperatorNameConventions.SHL, OperatorNameConventions.SHR, OperatorNameConventions.USHR)) {
                    createMemberFunction(op.asString(), typeWithBitwiseOps, intType)
                }
                createMemberFunction(OperatorNameConventions.INV.asString(), typeWithBitwiseOps)
            }
        }
        with (charClass.owner) {
            for (targetPrimitive in primitiveIrTypesWithComparisons) {
                createMemberFunction("to${targetPrimitive.classFqName!!.shortName().asString()}", targetPrimitive)
            }
            createMemberFunction(OperatorNameConventions.COMPARE_TO.asString(), intType, charType)
            createMemberFunction(OperatorNameConventions.PLUS.asString(), charType, intType)
            createMemberFunction(OperatorNameConventions.MINUS.asString(), charType, intType)
            createMemberFunction(OperatorNameConventions.MINUS.asString(), intType, charType)
            createMemberFunction(OperatorNameConventions.INC.asString(), charType)
            createMemberFunction(OperatorNameConventions.DEC.asString(), charType)
        }

        with (stringClass.owner) {
            createMemberFunction(OperatorNameConventions.COMPARE_TO.asString(), intType, defaultType)
            createProperty("length", intType)
            createMemberFunction(OperatorNameConventions.PLUS.asString(), defaultType, anyNType)
            createMemberFunction(OperatorNameConventions.GET.asString(), charType, intType)
        }
    }

    override val booleanArray: IrClassSymbol = kotlinIrPackage.createClass(PrimitiveType.BOOLEAN.arrayTypeName) { addArrayMembers(booleanType) }
    override val charArray: IrClassSymbol = kotlinIrPackage.createClass(PrimitiveType.CHAR.arrayTypeName) { addArrayMembers(charType) }
    override val byteArray: IrClassSymbol = kotlinIrPackage.createClass(PrimitiveType.BYTE.arrayTypeName) { addArrayMembers(byteType) }
    override val shortArray: IrClassSymbol = kotlinIrPackage.createClass(PrimitiveType.SHORT.arrayTypeName) { addArrayMembers(shortType) }
    override val intArray: IrClassSymbol = kotlinIrPackage.createClass(PrimitiveType.INT.arrayTypeName) { addArrayMembers(intType) }
    override val longArray: IrClassSymbol = kotlinIrPackage.createClass(PrimitiveType.LONG.arrayTypeName) { addArrayMembers(longType) }
    override val floatArray: IrClassSymbol = kotlinIrPackage.createClass(PrimitiveType.FLOAT.arrayTypeName) { addArrayMembers(floatType) }
    override val doubleArray: IrClassSymbol = kotlinIrPackage.createClass(PrimitiveType.DOUBLE.arrayTypeName) { addArrayMembers(doubleType) }

    override val primitiveArraysToPrimitiveTypes: Map<IrClassSymbol, PrimitiveType> = mapOf(
        booleanArray to PrimitiveType.BOOLEAN,
        charArray to PrimitiveType.CHAR,
        byteArray to PrimitiveType.BYTE,
        shortArray to PrimitiveType.SHORT,
        intArray to PrimitiveType.INT,
        longArray to PrimitiveType.LONG,
        floatArray to PrimitiveType.FLOAT,
        doubleArray to PrimitiveType.DOUBLE
    )

    override val primitiveArrays = primitiveArraysToPrimitiveTypes.keys
    override val primitiveArrayElementTypes = primitiveArraysToPrimitiveTypes.mapValues { primitiveTypeToIrType[it.value] }
    override val primitiveArrayForType = primitiveArrayElementTypes.asSequence().associate { it.value to it.key }

    private val _ieee754equalsFunByOperandType = mutableMapOf<IrClassifierSymbol, IrSimpleFunctionSymbol>()
    override val ieee754equalsFunByOperandType: MutableMap<IrClassifierSymbol, IrSimpleFunctionSymbol>
        get() = _ieee754equalsFunByOperandType

    override lateinit var eqeqeqSymbol: IrSimpleFunctionSymbol private set
    override lateinit var eqeqSymbol: IrSimpleFunctionSymbol private set
    override lateinit var throwCceSymbol: IrSimpleFunctionSymbol private set
    override lateinit var throwIseSymbol: IrSimpleFunctionSymbol private set
    override lateinit var andandSymbol: IrSimpleFunctionSymbol private set
    override lateinit var ororSymbol: IrSimpleFunctionSymbol private set
    override lateinit var noWhenBranchMatchedExceptionSymbol: IrSimpleFunctionSymbol private set
    override lateinit var illegalArgumentExceptionSymbol: IrSimpleFunctionSymbol private set
    override lateinit var dataClassArrayMemberHashCodeSymbol: IrSimpleFunctionSymbol private set
    override lateinit var dataClassArrayMemberToStringSymbol: IrSimpleFunctionSymbol private set

    override lateinit var checkNotNullSymbol: IrSimpleFunctionSymbol private set
    override lateinit var arrayOfNulls: IrSimpleFunctionSymbol private set

    override lateinit var lessFunByOperandType: Map<IrClassifierSymbol, IrSimpleFunctionSymbol> private set
    override lateinit var lessOrEqualFunByOperandType: Map<IrClassifierSymbol, IrSimpleFunctionSymbol> private set
    override lateinit var greaterOrEqualFunByOperandType: Map<IrClassifierSymbol, IrSimpleFunctionSymbol> private set
    override lateinit var greaterFunByOperandType: Map<IrClassifierSymbol, IrSimpleFunctionSymbol> private set

    private val internalIrPackageFragment: IrClassSymbol =
        createClass(StandardNames.BUILT_INS_PACKAGE_FQ_NAME.child(Name.identifier("InternalKt")), internalIrPackage) {

            fun addBuiltinFunctionSymbol(name: String, returnType: IrType, vararg valueParameterTypes: IrType, builder: IrSimpleFunction.() -> Unit = {}) =
                createFunction(KOTLIN_INTERNAL_IR_FQN, name, returnType, valueParameterTypes, this).also {
                    declarations.add(it)
                    it.builder()
                }.symbol

            primitiveFloatingPointIrTypes.forEach { fpType ->
                _ieee754equalsFunByOperandType.put(
                    fpType.classifierOrFail,
                    addBuiltinFunctionSymbol(BuiltInOperatorNames.IEEE754_EQUALS, booleanType, fpType.makeNullable(), fpType.makeNullable())
                )
            }
            eqeqeqSymbol = addBuiltinFunctionSymbol(BuiltInOperatorNames.EQEQEQ, booleanType, anyNType, anyNType)
            eqeqSymbol = addBuiltinFunctionSymbol(BuiltInOperatorNames.EQEQ, booleanType, anyNType, anyNType)
            throwCceSymbol = addBuiltinFunctionSymbol(BuiltInOperatorNames.THROW_CCE, nothingType)
            throwIseSymbol = addBuiltinFunctionSymbol(BuiltInOperatorNames.THROW_ISE, nothingType)
            andandSymbol = addBuiltinFunctionSymbol(BuiltInOperatorNames.ANDAND, booleanType, booleanType, booleanType)
            ororSymbol = addBuiltinFunctionSymbol(BuiltInOperatorNames.OROR, booleanType, booleanType, booleanType)
            noWhenBranchMatchedExceptionSymbol =
                addBuiltinFunctionSymbol(BuiltInOperatorNames.NO_WHEN_BRANCH_MATCHED_EXCEPTION, nothingType)
            illegalArgumentExceptionSymbol =
                addBuiltinFunctionSymbol(BuiltInOperatorNames.ILLEGAL_ARGUMENT_EXCEPTION, nothingType, stringType)
            dataClassArrayMemberHashCodeSymbol = addBuiltinFunctionSymbol("dataClassArrayMemberHashCode", intType, anyType)
            dataClassArrayMemberToStringSymbol = addBuiltinFunctionSymbol("dataClassArrayMemberToString", stringType, anyNType)

            checkNotNullSymbol = run {
                val typeParameter: IrTypeParameter = irFactory.createTypeParameter(
                    UNDEFINED_OFFSET, UNDEFINED_OFFSET, BUILTIN_OPERATOR, IrTypeParameterSymbolImpl(), Name.identifier("T0"), 0, true,
                    Variance.INVARIANT
                ).apply {
                    superTypes += anyType
                }

                createFunction(
                    KOTLIN_INTERNAL_IR_FQN, "CHECK_NOT_NULL",
                    IrSimpleTypeImpl(typeParameter.symbol, hasQuestionMark = false, emptyList(), emptyList()),
                    arrayOf(IrSimpleTypeImpl(typeParameter.symbol, hasQuestionMark = true, emptyList(), emptyList())),
                    this
                ).also {
                    it.typeParameters = listOf(typeParameter)
                    declarations.add(it)
                }.symbol
            }

            fun List<IrType>.defineComparisonOperatorForEachIrType(name: String) =
                associate { it.classifierOrFail to addBuiltinFunctionSymbol(name, booleanType, it, it) }

            lessFunByOperandType = primitiveIrTypesWithComparisons.defineComparisonOperatorForEachIrType(BuiltInOperatorNames.LESS)
            lessOrEqualFunByOperandType =
                primitiveIrTypesWithComparisons.defineComparisonOperatorForEachIrType(BuiltInOperatorNames.LESS_OR_EQUAL)
            greaterOrEqualFunByOperandType =
                primitiveIrTypesWithComparisons.defineComparisonOperatorForEachIrType(BuiltInOperatorNames.GREATER_OR_EQUAL)
            greaterFunByOperandType = primitiveIrTypesWithComparisons.defineComparisonOperatorForEachIrType(BuiltInOperatorNames.GREATER)

            arrayOfNulls =
                createFunction(kotlinPackage, "arrayOfNulls", arrayClass.defaultType, arrayOf(intType), kotlinIrPackage).also {
                    val typeParameter = it.addTypeParameter("T", anyNType)
                    it.returnType = arrayClass.typeWithArguments(listOf(typeParameter.defaultType as IrTypeArgument))
                }.symbol
        }

    override val unsignedArrays: Set<IrClassSymbol> = UnsignedType.values().mapNotNullTo(mutableSetOf()) { unsignedType ->
        referenceClassByClassId(unsignedType.arrayClassId)
    }

    override fun getKPropertyClass(mutable: Boolean, n: Int): IrClassSymbol = when (n) {
        0 -> if (mutable) kMutableProperty0Class else kProperty0Class
        1 -> if (mutable) kMutableProperty1Class else kProperty1Class
        2 -> if (mutable) kMutableProperty2Class else kProperty2Class
        else -> error("No KProperty for n=$n mutable=$mutable")
    }

    override val enumClass: IrClassSymbol by lazy { referenceClassByFqname(kotlinPackage, "Enum")!! }

    override val intPlusSymbol: IrSimpleFunctionSymbol
        get() = intClass.functions.single {
            it.owner.name == OperatorNameConventions.PLUS &&
                    KotlinTypeChecker.DEFAULT.equalTypes(it.owner.valueParameters[0].type.toKotlinType(), intType.toKotlinType())
        }

    override val intTimesSymbol: IrSimpleFunctionSymbol
        get() = intClass.functions.single {
            it.owner.name == OperatorNameConventions.TIMES &&
                    KotlinTypeChecker.DEFAULT.equalTypes(it.owner.valueParameters[0].type.toKotlinType(), intType.toKotlinType())
        }

    override val extensionToString: IrSimpleFunctionSymbol by lazy {
        findFunctions(kotlinPackage, Name.identifier("toString")).first { function ->
            function.owner.extensionReceiverParameter?.let { receiver ->
                KotlinTypeChecker.DEFAULT.equalTypes(receiver.type.toKotlinType(), anyNType.toKotlinType())
            } ?: false
        }
    }

    override val stringPlus: IrSimpleFunctionSymbol
        get() = intClass.functions.single {
            it.owner.name == OperatorNameConventions.PLUS &&
                    KotlinTypeChecker.DEFAULT.equalTypes(it.owner.valueParameters[0].type.toKotlinType(), stringType.toKotlinType())
        }

    private class KotlinPackageFuns(
        val arrayOf: IrSimpleFunctionSymbol,
    )

    private val kotlinBuiltinFunctions by lazy {
        fun IrClassSymbol.addPackageFun(name: String, returnType: IrType, vararg argumentTypes: IrType, builder: IrSimpleFunction.() -> Unit) =
            createFunction(kotlinPackage, name, returnType, argumentTypes, owner).also {
                it.builder()
                this.owner.declarations.add(it)
            }.symbol

        val kotlinKt = createClass(kotlinPackage.child(Name.identifier("KotlinKt")), kotlinIrPackage)
        KotlinPackageFuns(
            arrayOf = kotlinKt.addPackageFun("arrayOf", arrayClass.defaultType) {
                addTypeParameter("T", anyNType)
                addValueParameter {
                    this.name = Name.identifier("elements")
                    this.type = arrayClass.defaultType
                    this.varargElementType = typeParameters[0].defaultType
                    this.origin = origin
                }
            }
        )
    }

    override val arrayOf: IrSimpleFunctionSymbol get() = kotlinBuiltinFunctions.arrayOf

    override val toUIntByExtensionReceiver: Map<IrClassifierSymbol, IrSimpleFunctionSymbol> by lazy {
        findFunctions(kotlinPackage, Name.identifier("toUInt")).mapNotNull { fn ->
            fn.owner.extensionReceiverParameter?.type?.classifierOrNull?.let { klass ->
                klass to fn
            }
        }.toMap()
    }

    override val toULongByExtensionReceiver: Map<IrClassifierSymbol, IrSimpleFunctionSymbol> by lazy {
        findFunctions(kotlinPackage, Name.identifier("toULong")).mapNotNull { fn ->
            fn.owner.extensionReceiverParameter?.type?.classifierOrNull?.let { klass ->
                klass to fn
            }
        }.toMap()
    }

    private val functionNMap = mutableMapOf<Int, IrClass>()
    private val kFunctionNMap = mutableMapOf<Int, IrClass>()
    private val suspendFunctionNMap = mutableMapOf<Int, IrClass>()
    private val kSuspendFunctionNMap = mutableMapOf<Int, IrClass>()

    override fun functionN(arity: Int, declarator: SymbolTable.((IrClassSymbol) -> IrClass) -> IrClass): IrClass = functionN(arity)

    override fun kFunctionN(arity: Int, declarator: SymbolTable.((IrClassSymbol) -> IrClass) -> IrClass): IrClass = kFunctionN(arity)

    override fun suspendFunctionN(arity: Int, declarator: SymbolTable.((IrClassSymbol) -> IrClass) -> IrClass): IrClass = suspendFunctionN(arity)

    override fun kSuspendFunctionN(arity: Int, declarator: SymbolTable.((IrClassSymbol) -> IrClass) -> IrClass): IrClass = kSuspendFunctionN(arity)

    override fun functionN(arity: Int): IrClass = functionNMap.getOrPut(arity) {
        referenceClassByClassId(StandardNames.getFunctionClassId(arity))!!.owner
    }

    override fun kFunctionN(arity: Int): IrClass = kFunctionNMap.getOrPut(arity) {
        referenceClassByClassId(StandardNames.getKFunctionClassId(arity))!!.owner
    }

    override fun suspendFunctionN(arity: Int): IrClass = suspendFunctionNMap.getOrPut(arity) {
        referenceClassByClassId(StandardNames.getSuspendFunctionClassId(arity))!!.owner
    }

    override fun kSuspendFunctionN(arity: Int): IrClass = kSuspendFunctionNMap.getOrPut(arity) {
        referenceClassByClassId(StandardNames.getKSuspendFunctionClassId(arity))!!.owner
    }

    override fun findFunctions(name: Name, vararg packageNameSegments: String): Iterable<IrSimpleFunctionSymbol> =
        findFunctions(FqName.fromSegments(packageNameSegments.asList()), name)

    override fun findClass(name: Name, vararg packageNameSegments: String): IrClassSymbol? =
        referenceClassByFqname(FqName.fromSegments(packageNameSegments.asList()), name)

    override fun getBinaryOperator(name: Name, lhsType: IrType, rhsType: IrType): IrSimpleFunctionSymbol {
        val definingClass = lhsType.getMaybeBuiltinClass() ?: error("Defining class not found: $lhsType")
        return definingClass.functions.single { function ->
            function.name == name && function.valueParameters.size == 1 && function.valueParameters[0].type == rhsType
        }.symbol
    }

    override fun getUnaryOperator(name: Name, receiverType: IrType): IrSimpleFunctionSymbol {
        val definingClass = receiverType.getMaybeBuiltinClass() ?: error("Defining class not found: $receiverType")
        return definingClass.functions.single { function ->
            function.name == name && function.valueParameters.isEmpty()
        }.symbol
    }

    override val getProgressionLastElementByReturnType: Map<IrClassifierSymbol?, IrSimpleFunctionSymbol> by lazy {
        findFunctions(kotlinPackage.child(Name.identifier("internal")), Name.identifier("getProgressionLastElement")).mapNotNull { fn ->
            fn.owner.returnType.classOrNull?.let { it to fn }
        }.toMap()
    }

    private fun referenceClassByFqname(topLevelFqName: FqName) =
        referenceClassByClassId(ClassId.topLevel(topLevelFqName))

    private fun referenceClassByFqname(packageName: FqName, identifier: Name) =
        referenceClassByClassId(ClassId(packageName, identifier))

    private fun referenceClassByFqname(packageName: FqName, identifier: String) =
        referenceClassByClassId(ClassId(packageName, Name.identifier(identifier)))

    private fun referenceClassByClassId(classId: ClassId): IrClassSymbol? {
        val firSymbol = components.session.symbolProvider.getClassLikeSymbolByFqName(classId) ?: return null
        val firClassSymbol = firSymbol as? FirClassSymbol ?: return null
        return components.classifierStorage.getIrClassSymbol(firClassSymbol)
    }

    private fun referenceNestedClass(klass: IrClassSymbol, identifier: String): IrClassSymbol? =
        referenceClassByClassId(klass.owner.classId!!.createNestedClassId(Name.identifier(identifier)))

    private fun referenceNestedClass(fqName: FqName): IrClassSymbol? =
        referenceClassByClassId(ClassId(fqName.parent().parent(), fqName.parent().shortName()).createNestedClassId(fqName.shortName()))

    private fun IrType.getMaybeBuiltinClass(): IrClass? {
        val lhsClassFqName = classFqName!!
        return baseIrTypes.find { it.classFqName == lhsClassFqName }?.getClass()
            ?: referenceClassByFqname(lhsClassFqName)?.owner
    }

    private fun createPackage(fqName: FqName): IrPackageFragment =
        IrExternalPackageFragmentImpl.createEmptyExternalPackageFragment(moduleDescriptor, fqName)

    private fun createClass(
        fqName: FqName,
        parent: IrDeclarationParent,
        classKind: ClassKind = ClassKind.CLASS,
        classModality: Modality = Modality.FINAL,
        classIsInline: Boolean = false,
        block: IrClass.() -> Unit = {}
    ): IrClassSymbol
    {
        val signature = IdSignature.PublicSignature(fqName.parent().asString(), fqName.shortName().asString(), null, 0)

        return components.symbolTable.declareClass(
            signature,
            { IrClassPublicSymbolImpl(signature) },
            { symbol ->
                IrClassBuilder().run {
                    name = fqName.shortName()
                    kind = classKind
                    modality = classModality
                    isInline = classIsInline
                    origin = BUILTIN_CLASS
                    irFactory.createClass(
                        startOffset, endOffset, origin, symbol, name, kind, visibility, modality,
                        isCompanion, isInner, isData, isExternal, isInline, isExpect, isFun
                    )
                }.also {
                    it.parent = parent
                    it.createImplicitParameterDeclarationWithWrappedDescriptor()
                    it.block()
                }
            }
        ).symbol
    }

    private fun IrPackageFragment.createClass(
        name: String,
        classKind: ClassKind = ClassKind.CLASS,
        classModality: Modality = Modality.FINAL,
        classIsInline: Boolean = false,
        block: IrClass.() -> Unit = {}
    ): IrClassSymbol =
        createClass(Name.identifier(name), classKind, classModality, classIsInline, block)

    private fun IrPackageFragment.createClass(
        name: Name,
        classKind: ClassKind = ClassKind.CLASS,
        classModality: Modality = Modality.FINAL,
        classIsInline: Boolean = false,
        block: IrClass.() -> Unit = {}
    ): IrClassSymbol =
        createClass(fqName.child(name), this, classKind, classModality, classIsInline, block)

    private fun IrClass.createConstructor(
        origin: IrDeclarationOrigin = BUILTIN_CLASS_CONSTRUCTOR,
        isPrimary: Boolean = true,
        visibility: DescriptorVisibility = DescriptorVisibilities.PUBLIC,
        build: IrConstructor.() -> Unit = {}
    ): IrConstructorSymbol {
        val name = Name.special("<init>")
        val signature = IdSignature.PublicSignature(this.packageFqName!!.asString(), classId!!.relativeClassName.child(name).asString(), null, 0)
        val ctor = irFactory.createConstructor(
            UNDEFINED_OFFSET,UNDEFINED_OFFSET, origin, IrConstructorPublicSymbolImpl(signature), name, visibility, defaultType,
            isInline = false, isExternal = false, isPrimary = isPrimary, isExpect = false
        )
        ctor.parent = this
        ctor.build()
        declarations.add(ctor)
        return ctor.symbol
    }

    private fun IrClass.createMemberFunction(
        name: String, returnType: IrType, vararg valueParameterTypes: IrType,
        origin: IrDeclarationOrigin = BUILTIN_OPERATOR,
        build: IrFunctionBuilder.() -> Unit = {}
    ) = createFunction(
        IdSignature.PublicSignature(this.packageFqName!!.asString(), classId!!.relativeClassName.child(Name.identifier(name)).asString(), null, 0),
        name, returnType, valueParameterTypes, parent, origin, build
    ).also {
        it.addDispatchReceiver { type = this@createMemberFunction.defaultType }
        declarations.add(it)
        it.parent = this@createMemberFunction
    }

    private fun createFunction(
        signature: IdSignature,
        name: String,
        returnType: IrType,
        valueParameterTypes: Array<out IrType>,
        parent: IrDeclarationParent,
        origin: IrDeclarationOrigin = BUILTIN_OPERATOR,
        build: IrFunctionBuilder.() -> Unit = {}
    ) = IrFunctionBuilder().run {
        this.name = Name.identifier(name)
        this.returnType = returnType
        this.origin = origin
        build()
        irFactory.createFunction(
            startOffset, endOffset, origin, IrSimpleFunctionPublicSymbolImpl(signature), this.name, visibility, modality, this.returnType,
            isInline, isExternal, isTailrec, isSuspend, isOperator, isInfix, isExpect, isFakeOverride, containerSource,
        ).also { fn ->
            valueParameterTypes.forEachIndexed { index, irType ->
                fn.addValueParameter(Name.identifier("arg$index"), irType, origin)
            }
            fn.parent = parent
        }
    }

    private fun createFunction(
        packageFqName: FqName,
        name: String,
        returnType: IrType,
        valueParameterTypes: Array<out IrType>,
        parent: IrDeclarationParent
    ) = createFunction(
        IdSignature.PublicSignature(packageFqName.asString(), name, null, 0),
        name, returnType, valueParameterTypes, parent
    )

    private fun IrClass.addArrayMembers(elementType: IrType) {
        addConstructor {
            origin = BUILTIN_CLASS_CONSTRUCTOR
            returnType = defaultType
            isPrimary = true
        }.also {
            it.addValueParameter("size", intType, BUILTIN_CLASS_CONSTRUCTOR)
        }
        createMemberFunction("get", elementType, intType) {
            isOperator = true
        }
        createMemberFunction("set", unitType, intType, elementType) {
            isOperator = true
        }
        createProperty("size", intType)
    }

    private fun IrClass.createProperty(propertyName: String, returnType: IrType, builder: IrProperty.() -> Unit = {}) {
        addProperty { name = Name.identifier(propertyName) }.also {
            it.getter = irFactory.buildFun {
                name = Name.special("<get-$propertyName>")
                this.returnType = returnType
            }.also {
                it.addDispatchReceiver { type = this@createProperty.defaultType }
                it.parent = this
            }
            it.builder()
        }
    }

    private fun IrPackageFragment.createNumberClass(name: String, builder: IrClass.() -> Unit = {}): IrClassSymbol =
        createClass(name) {
            builder()
        }

    private fun findFunctions(packageName: FqName, name: Name) =
        components.session.symbolProvider.getTopLevelFunctionSymbols(packageName, name).mapNotNull { firOpSymbol ->
            components.declarationStorage.getIrFunctionSymbol(firOpSymbol) as? IrSimpleFunctionSymbol
        }

    private fun findFunction(
        packageName: FqName,
        name: Name,
        returnType: IrType,
        vararg valueParameterTypes: IrType
    ): IrSimpleFunctionSymbol =
        findFunctions(packageName, name).firstOrNull { irSymbol ->
            irSymbol.owner.let {
                it.returnType == returnType && it.valueParameters.size == valueParameterTypes.size &&
                        it.valueParameters.zip(valueParameterTypes).all { it.first.type == it.second }
            }
        } ?: error("no fun $name found in $packageName")

//    private fun defineOperator(name: String, returnType: IrType, valueParameterTypes: List<IrType>): IrSimpleFunctionSymbol {
//        val operatorDescriptor =
//            IrSimpleBuiltinOperatorDescriptorImpl(packageFragmentDescriptor, Name.identifier(name), returnType.originalKotlinType!!)
//
//        for ((i, valueParameterType) in valueParameterTypes.withIndex()) {
//            operatorDescriptor.addValueParameter(
//                IrBuiltinValueParameterDescriptorImpl(
//                    operatorDescriptor, Name.identifier("arg$i"), i, valueParameterType.originalKotlinType!!
//                )
//            )
//        }
//
//        val symbol = symbolTable.declareSimpleFunctionIfNotExists(operatorDescriptor) {
//            val operator = irFactory.createFunction(
//                UNDEFINED_OFFSET, UNDEFINED_OFFSET, BUILTIN_OPERATOR, it, Name.identifier(name), DescriptorVisibilities.PUBLIC, Modality.FINAL,
//                returnType, isInline = false, isExternal = false, isTailrec = false, isSuspend = false,
//                isOperator = false, isInfix = false, isExpect = false, isFakeOverride = false
//            )
//            operator.parent = packageFragment
//            packageFragment.declarations += operator
//
//            operator.valueParameters = valueParameterTypes.withIndex().map { (i, valueParameterType) ->
//                val valueParameterDescriptor = operatorDescriptor.valueParameters[i]
//                val valueParameterSymbol = IrValueParameterSymbolImpl(valueParameterDescriptor)
//                irFactory.createValueParameter(
//                    UNDEFINED_OFFSET, UNDEFINED_OFFSET, BUILTIN_OPERATOR, valueParameterSymbol, Name.identifier("arg$i"), i,
//                    valueParameterType, null, isCrossinline = false, isNoinline = false, isHidden = false, isAssignable = false
//                ).apply {
//                    parent = operator
//                }
//            }
//
//            operator
//        }
//
//        return symbol.symbol
//    }
}

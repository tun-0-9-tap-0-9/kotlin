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
import org.jetbrains.kotlin.ir.symbols.IrSimpleFunctionSymbol
import org.jetbrains.kotlin.ir.symbols.impl.IrClassPublicSymbolImpl
import org.jetbrains.kotlin.ir.symbols.impl.IrSimpleFunctionPublicSymbolImpl
import org.jetbrains.kotlin.ir.symbols.impl.IrTypeParameterSymbolImpl
import org.jetbrains.kotlin.ir.types.*
import org.jetbrains.kotlin.ir.types.impl.IrSimpleTypeImpl
import org.jetbrains.kotlin.ir.util.IdSignature
import org.jetbrains.kotlin.ir.util.SymbolTable
import org.jetbrains.kotlin.ir.util.classId
import org.jetbrains.kotlin.name.ClassId
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.types.Variance

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

    override val anyClass: IrClassSymbol = referenceClassByFqname(kotlinPackage, "Any")!!
    override val anyType: IrType = anyClass.defaultType
    override val anyNType = anyType.withHasQuestionMark(true)

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
    override val charClass: IrClassSymbol = referenceClassByFqname(kotlinPackage, "Char")!!

    override val numberClass: IrClassSymbol = referenceClassByFqname(kotlinPackage, "Number")!!
    override val numberType: IrType get() = numberClass.defaultType

    override val byteType: IrType get() = byteClass.defaultType
    override val byteClass: IrClassSymbol = referenceClassByFqname(kotlinPackage, "Byte")!!
    override val shortType: IrType get() = shortClass.defaultType
    override val shortClass: IrClassSymbol = referenceClassByFqname(kotlinPackage, "Short")!!
    override val intType: IrType get() = intClass.defaultType
    override val intClass: IrClassSymbol = referenceClassByFqname(kotlinPackage, "Int")!!
    override val longType: IrType get() = longClass.defaultType
    override val longClass: IrClassSymbol = referenceClassByFqname(kotlinPackage, "Long")!!
    override val floatType: IrType get() = floatClass.defaultType
    override val floatClass: IrClassSymbol = referenceClassByFqname(kotlinPackage, "Float")!!
    override val doubleType: IrType get() = doubleClass.defaultType
    override val doubleClass: IrClassSymbol = referenceClassByFqname(kotlinPackage, "Double")!!
    override val nothingType: IrType get() = nothingClass.defaultType
    override val nothingClass: IrClassSymbol = referenceClassByFqname(kotlinPackage, "Nothing")!!
    override val nothingNType: IrType = nothingType.withHasQuestionMark(true)

    override val unitType: IrType get() = unitClass.defaultType
    override val unitClass: IrClassSymbol = referenceClassByFqname(kotlinPackage, "Unit")!!
    override val stringType: IrType get() = stringClass.defaultType
    override val stringClass: IrClassSymbol = //referenceClassByFqname(kotlinPackage, "String")!!
        createClass(kotlinPackage.child(Name.identifier("String")), kotlinIrPackage)

    override val charSequenceClass: IrClassSymbol = referenceClassByFqname(kotlinPackage, "CharSequence")!!

    override val collectionClass: IrClassSymbol by lazy { referenceClassByFqname(kotlinCollectionsPackage, "Collection")!! }
    override val arrayClass: IrClassSymbol by lazy { referenceClassByFqname(kotlinPackage, "Array")!! }
    override val setClass: IrClassSymbol by lazy { referenceClassByFqname(kotlinCollectionsPackage, "Set")!! }
    override val listClass: IrClassSymbol by lazy { referenceClassByFqname(kotlinCollectionsPackage, "List")!! }
    override val mapClass: IrClassSymbol by lazy { referenceClassByFqname(kotlinCollectionsPackage, "Map")!! }
    override val mapEntryClass: IrClassSymbol by lazy { referenceNestedClass(mapClass, "Entry")!! }

//    override val iterableClass: IrClassSymbol = referenceClassByFqname(ClassId(kotlinCollectionsPackageFqName, Name.identifier("Iterable")))!!
//    override val listIteratorClass: IrClassSymbol = referenceClassByFqname(ClassId(kotlinPackageFqName, Name.identifier("ListIterator")))!!
//    override val mutableCollectionClass: IrClassSymbol = referenceClassByFqname(ClassId(kotlinPackageFqName, Name.identifier("mutableCollection")))!!
//    override val mutableSetClass: IrClassSymbol = referenceClassByFqname(ClassId(kotlinPackageFqName, Name.identifier("mutableSet")))!!
//    override val mutableListClass: IrClassSymbol = referenceClassByFqname(ClassId(kotlinPackageFqName, Name.identifier("mutableList")))!!
//    override val mutableMapClass: IrClassSymbol = referenceClassByFqname(ClassId(kotlinPackageFqName, Name.identifier("mutableMap")))!!
//    override val mutableMapEntryClass: IrClassSymbol = referenceClassByFqname(ClassId(kotlinPackageFqName, Name.identifier("mutableMapEntry")))!!
//    override val mutableIterableClass: IrClassSymbol = referenceClassByFqname(ClassId(kotlinPackageFqName, Name.identifier("mutableIterable")))!!
//    override val mutableIteratorClass: IrClassSymbol = referenceClassByFqname(ClassId(kotlinPackageFqName, Name.identifier("mutableIterator")))!!
//    override val mutableListIteratorClass: IrClassSymbol = referenceClassByFqname(ClassId(kotlinPackageFqName, Name.identifier("mutableListIterator")))!!

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

    override val primitiveIrTypes = listOf(booleanType, charType, byteType, shortType, intType, longType, floatType, doubleType)
    override val primitiveIrTypesWithComparisons = listOf(charType, byteType, shortType, intType, longType, floatType, doubleType)
    override val primitiveFloatingPointIrTypes = listOf(floatType, doubleType)

    override val booleanArray: IrClassSymbol = referenceClassByFqname(kotlinPackage, PrimitiveType.BOOLEAN.arrayTypeName)!!
    override val charArray: IrClassSymbol = referenceClassByFqname(kotlinPackage, PrimitiveType.CHAR.arrayTypeName)!!
    override val byteArray: IrClassSymbol = referenceClassByFqname(kotlinPackage, PrimitiveType.BYTE.arrayTypeName)!!
    override val shortArray: IrClassSymbol = referenceClassByFqname(kotlinPackage, PrimitiveType.SHORT.arrayTypeName)!!
    override val intArray: IrClassSymbol = referenceClassByFqname(kotlinPackage, PrimitiveType.INT.arrayTypeName)!!
    override val longArray: IrClassSymbol = referenceClassByFqname(kotlinPackage, PrimitiveType.LONG.arrayTypeName)!!
    override val floatArray: IrClassSymbol = referenceClassByFqname(kotlinPackage, PrimitiveType.FLOAT.arrayTypeName)!!
    override val doubleArray: IrClassSymbol = referenceClassByFqname(kotlinPackage, PrimitiveType.DOUBLE.arrayTypeName)!!

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

    override lateinit var lessFunByOperandType: Map<IrClassifierSymbol, IrSimpleFunctionSymbol> private set
    override lateinit var lessOrEqualFunByOperandType: Map<IrClassifierSymbol, IrSimpleFunctionSymbol> private set
    override lateinit var greaterOrEqualFunByOperandType: Map<IrClassifierSymbol, IrSimpleFunctionSymbol> private set
    override lateinit var greaterFunByOperandType: Map<IrClassifierSymbol, IrSimpleFunctionSymbol> private set

    private val internalIrPackageFragment: IrClassSymbol =
        createClass(KOTLIN_INTERNAL_IR_FQN.child(Name.identifier("InternalKt")), internalIrPackage) { klass ->

            fun addBuiltinFunctionSymbol(name: String, returnType: IrType, vararg valueParameterTypes: IrType) =
                createFunction(KOTLIN_INTERNAL_IR_FQN, name, returnType, valueParameterTypes, klass).also {
                    klass.declarations.add(it)
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
                    klass
                ).also {
                    it.typeParameters = listOf(typeParameter)
                    klass.declarations.add(it)
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

    override val enumClass: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val intPlusSymbol: IrSimpleFunctionSymbol
        get() = TODO("Not yet implemented")
    override val intTimesSymbol: IrSimpleFunctionSymbol
        get() = TODO("Not yet implemented")
    override val extensionToString: IrSimpleFunctionSymbol
        get() = TODO("Not yet implemented")
    override val stringPlus: IrSimpleFunctionSymbol
        get() = TODO("Not yet implemented")
    override val arrayOf: IrSimpleFunctionSymbol
        get() = TODO("Not yet implemented")

    private class KotlinPackageFuns(val arrayOfNulls: IrSimpleFunctionSymbol)

    private val kotlinPackageFragment by lazy {
        val fragment = createClass(kotlinPackage.child(Name.identifier("LibraryKt")), kotlinIrPackage)
        KotlinPackageFuns(
            arrayOfNulls = createFunction(
                kotlinPackage,
                "arrayOfNulls",
                arrayClass.defaultType,
                arrayOf(intType),
                fragment.owner
            ).also {
                it.addTypeParameter("T", anyNType)
            }.symbol
        )
    }

    override val arrayOfNulls: IrSimpleFunctionSymbol get() = kotlinPackageFragment.arrayOfNulls

    override val toUIntByExtensionReceiver: Map<IrClassifierSymbol, IrSimpleFunctionSymbol>
        get() = TODO("Not yet implemented")
    override val toULongByExtensionReceiver: Map<IrClassifierSymbol, IrSimpleFunctionSymbol>
        get() = TODO("Not yet implemented")

    override fun functionN(arity: Int, declarator: SymbolTable.((IrClassSymbol) -> IrClass) -> IrClass): IrClass {
        TODO("Not yet implemented")
    }

    override fun kFunctionN(arity: Int, declarator: SymbolTable.((IrClassSymbol) -> IrClass) -> IrClass): IrClass {
        TODO("Not yet implemented")
    }

    override fun suspendFunctionN(arity: Int, declarator: SymbolTable.((IrClassSymbol) -> IrClass) -> IrClass): IrClass {
        TODO("Not yet implemented")
    }

    override fun kSuspendFunctionN(arity: Int, declarator: SymbolTable.((IrClassSymbol) -> IrClass) -> IrClass): IrClass {
        TODO("Not yet implemented")
    }

    override fun functionN(arity: Int): IrClass {
        TODO("Not yet implemented")
    }

    override fun kFunctionN(arity: Int): IrClass {
        TODO("Not yet implemented")
    }

    override fun suspendFunctionN(arity: Int): IrClass {
        TODO("Not yet implemented")
    }

    override fun kSuspendFunctionN(arity: Int): IrClass {
        TODO("Not yet implemented")
    }

    override fun findFunctions(name: Name, vararg packageNameSegments: String): Iterable<IrSimpleFunctionSymbol> =
        findFunctions(FqName.fromSegments(packageNameSegments.asList()), name)

    override fun findClass(name: Name, vararg packageNameSegments: String): IrClassSymbol? =
        referenceClassByFqname(FqName.fromSegments(packageNameSegments.asList()), name)

    override fun getBinaryOperator(name: Name, lhsType: IrType, rhsType: IrType): IrSimpleFunctionSymbol {
        TODO("Not yet implemented")
    }

    override fun getUnaryOperator(name: Name, receiverType: IrType): IrSimpleFunctionSymbol {
        TODO("Not yet implemented")
    }

    override val getProgressionLastElementByReturnType: Map<IrClassifierSymbol?, IrSimpleFunctionSymbol>
        get() = TODO("Not yet implemented")

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

    private fun createPackage(fqName: FqName): IrPackageFragment =
        IrExternalPackageFragmentImpl.createEmptyExternalPackageFragment(moduleDescriptor, fqName)

    private fun createClass(
        fqName: FqName,
        parent: IrDeclarationParent,
        classKind: ClassKind = ClassKind.CLASS,
        classModality: Modality = Modality.FINAL,
        classIsInline: Boolean = false,
        block: (IrClass) -> Unit = {}
    ): IrClassSymbol =
        IrClassBuilder().run {
            name = fqName.shortName()
            kind = classKind
            modality = classModality
            isInline = classIsInline
            irFactory.createClass(
                startOffset, endOffset, origin,
                IrClassPublicSymbolImpl(IdSignature.PublicSignature(fqName.parent().asString(), fqName.shortName().asString(), null, 0)),
                name, kind, visibility, modality,
                isCompanion, isInner, isData, isExternal, isInline, isExpect, isFun
            )
        }.also {
            it.parent = parent
            it.createImplicitParameterDeclarationWithWrappedDescriptor()
            block(it)
        }.symbol

    private fun createFunction(
        signature: IdSignature,
        name: String,
        returnType: IrType,
        valueParameterTypes: Array<out IrType>,
        parent: IrDeclarationParent,
        origin: IrDeclarationOrigin = BUILTIN_OPERATOR
    ) = IrFunctionBuilder().run {
        this.name = Name.identifier(name)
        this.returnType = returnType
        this.origin = origin
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

/*
 * Copyright 2010-2021 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.fir.backend

import org.jetbrains.kotlin.backend.common.ir.createImplicitParameterDeclarationWithWrappedDescriptor
import org.jetbrains.kotlin.builtins.PrimitiveType
import org.jetbrains.kotlin.config.LanguageVersionSettings
import org.jetbrains.kotlin.descriptors.ClassKind
import org.jetbrains.kotlin.descriptors.Modality
import org.jetbrains.kotlin.fir.descriptors.FirModuleDescriptor
import org.jetbrains.kotlin.fir.resolve.symbolProvider
import org.jetbrains.kotlin.fir.symbols.impl.FirClassSymbol
import org.jetbrains.kotlin.ir.BuiltInOperatorNames
import org.jetbrains.kotlin.ir.IrBuiltIns
import org.jetbrains.kotlin.ir.builders.declarations.addFunction
import org.jetbrains.kotlin.ir.builders.declarations.addValueParameter
import org.jetbrains.kotlin.ir.builders.declarations.buildClass
import org.jetbrains.kotlin.ir.declarations.IrClass
import org.jetbrains.kotlin.ir.declarations.IrDeclarationParent
import org.jetbrains.kotlin.ir.declarations.IrFactory
import org.jetbrains.kotlin.ir.declarations.IrPackageFragment
import org.jetbrains.kotlin.ir.declarations.impl.IrExternalPackageFragmentImpl
import org.jetbrains.kotlin.ir.symbols.IrClassSymbol
import org.jetbrains.kotlin.ir.symbols.IrClassifierSymbol
import org.jetbrains.kotlin.ir.symbols.IrSimpleFunctionSymbol
import org.jetbrains.kotlin.ir.types.*
import org.jetbrains.kotlin.ir.util.SymbolTable
import org.jetbrains.kotlin.ir.util.classId
import org.jetbrains.kotlin.name.ClassId
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.FqNameUnsafe
import org.jetbrains.kotlin.name.Name

class IrBuiltInsOverFir(
    private val components: Fir2IrComponents,
    override val languageVersionSettings: LanguageVersionSettings,
    private val moduleDescriptor: FirModuleDescriptor
) : IrBuiltIns() {

    override val irFactory: IrFactory = components.symbolTable.irFactory

    private val kotlinPackage = FqName("kotlin")
    private val javaLangPackage = FqName("java.lang")
    private val kotlinCollectionsPackage = FqName("kotlin.collections")

    private val internalIrPackage = createPackage(KOTLIN_INTERNAL_IR_FQN)

    override val anyClass: IrClassSymbol = referenceClassByFqname(kotlinPackage, "Any")!!
    override val anyType: IrType = anyClass.defaultType
    override val anyNType = anyType.withHasQuestionMark(true)

    override val booleanType: IrType get() = booleanClass.defaultType
    override val booleanClass: IrClassSymbol = referenceClassByFqname(javaLangPackage, "Boolean")!!
    override val charType: IrType get() = charClass.defaultType
    override val charClass: IrClassSymbol = referenceClassByFqname(kotlinPackage, "Char")!!

    override val numberClass: IrClassSymbol = referenceClassByFqname(kotlinPackage, "Number")!!
    override val numberType: IrType get() = numberClass.defaultType

    override val byteType: IrType get() = byteClass.defaultType
    override val byteClass: IrClassSymbol = referenceClassByFqname(javaLangPackage, "Byte")!!
    override val shortType: IrType get() = shortClass.defaultType
    override val shortClass: IrClassSymbol = referenceClassByFqname(javaLangPackage, "Short")!!
    override val intType: IrType get() = intClass.defaultType
    override val intClass: IrClassSymbol = referenceClassByFqname(javaLangPackage, "Integer")!!
    override val longType: IrType get() = longClass.defaultType
    override val longClass: IrClassSymbol = referenceClassByFqname(javaLangPackage, "Long")!!
    override val floatType: IrType get() = floatClass.defaultType
    override val floatClass: IrClassSymbol = referenceClassByFqname(javaLangPackage, "Float")!!
    override val doubleType: IrType get() = doubleClass.defaultType
    override val doubleClass: IrClassSymbol = referenceClassByFqname(javaLangPackage, "Double")!!
    override val nothingType: IrType get() = nothingClass.defaultType
    override val nothingClass: IrClassSymbol = referenceClassByFqname(kotlinPackage, "Nothing")!!
    override val nothingNType: IrType = nothingType.withHasQuestionMark(true)

    override val unitType: IrType get() = unitClass.defaultType
    override val unitClass: IrClassSymbol = referenceClassByFqname(kotlinPackage, "Unit")!!
    override val stringType: IrType get() = stringClass.defaultType
    override val stringClass: IrClassSymbol = referenceClassByFqname(javaLangPackage, "String")!!

    override val charSequenceClass: IrClassSymbol = referenceClassByFqname(kotlinPackage, "CharSequence")!!

    override val collectionClass: IrClassSymbol by lazy { referenceClassByFqname(kotlinCollectionsPackage, "Collection")!! }
    override val arrayClass: IrClassSymbol by lazy { referenceClassByFqname(kotlinPackage, "Array")!! }
    override val setClass: IrClassSymbol by lazy { referenceClassByFqname(kotlinCollectionsPackage, "Set")!! }
    override val listClass: IrClassSymbol by lazy { referenceClassByFqname(kotlinCollectionsPackage, "List")!! }
    override val mapClass: IrClassSymbol by lazy { referenceClassByFqname(kotlinCollectionsPackage, "Map")!! }
    override val mapEntryClass: IrClassSymbol by lazy {
        referenceClassByFqname(
            mapClass.owner.classId!!.createNestedClassId(
                Name.identifier(
                    "MapEntry"
                )
            )
        )!!
    }

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

    override val iterableClass: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val listIteratorClass: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val mutableCollectionClass: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val mutableSetClass: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val mutableListClass: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val mutableMapClass: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val mutableMapEntryClass: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val mutableIterableClass: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val mutableIteratorClass: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val mutableListIteratorClass: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val comparableClass: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val throwableType: IrType
        get() = TODO("Not yet implemented")
    override val throwableClass: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val kCallableClass: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val kPropertyClass: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val kClassClass: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val kProperty0Class: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val kProperty1Class: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val kProperty2Class: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val kMutableProperty0Class: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val kMutableProperty1Class: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val kMutableProperty2Class: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val functionClass: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val kFunctionClass: IrClassSymbol
        get() = TODO("Not yet implemented")

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

    override lateinit var booleanNotSymbol: IrSimpleFunctionSymbol private set
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

    private val internalIrPackageFragment: IrClassSymbol =
        createClass(KOTLIN_INTERNAL_IR_FQN.child(Name.identifier("InternalKt")), internalIrPackage) { klass ->

            fun addBuiltinFunction(name: String, returnType: IrType, vararg valueParameterTypes: IrType) =
                klass.addFunction(name, returnType, isStatic = true).also { fn ->
                    valueParameterTypes.forEachIndexed { index, irType ->
                        fn.addValueParameter(Name.identifier("arg$index"), irType)
                    }
                }.symbol

            primitiveFloatingPointIrTypes.forEach { fpType ->
                _ieee754equalsFunByOperandType.put(
                    fpType.classifierOrFail,
                    addBuiltinFunction(BuiltInOperatorNames.IEEE754_EQUALS, booleanType, fpType.makeNullable(), fpType.makeNullable())
                )
            }
            booleanNotSymbol = addBuiltinFunction(BuiltInOperatorNames.EQEQEQ, booleanType, anyNType, anyNType)
            eqeqeqSymbol = addBuiltinFunction(BuiltInOperatorNames.EQEQEQ, booleanType, anyNType, anyNType)
            eqeqSymbol = addBuiltinFunction(BuiltInOperatorNames.EQEQ, booleanType, anyNType, anyNType)
            throwCceSymbol = addBuiltinFunction(BuiltInOperatorNames.THROW_CCE, nothingType)
            throwIseSymbol = addBuiltinFunction(BuiltInOperatorNames.THROW_ISE, nothingType)
            andandSymbol = addBuiltinFunction(BuiltInOperatorNames.ANDAND, booleanType, booleanType, booleanType)
            ororSymbol = addBuiltinFunction(BuiltInOperatorNames.OROR, booleanType, booleanType, booleanType)
            noWhenBranchMatchedExceptionSymbol = addBuiltinFunction(BuiltInOperatorNames.NO_WHEN_BRANCH_MATCHED_EXCEPTION, nothingType)
            illegalArgumentExceptionSymbol = addBuiltinFunction(BuiltInOperatorNames.ILLEGAL_ARGUMENT_EXCEPTION, nothingType, stringType)
            dataClassArrayMemberHashCodeSymbol = addBuiltinFunction("dataClassArrayMemberHashCode", intType, anyType)
            dataClassArrayMemberToStringSymbol = addBuiltinFunction("dataClassArrayMemberToString", stringType, anyNType)
        }

    override val unsignedArrays: Set<IrClassSymbol>
        get() = TODO("Not yet implemented")
    override val lessFunByOperandType: Map<IrClassifierSymbol, IrSimpleFunctionSymbol>
        get() = TODO("Not yet implemented")
    override val lessOrEqualFunByOperandType: Map<IrClassifierSymbol, IrSimpleFunctionSymbol>
        get() = TODO("Not yet implemented")
    override val greaterOrEqualFunByOperandType: Map<IrClassifierSymbol, IrSimpleFunctionSymbol>
        get() = TODO("Not yet implemented")
    override val greaterFunByOperandType: Map<IrClassifierSymbol, IrSimpleFunctionSymbol>
        get() = TODO("Not yet implemented")

    override val checkNotNullSymbol: IrSimpleFunctionSymbol
        get() = TODO("Not yet implemented")

    override fun getKPropertyClass(mutable: Boolean, n: Int): IrClassSymbol {
        TODO("Not yet implemented")
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
    override val arrayOfNulls: IrSimpleFunctionSymbol
        get() = TODO("Not yet implemented")
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

    override fun findFunctions(name: Name, vararg packageNameSegments: String): Iterable<IrSimpleFunctionSymbol> {
        TODO("Not yet implemented")
    }

    override fun findClass(name: Name, vararg packageNameSegments: String): IrClassSymbol? {
        TODO("Not yet implemented")
    }

    override fun getBinaryOperator(name: Name, lhsType: IrType, rhsType: IrType): IrSimpleFunctionSymbol {
        TODO("Not yet implemented")
    }

    override fun getUnaryOperator(name: Name, receiverType: IrType): IrSimpleFunctionSymbol {
        TODO("Not yet implemented")
    }

    override val getProgressionLastElementByReturnType: Map<IrClassifierSymbol?, IrSimpleFunctionSymbol>
        get() = TODO("Not yet implemented")

    private fun referenceClassByFqname(packageName: FqName, identifier: Name) =
        referenceClassByFqname(ClassId(packageName, identifier))

    private fun referenceClassByFqname(packageName: FqName, identifier: String) =
        referenceClassByFqname(ClassId(packageName, Name.identifier(identifier)))

    private fun referenceClassByFqname(classId: ClassId): IrClassSymbol? {
        val firSymbol = components.session.symbolProvider.getClassLikeSymbolByFqName(classId) ?: return null
        val firClassSymbol = firSymbol as? FirClassSymbol ?: return null
        return components.classifierStorage.getIrClassSymbol(firClassSymbol)
    }

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
        irFactory.buildClass {
            name = fqName.shortName()
            kind = classKind
            modality = classModality
            isInline = classIsInline
        }.also {
            it.parent = parent
            block(it)
        }.symbol


    private fun findOperator(packageName: FqName, name: String, returnType: IrType, valueParameterTypes: List<IrType>): IrSimpleFunctionSymbol {
        for (firOpSymbol in components.session.symbolProvider.getTopLevelFunctionSymbols(packageName, Name.identifier(name))) {
            val irSymbol = components.declarationStorage.getIrFunctionSymbol(firOpSymbol) as? IrSimpleFunctionSymbol ?: continue
            if (irSymbol.owner.let {
                    it.returnType == returnType && it.valueParameters.size == valueParameterTypes.size &&
                            it.valueParameters.zip(valueParameterTypes).all { it.first.type == it.second }
                }
            ) {
                return irSymbol
            }
        }
        error("no op $name found in $packageName")
    }
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

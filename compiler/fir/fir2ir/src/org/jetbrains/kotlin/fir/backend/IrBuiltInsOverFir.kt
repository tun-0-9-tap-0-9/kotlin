/*
 * Copyright 2010-2021 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.fir.backend

import org.jetbrains.kotlin.builtins.PrimitiveType
import org.jetbrains.kotlin.config.LanguageVersionSettings
import org.jetbrains.kotlin.fir.descriptors.FirModuleDescriptor
import org.jetbrains.kotlin.fir.resolve.symbolProvider
import org.jetbrains.kotlin.fir.symbols.impl.FirClassSymbol
import org.jetbrains.kotlin.ir.IrBuiltIns
import org.jetbrains.kotlin.ir.declarations.IrClass
import org.jetbrains.kotlin.ir.declarations.IrFactory
import org.jetbrains.kotlin.ir.declarations.IrPackageFragment
import org.jetbrains.kotlin.ir.declarations.impl.IrExternalPackageFragmentImpl
import org.jetbrains.kotlin.ir.symbols.IrClassSymbol
import org.jetbrains.kotlin.ir.symbols.IrClassifierSymbol
import org.jetbrains.kotlin.ir.symbols.IrSimpleFunctionSymbol
import org.jetbrains.kotlin.ir.types.IrType
import org.jetbrains.kotlin.ir.types.defaultType
import org.jetbrains.kotlin.ir.types.withHasQuestionMark
import org.jetbrains.kotlin.ir.util.SymbolTable
import org.jetbrains.kotlin.name.ClassId
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name

class IrBuiltInsOverFir(
    private val components: Fir2IrComponents,
    override val languageVersionSettings: LanguageVersionSettings,
    private val moduleDescriptor: FirModuleDescriptor
) : IrBuiltIns() {

    private val kotlinPackage = FqName("kotlin")
    private val javaLangPackage = FqName("java.lang")
    private val kotlinCollectionsPackage = FqName("kotlin.collections")

    override val irFactory: IrFactory = components.symbolTable.irFactory

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
//    override val mapEntryClass: IrClassSymbol = referenceClassByFqname(ClassId(kotlinPackageFqName, Name.identifier("MapEntry")))!!
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

    override val mapEntryClass: IrClassSymbol
        get() = TODO("Not yet implemented")
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
    override val primitiveTypeToIrType: Map<PrimitiveType, IrType>
        get() = TODO("Not yet implemented")
    override val primitiveIrTypes: List<IrType>
        get() = TODO("Not yet implemented")
    override val primitiveIrTypesWithComparisons: List<IrType>
        get() = TODO("Not yet implemented")
    override val primitiveFloatingPointIrTypes: List<IrType>
        get() = TODO("Not yet implemented")
    override val byteArray: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val charArray: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val shortArray: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val intArray: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val longArray: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val floatArray: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val doubleArray: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val booleanArray: IrClassSymbol
        get() = TODO("Not yet implemented")
    override val primitiveArraysToPrimitiveTypes: Map<IrClassSymbol, PrimitiveType>
        get() = TODO("Not yet implemented")
    override val primitiveArrays: Set<IrClassSymbol>
        get() = TODO("Not yet implemented")
    override val primitiveArrayElementTypes: Map<IrClassSymbol, IrType?>
        get() = TODO("Not yet implemented")
    override val primitiveArrayForType: Map<IrType?, IrClassSymbol>
        get() = TODO("Not yet implemented")
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
    override val ieee754equalsFunByOperandType: Map<IrClassifierSymbol, IrSimpleFunctionSymbol>
        get() = TODO("Not yet implemented")
    override val booleanNotSymbol: IrSimpleFunctionSymbol
        get() = TODO("Not yet implemented")
    override val eqeqeqSymbol: IrSimpleFunctionSymbol
        get() = TODO("Not yet implemented")
    override val eqeqSymbol: IrSimpleFunctionSymbol
        get() = TODO("Not yet implemented")
    override val throwCceSymbol: IrSimpleFunctionSymbol
        get() = TODO("Not yet implemented")
    override val throwIseSymbol: IrSimpleFunctionSymbol
        get() = TODO("Not yet implemented")
    override val andandSymbol: IrSimpleFunctionSymbol
        get() = TODO("Not yet implemented")
    override val ororSymbol: IrSimpleFunctionSymbol
        get() = TODO("Not yet implemented")
    override val noWhenBranchMatchedExceptionSymbol: IrSimpleFunctionSymbol
        get() = TODO("Not yet implemented")
    override val illegalArgumentExceptionSymbol: IrSimpleFunctionSymbol
        get() = TODO("Not yet implemented")
    override val checkNotNullSymbol: IrSimpleFunctionSymbol
        get() = TODO("Not yet implemented")
    override val dataClassArrayMemberHashCodeSymbol: IrSimpleFunctionSymbol
        get() = TODO("Not yet implemented")
    override val dataClassArrayMemberToStringSymbol: IrSimpleFunctionSymbol
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
    
    private fun referenceClassByFqname(packageName: FqName, identifier: String) =
        referenceClassByFqname(ClassId(packageName, Name.identifier(identifier)))

    private fun referenceClassByFqname(classId: ClassId): IrClassSymbol? {
        val firSymbol = components.session.symbolProvider.getClassLikeSymbolByFqName(classId) ?: return null
        val firClassSymbol = firSymbol as? FirClassSymbol ?: return null
        return components.classifierStorage.getIrClassSymbol(firClassSymbol)
    }

    private fun createPackage(fqName: FqName): IrPackageFragment =
        IrExternalPackageFragmentImpl.createEmptyExternalPackageFragment(moduleDescriptor, fqName)
}
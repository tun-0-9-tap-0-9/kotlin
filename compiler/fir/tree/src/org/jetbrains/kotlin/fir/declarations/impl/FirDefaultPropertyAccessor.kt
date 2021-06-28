/*
 * Copyright 2000-2018 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.fir.declarations.impl

import org.jetbrains.kotlin.descriptors.EffectiveVisibility
import org.jetbrains.kotlin.descriptors.Modality
import org.jetbrains.kotlin.descriptors.Visibility
import org.jetbrains.kotlin.fir.FirImplementationDetail
import org.jetbrains.kotlin.fir.FirModuleData
import org.jetbrains.kotlin.fir.FirSourceElement
import org.jetbrains.kotlin.fir.contracts.impl.FirEmptyContractDescription
import org.jetbrains.kotlin.fir.declarations.*
import org.jetbrains.kotlin.fir.declarations.builder.buildDefaultSetterValueParameter
import org.jetbrains.kotlin.fir.expressions.FirBlock
import org.jetbrains.kotlin.fir.symbols.impl.FirPropertyAccessorSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirVariableSymbol
import org.jetbrains.kotlin.fir.types.FirTypeRef
import org.jetbrains.kotlin.fir.types.impl.FirImplicitUnitTypeRef
import org.jetbrains.kotlin.name.CallableId
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name

@OptIn(FirImplementationDetail::class)
abstract class FirDefaultPropertyAccessor(
    source: FirSourceElement?,
    moduleData: FirModuleData,
    origin: FirDeclarationOrigin,
    propertyTypeRef: FirTypeRef,
    valueParameters: MutableList<FirValueParameter>,
    isGetter: Boolean,
    visibility: Visibility,
    modality: Modality = Modality.FINAL,
    effectiveVisibility: EffectiveVisibility? = null,
    symbol: FirPropertyAccessorSymbol
) : FirPropertyAccessorImpl(
    source,
    moduleData,
    resolvePhase = if (effectiveVisibility != null) FirResolvePhase.BODY_RESOLVE else FirResolvePhase.TYPES,
    origin,
    FirDeclarationAttributes(),
    propertyTypeRef,
    valueParameters,
    body = null,
    status = if (effectiveVisibility == null)
        FirDeclarationStatusImpl(visibility, modality)
    else
        FirResolvedDeclarationStatusImpl(visibility, modality, effectiveVisibility),
    containerSource = null,
    dispatchReceiverType = null,
    contractDescription = FirEmptyContractDescription,
    symbol,
    isGetter,
    annotations = mutableListOf(),
    typeParameters = mutableListOf(),
) {
    override var resolvePhase
        get() = if (status is FirResolvedDeclarationStatus) FirResolvePhase.BODY_RESOLVE else FirResolvePhase.TYPES
        set(_) {}

    final override var body: FirBlock?
        get() = null
        set(_) {}

    companion object {
        fun createGetterOrSetter(
            source: FirSourceElement?,
            moduleData: FirModuleData,
            origin: FirDeclarationOrigin,
            propertyTypeRef: FirTypeRef,
            visibility: Visibility,
            isGetter: Boolean
        ): FirDefaultPropertyAccessor {
            return if (isGetter) {
                FirDefaultPropertyGetter(source, moduleData, origin, propertyTypeRef, visibility, Modality.FINAL)
            } else {
                FirDefaultPropertySetter(source, moduleData, origin, propertyTypeRef, visibility, Modality.FINAL)
            }
        }
    }
}

class FirDefaultPropertyGetter(
    source: FirSourceElement?,
    moduleData: FirModuleData,
    origin: FirDeclarationOrigin,
    propertyTypeRef: FirTypeRef,
    visibility: Visibility,
    modality: Modality = Modality.FINAL,
    effectiveVisibility: EffectiveVisibility? = null,
    symbol: FirPropertyAccessorSymbol = FirPropertyAccessorSymbol()
) : FirDefaultPropertyAccessor(
    source,
    moduleData,
    origin,
    propertyTypeRef,
    valueParameters = mutableListOf(),
    isGetter = true,
    visibility = visibility,
    modality = modality,
    effectiveVisibility = effectiveVisibility,
    symbol = symbol
)

class FirDefaultPropertySetter(
    source: FirSourceElement?,
    moduleData: FirModuleData,
    origin: FirDeclarationOrigin,
    propertyTypeRef: FirTypeRef,
    visibility: Visibility,
    modality: Modality = Modality.FINAL,
    effectiveVisibility: EffectiveVisibility? = null,
    symbol: FirPropertyAccessorSymbol = FirPropertyAccessorSymbol()
) : FirDefaultPropertyAccessor(
    source,
    moduleData,
    origin,
    FirImplicitUnitTypeRef(source),
    valueParameters = mutableListOf(
        buildDefaultSetterValueParameter builder@{
            this@builder.source = source
            this@builder.moduleData = moduleData
            this@builder.origin = origin
            this@builder.returnTypeRef = propertyTypeRef
            this@builder.symbol = FirVariableSymbol(CallableId(FqName.ROOT, Name.special("<default-setter-parameter>")))
        }
    ),
    isGetter = false,
    visibility = visibility,
    modality = modality,
    effectiveVisibility = effectiveVisibility,
    symbol = symbol
)

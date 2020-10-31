/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2019-2020 Fabrice TIERCELIN - initial API and implementation
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.   See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program under LICENSE-GNUGPL.   If not, see
 * <http://www.gnu.org/licenses/>.
 *
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution under LICENSE-ECLIPSE, and is
 * available at http://www.eclipse.org/legal/epl-v10.html
 */
package org.autorefactor.jdt.internal.ui.fix;

import org.eclipse.osgi.util.NLS;

/**
 * Multi fix messages.
 *
 * @since 1.11
 */
public final class MultiFixMessages extends NLS {
    private static final String BUNDLE_NAME= "org.autorefactor.jdt.internal.ui.fix.MultiFixMessages"; //$NON-NLS-1$

    private MultiFixMessages() {
    }

    /**
     * Automatically filled.
     */
    public static String IntPrimitiveRatherThanWrapperCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String IntPrimitiveRatherThanWrapperCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String IntPrimitiveRatherThanWrapperCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String ShortPrimitiveRatherThanWrapperCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String ShortPrimitiveRatherThanWrapperCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String ShortPrimitiveRatherThanWrapperCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String LongPrimitiveRatherThanWrapperCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String LongPrimitiveRatherThanWrapperCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String LongPrimitiveRatherThanWrapperCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String DoublePrimitiveRatherThanWrapperCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String DoublePrimitiveRatherThanWrapperCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String DoublePrimitiveRatherThanWrapperCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String FloatPrimitiveRatherThanWrapperCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String FloatPrimitiveRatherThanWrapperCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String FloatPrimitiveRatherThanWrapperCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String CharPrimitiveRatherThanWrapperCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String CharPrimitiveRatherThanWrapperCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String CharPrimitiveRatherThanWrapperCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String BytePrimitiveRatherThanWrapperCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String BytePrimitiveRatherThanWrapperCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String BytePrimitiveRatherThanWrapperCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String AssignRatherThanFilterThenAssignAnywayCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String AssignRatherThanFilterThenAssignAnywayCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String AssignRatherThanFilterThenAssignAnywayCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String AssignRatherThanTernaryFilterThenAssignAnywayCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String AssignRatherThanTernaryFilterThenAssignAnywayCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String AssignRatherThanTernaryFilterThenAssignAnywayCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String VectorOldToNewAPICleanUp_name;
    /**
     * Automatically filled.
     */
    public static String VectorOldToNewAPICleanUp_description;
    /**
     * Automatically filled.
     */
    public static String VectorOldToNewAPICleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String PrimitiveWrapperCreationCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String PrimitiveWrapperCreationCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String PrimitiveWrapperCreationCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String AutoBoxingRatherThanExplicitMethodCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String AutoBoxingRatherThanExplicitMethodCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String AutoBoxingRatherThanExplicitMethodCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String UnboxingRatherThanExplicitMethodCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String UnboxingRatherThanExplicitMethodCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String UnboxingRatherThanExplicitMethodCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String BracketsRatherThanArrayInstantiationCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String BracketsRatherThanArrayInstantiationCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String BracketsRatherThanArrayInstantiationCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String LambdaExpressionRatherThanComparatorCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String LambdaExpressionRatherThanComparatorCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String LambdaExpressionRatherThanComparatorCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String LambdaCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String LambdaCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String LambdaCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String LiteralRatherThanBooleanConstantCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String LiteralRatherThanBooleanConstantCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String LiteralRatherThanBooleanConstantCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String BooleanCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String BooleanCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String BooleanCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String BooleanPrimitiveRatherThanWrapperCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String BooleanPrimitiveRatherThanWrapperCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String BooleanPrimitiveRatherThanWrapperCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String LazyLogicalRatherThanEagerCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String LazyLogicalRatherThanEagerCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String LazyLogicalRatherThanEagerCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String BooleanConstantRatherThanValueOfCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String BooleanConstantRatherThanValueOfCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String BooleanConstantRatherThanValueOfCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String BooleanEqualsRatherThanNullCheckCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String BooleanEqualsRatherThanNullCheckCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String BooleanEqualsRatherThanNullCheckCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String XORRatherThanDuplicateConditionsCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String XORRatherThanDuplicateConditionsCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String XORRatherThanDuplicateConditionsCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String ORConditionRatherThanRedundantClausesCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String ORConditionRatherThanRedundantClausesCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String ORConditionRatherThanRedundantClausesCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String TernaryOperatorRatherThanDuplicateConditionsCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String TernaryOperatorRatherThanDuplicateConditionsCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String TernaryOperatorRatherThanDuplicateConditionsCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String InvertEqualsCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String InvertEqualsCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String InvertEqualsCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String ComparisonCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String ComparisonCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String ComparisonCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String DoubleCompareRatherThanEqualityCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String DoubleCompareRatherThanEqualityCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String DoubleCompareRatherThanEqualityCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String RemoveUnneededThisExpressionCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String RemoveUnneededThisExpressionCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String RemoveUnneededThisExpressionCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String AggregateConstructorRatherThanGWTMethodCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String AggregateConstructorRatherThanGWTMethodCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String AggregateConstructorRatherThanGWTMethodCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String StandardMethodRatherThanLibraryMethodCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String StandardMethodRatherThanLibraryMethodCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String StandardMethodRatherThanLibraryMethodCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String InstanceofRatherThanIsInstanceCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String InstanceofRatherThanIsInstanceCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String InstanceofRatherThanIsInstanceCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String StringRatherThanNewStringCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String StringRatherThanNewStringCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String StringRatherThanNewStringCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String StringCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String StringCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String StringCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String StringValueOfRatherThanConcatCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String StringValueOfRatherThanConcatCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String StringValueOfRatherThanConcatCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String SubstringWithOneParameterRatherThanTwoCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String SubstringWithOneParameterRatherThanTwoCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String SubstringWithOneParameterRatherThanTwoCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String BigNumberCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String BigNumberCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String BigNumberCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String OppositeComparisonRatherThanNegativeExpressionCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String OppositeComparisonRatherThanNegativeExpressionCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String OppositeComparisonRatherThanNegativeExpressionCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String PrimitiveComparisonRatherThanWrapperComparisonCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String PrimitiveComparisonRatherThanWrapperComparisonCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String PrimitiveComparisonRatherThanWrapperComparisonCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String SerializeRatherThanBoxingAndSerializeCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String SerializeRatherThanBoxingAndSerializeCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String SerializeRatherThanBoxingAndSerializeCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String RemoveEmptyIfCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String RemoveEmptyIfCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String RemoveEmptyIfCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String NoLoopIterationRatherThanEmptyCheckCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String NoLoopIterationRatherThanEmptyCheckCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String NoLoopIterationRatherThanEmptyCheckCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String InlineCodeRatherThanPeremptoryConditionCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String InlineCodeRatherThanPeremptoryConditionCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String InlineCodeRatherThanPeremptoryConditionCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String RemoveUselessBlockCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String RemoveUselessBlockCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String RemoveUselessBlockCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String RemoveEmptyStatementCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String RemoveEmptyStatementCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String RemoveEmptyStatementCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String SingleDeclarationsRatherThanMultiDeclarationCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String SingleDeclarationsRatherThanMultiDeclarationCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String SingleDeclarationsRatherThanMultiDeclarationCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String EndOfMethodRatherThanReturnCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String EndOfMethodRatherThanReturnCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String EndOfMethodRatherThanReturnCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String EndOfLoopRatherThanContinueCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String EndOfLoopRatherThanContinueCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String EndOfLoopRatherThanContinueCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String WhileConditionRatherThanInnerIfCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String WhileConditionRatherThanInnerIfCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String WhileConditionRatherThanInnerIfCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String DoWhileRatherThanWhileCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String DoWhileRatherThanWhileCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String DoWhileRatherThanWhileCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String DoWhileRatherThanDuplicateCodeCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String DoWhileRatherThanDuplicateCodeCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String DoWhileRatherThanDuplicateCodeCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String IfRatherThanWhileAndFallsThroughCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String IfRatherThanWhileAndFallsThroughCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String IfRatherThanWhileAndFallsThroughCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String SuperCallRatherThanUselessOverridingCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String SuperCallRatherThanUselessOverridingCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String SuperCallRatherThanUselessOverridingCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String AndConditionRatherThanEmbededIfCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String AndConditionRatherThanEmbededIfCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String AndConditionRatherThanEmbededIfCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String CommonCodeInIfElseStatementCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String CommonCodeInIfElseStatementCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String CommonCodeInIfElseStatementCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String OppositeConditionRatherThanDuplicateConditionCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String OppositeConditionRatherThanDuplicateConditionCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String OppositeConditionRatherThanDuplicateConditionCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String OneConditionRatherThanUnreachableBlockCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String OneConditionRatherThanUnreachableBlockCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String OneConditionRatherThanUnreachableBlockCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String MergeConditionalBlocksCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String MergeConditionalBlocksCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String MergeConditionalBlocksCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String OneIfRatherThanDuplicateBlocksThatFallThroughCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String OneIfRatherThanDuplicateBlocksThatFallThroughCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String OneIfRatherThanDuplicateBlocksThatFallThroughCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String OutsideCodeRatherThanFallingThroughBlocksCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String OutsideCodeRatherThanFallingThroughBlocksCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String OutsideCodeRatherThanFallingThroughBlocksCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String ElseRatherThanOppositeConditionCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String ElseRatherThanOppositeConditionCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String ElseRatherThanOppositeConditionCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String GenericMapRatherThanRawMapCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String GenericMapRatherThanRawMapCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String GenericMapRatherThanRawMapCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String GenericListRatherThanRawListCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String GenericListRatherThanRawListCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String GenericListRatherThanRawListCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String UseDiamondOperatorCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String UseDiamondOperatorCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String NIORatherThanIOCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String NIORatherThanIOCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String NIORatherThanIOCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String UseDiamondOperatorCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String UseMultiCatchCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String UseMultiCatchCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String UseMultiCatchCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String ContainsRatherThanLoopCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String ContainsRatherThanLoopCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String ContainsRatherThanLoopCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String ContainsAllRatherThanLoopCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String ContainsAllRatherThanLoopCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String ContainsAllRatherThanLoopCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String DisjointRatherThanLoopCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String DisjointRatherThanLoopCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String DisjointRatherThanLoopCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String CollectionCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String CollectionCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String CollectionCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String AddAllRatherThanLoopCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String AddAllRatherThanLoopCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String AddAllRatherThanLoopCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String FillRatherThanLoopCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String FillRatherThanLoopCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String FillRatherThanLoopCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String JoinRatherThanLoopCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String JoinRatherThanLoopCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String JoinRatherThanLoopCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String ObjectsEqualsRatherThanEqualsAndNullCheckCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String ObjectsEqualsRatherThanEqualsAndNullCheckCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String ObjectsEqualsRatherThanEqualsAndNullCheckCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String BreakRatherThanPassiveIterationsCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String BreakRatherThanPassiveIterationsCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String BreakRatherThanPassiveIterationsCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String UpdateSetRatherThanTestingFirstCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String UpdateSetRatherThanTestingFirstCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String UpdateSetRatherThanTestingFirstCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String IsEmptyRatherThanSizeCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String IsEmptyRatherThanSizeCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String IsEmptyRatherThanSizeCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String ReduceIndentationCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String ReduceIndentationCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String ReduceIndentationCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String VariableInsideIfRatherThanAboveCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String VariableInsideIfRatherThanAboveCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String VariableInsideIfRatherThanAboveCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String MapCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String MapCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String MapCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String EntrySetRatherThanKeySetAndValueSearchCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String EntrySetRatherThanKeySetAndValueSearchCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String EntrySetRatherThanKeySetAndValueSearchCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String MethodOnMapRatherThanMethodOnKeySetCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String MethodOnMapRatherThanMethodOnKeySetCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String MethodOnMapRatherThanMethodOnKeySetCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String NoAssignmentInIfConditionCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String NoAssignmentInIfConditionCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String NoAssignmentInIfConditionCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String IncrementStatementRatherThanIncrementExpressionCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String IncrementStatementRatherThanIncrementExpressionCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String IncrementStatementRatherThanIncrementExpressionCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String DeclarationOutsideLoopRatherThanInsideCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String DeclarationOutsideLoopRatherThanInsideCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String DeclarationOutsideLoopRatherThanInsideCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String IfElseIfCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String IfElseIfCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String IfElseIfCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String CommonIfInIfElseCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String CommonIfInIfElseCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String CommonIfInIfElseCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String StringBuilderCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String StringBuilderCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String StringBuilderCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String StringBuilderMethodRatherThanReassignationCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String StringBuilderMethodRatherThanReassignationCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String StringBuilderMethodRatherThanReassignationCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String StringBuilderRatherThanStringBufferCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String StringBuilderRatherThanStringBufferCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String StringBuilderRatherThanStringBufferCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String StringBuilderRatherThanStringCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String StringBuilderRatherThanStringCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String StringBuilderRatherThanStringCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String AtomicObjectRatherThanMonoIndexArrayCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String AtomicObjectRatherThanMonoIndexArrayCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String AtomicObjectRatherThanMonoIndexArrayCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String PatternRatherThanRegExStringCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String PatternRatherThanRegExStringCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String PatternRatherThanRegExStringCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String OptimizeRegExCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String OptimizeRegExCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String OptimizeRegExCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String CollectionsAddAllRatherThanAsListCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String CollectionsAddAllRatherThanAsListCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String CollectionsAddAllRatherThanAsListCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String HashMapRatherThanHashtableCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String HashMapRatherThanHashtableCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String HashMapRatherThanHashtableCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String ArrayListRatherThanVectorCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String ArrayListRatherThanVectorCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String ArrayListRatherThanVectorCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String ArrayDequeRatherThanStackCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String ArrayDequeRatherThanStackCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String ArrayDequeRatherThanStackCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String SetRatherThanMapCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String SetRatherThanMapCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String SetRatherThanMapCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String ArrayListRatherThanLinkedListCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String ArrayListRatherThanLinkedListCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String ArrayListRatherThanLinkedListCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String SetRatherThanListCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String SetRatherThanListCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String SetRatherThanListCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String HashMapRatherThanTreeMapCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String HashMapRatherThanTreeMapCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String HashMapRatherThanTreeMapCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String HashSetRatherThanTreeSetCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String HashSetRatherThanTreeSetCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String HashSetRatherThanTreeSetCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String UseStringContainsCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String UseStringContainsCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String UseStringContainsCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String CommentsCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String CommentsCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String CommentsCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String RemoveFieldsDefaultValuesCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String RemoveFieldsDefaultValuesCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String RemoveFieldsDefaultValuesCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String StaticConstantRatherThanInstanceConstantCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String StaticConstantRatherThanInstanceConstantCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String StaticConstantRatherThanInstanceConstantCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String RemoveOverriddenAssignmentCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String RemoveOverriddenAssignmentCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String RemoveOverriddenAssignmentCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String Java7HashRatherThanEclipseJava6HashCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String Java7HashRatherThanEclipseJava6HashCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String Java7HashRatherThanEclipseJava6HashCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String AnnotationCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String AnnotationCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String AnnotationCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String TryWithResourceCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String TryWithResourceCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String TryWithResourceCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String OneTryRatherThanTwoCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String OneTryRatherThanTwoCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String OneTryRatherThanTwoCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String TestNGAssertCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String TestNGAssertCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String TestNGAssertCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String JupiterAssertCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String JupiterAssertCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String JupiterAssertCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String JUnitAssertCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String JUnitAssertCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String JUnitAssertCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String AssertJCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String AssertJCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String AssertJCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String SeparateAssertionsRatherThanBooleanExpressionCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String SeparateAssertionsRatherThanBooleanExpressionCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String SeparateAssertionsRatherThanBooleanExpressionCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String RemoveEmptyLinesCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String RemoveEmptyLinesCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String RemoveEmptyLinesCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String RemoveEmptySuperConstrInvocationCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String RemoveEmptySuperConstrInvocationCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String RemoveEmptySuperConstrInvocationCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String ImplicitDefaultConstructorRatherThanWrittenOneCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String ImplicitDefaultConstructorRatherThanWrittenOneCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String ImplicitDefaultConstructorRatherThanWrittenOneCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String AndroidWakeLockCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String AndroidWakeLockCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String AndroidWakeLockCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String AndroidViewHolderCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String AndroidViewHolderCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String AndroidViewHolderCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String LogParametersRatherThanLogMessageCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String LogParametersRatherThanLogMessageCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String LogParametersRatherThanLogMessageCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String NamedMethodRatherThanLogLevelParameterCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String NamedMethodRatherThanLogLevelParameterCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String NamedMethodRatherThanLogLevelParameterCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String EnumMapRatherThanHashMapCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String EnumMapRatherThanHashMapCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String EnumMapRatherThanHashMapCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String EnumSetRatherThanHashSetCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String EnumSetRatherThanHashSetCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String EnumSetRatherThanHashSetCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String RemoveUncheckedThrowsClausesCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String RemoveUncheckedThrowsClausesCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String RemoveUncheckedThrowsClausesCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String UppercaseNumberSuffixRatherThanLowercaseCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String UppercaseNumberSuffixRatherThanLowercaseCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String UppercaseNumberSuffixRatherThanLowercaseCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String FormattedNumberRatherThanPackedNumberCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String FormattedNumberRatherThanPackedNumberCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String FormattedNumberRatherThanPackedNumberCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String SwitchCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String SwitchCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String SwitchCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String IfRatherThanTwoSwitchCasesCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String IfRatherThanTwoSwitchCasesCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String IfRatherThanTwoSwitchCasesCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String RemoveSemiColonCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String RemoveSemiColonCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String RemoveSemiColonCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String AddBracketsToControlStatementCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String AddBracketsToControlStatementCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String AddBracketsToControlStatementCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String RemoveUnnecessaryLocalBeforeReturnCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String RemoveUnnecessaryLocalBeforeReturnCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String RemoveUnnecessaryLocalBeforeReturnCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String RedundantModifiersCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String RedundantModifiersCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String RedundantModifiersCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String StaticInnerClassThanNonStaticCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String StaticInnerClassThanNonStaticCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String StaticInnerClassThanNonStaticCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String RemoveUnnecessaryCastCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String RemoveUnnecessaryCastCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String RemoveUnnecessaryCastCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String PushNegationDownCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String PushNegationDownCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String PushNegationDownCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String LocalVariableRatherThanFieldCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String LocalVariableRatherThanFieldCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String LocalVariableRatherThanFieldCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String SimpleNameRatherThanQualifiedNameCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String SimpleNameRatherThanQualifiedNameCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String SimpleNameRatherThanQualifiedNameCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String DoubleNegationCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String DoubleNegationCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String DoubleNegationCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String RedundantTruthCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String RedundantTruthCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String RedundantTruthCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String RedundantBooleanCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String RedundantBooleanCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String RedundantBooleanCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String EqualsNullableCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String EqualsNullableCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String EqualsNullableCleanUp_reason;
    /**
     * Automatically filled.
     */
    public static String RemoveParenthesisCleanUp_name;
    /**
     * Automatically filled.
     */
    public static String RemoveParenthesisCleanUp_description;
    /**
     * Automatically filled.
     */
    public static String RemoveParenthesisCleanUp_reason;

    static {
        // Initialize resource bundle
        NLS.initializeMessages(BUNDLE_NAME, MultiFixMessages.class);
    }
}

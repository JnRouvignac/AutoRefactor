/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2019 Fabrice TIERCELIN - initial API and implementation
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program under LICENSE-GNUGPL.  If not, see
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
    private static final String BUNDLE_NAME
    = "org.eclipse.jdt.internal.ui.fix.AutoRefactorMultiFixMessages"; //$NON-NLS-1$

    private MultiFixMessages() {
    }

    public static String CleanUpRefactoringWizard_IntPrimitiveRatherThanWrapperCleanUp_name;
    public static String CleanUpRefactoringWizard_IntPrimitiveRatherThanWrapperCleanUp_description;
    public static String CleanUpRefactoringWizard_IntPrimitiveRatherThanWrapperCleanUp_reason;
    public static String CleanUpRefactoringWizard_ShortPrimitiveRatherThanWrapperCleanUp_name;
    public static String CleanUpRefactoringWizard_ShortPrimitiveRatherThanWrapperCleanUp_description;
    public static String CleanUpRefactoringWizard_ShortPrimitiveRatherThanWrapperCleanUp_reason;
    public static String CleanUpRefactoringWizard_LongPrimitiveRatherThanWrapperCleanUp_name;
    public static String CleanUpRefactoringWizard_LongPrimitiveRatherThanWrapperCleanUp_description;
    public static String CleanUpRefactoringWizard_LongPrimitiveRatherThanWrapperCleanUp_reason;
    public static String CleanUpRefactoringWizard_DoublePrimitiveRatherThanWrapperCleanUp_name;
    public static String CleanUpRefactoringWizard_DoublePrimitiveRatherThanWrapperCleanUp_description;
    public static String CleanUpRefactoringWizard_DoublePrimitiveRatherThanWrapperCleanUp_reason;
    public static String CleanUpRefactoringWizard_FloatPrimitiveRatherThanWrapperCleanUp_name;
    public static String CleanUpRefactoringWizard_FloatPrimitiveRatherThanWrapperCleanUp_description;
    public static String CleanUpRefactoringWizard_FloatPrimitiveRatherThanWrapperCleanUp_reason;
    public static String CleanUpRefactoringWizard_CharPrimitiveRatherThanWrapperCleanUp_name;
    public static String CleanUpRefactoringWizard_CharPrimitiveRatherThanWrapperCleanUp_description;
    public static String CleanUpRefactoringWizard_CharPrimitiveRatherThanWrapperCleanUp_reason;
    public static String CleanUpRefactoringWizard_BytePrimitiveRatherThanWrapperCleanUp_name;
    public static String CleanUpRefactoringWizard_BytePrimitiveRatherThanWrapperCleanUp_description;
    public static String CleanUpRefactoringWizard_BytePrimitiveRatherThanWrapperCleanUp_reason;
    public static String CleanUpRefactoringWizard_RemoveUselessNullCheckCleanUp_name;
    public static String CleanUpRefactoringWizard_RemoveUselessNullCheckCleanUp_description;
    public static String CleanUpRefactoringWizard_RemoveUselessNullCheckCleanUp_reason;
    public static String CleanUpRefactoringWizard_VectorOldToNewAPICleanUp_name;
    public static String CleanUpRefactoringWizard_VectorOldToNewAPICleanUp_description;
    public static String CleanUpRefactoringWizard_VectorOldToNewAPICleanUp_reason;
    public static String CleanUpRefactoringWizard_PrimitiveWrapperCreationCleanUp_name;
    public static String CleanUpRefactoringWizard_PrimitiveWrapperCreationCleanUp_description;
    public static String CleanUpRefactoringWizard_PrimitiveWrapperCreationCleanUp_reason;
    public static String CleanUpRefactoringWizard_AutoBoxingRatherThanExplicitMethodCleanUp_name;
    public static String CleanUpRefactoringWizard_AutoBoxingRatherThanExplicitMethodCleanUp_description;
    public static String CleanUpRefactoringWizard_AutoBoxingRatherThanExplicitMethodCleanUp_reason;
    public static String CleanUpRefactoringWizard_UnboxingRatherThanExplicitMethodCleanUp_name;
    public static String CleanUpRefactoringWizard_UnboxingRatherThanExplicitMethodCleanUp_description;
    public static String CleanUpRefactoringWizard_UnboxingRatherThanExplicitMethodCleanUp_reason;
    public static String CleanUpRefactoringWizard_BracketsRatherThanArrayInstantiationCleanUp_name;
    public static String CleanUpRefactoringWizard_BracketsRatherThanArrayInstantiationCleanUp_description;
    public static String CleanUpRefactoringWizard_BracketsRatherThanArrayInstantiationCleanUp_reason;
    public static String CleanUpRefactoringWizard_LambdaExpressionRatherThanComparatorCleanUp_name;
    public static String CleanUpRefactoringWizard_LambdaExpressionRatherThanComparatorCleanUp_description;
    public static String CleanUpRefactoringWizard_LambdaExpressionRatherThanComparatorCleanUp_reason;
    public static String CleanUpRefactoringWizard_LambdaCleanUp_name;
    public static String CleanUpRefactoringWizard_LambdaCleanUp_description;
    public static String CleanUpRefactoringWizard_LambdaCleanUp_reason;
    public static String CleanUpRefactoringWizard_LiteralRatherThanBooleanConstantCleanUp_name;
    public static String CleanUpRefactoringWizard_LiteralRatherThanBooleanConstantCleanUp_description;
    public static String CleanUpRefactoringWizard_LiteralRatherThanBooleanConstantCleanUp_reason;
    public static String CleanUpRefactoringWizard_BooleanCleanUp_name;
    public static String CleanUpRefactoringWizard_BooleanCleanUp_description;
    public static String CleanUpRefactoringWizard_BooleanCleanUp_reason;
    public static String CleanUpRefactoringWizard_BooleanPrimitiveRatherThanWrapperCleanUp_name;
    public static String CleanUpRefactoringWizard_BooleanPrimitiveRatherThanWrapperCleanUp_description;
    public static String CleanUpRefactoringWizard_BooleanPrimitiveRatherThanWrapperCleanUp_reason;
    public static String CleanUpRefactoringWizard_LazyLogicalRatherThanEagerCleanUp_name;
    public static String CleanUpRefactoringWizard_LazyLogicalRatherThanEagerCleanUp_description;
    public static String CleanUpRefactoringWizard_LazyLogicalRatherThanEagerCleanUp_reason;
    public static String CleanUpRefactoringWizard_BooleanConstantRatherThanValueOfCleanUp_name;
    public static String CleanUpRefactoringWizard_BooleanConstantRatherThanValueOfCleanUp_description;
    public static String CleanUpRefactoringWizard_BooleanConstantRatherThanValueOfCleanUp_reason;
    public static String CleanUpRefactoringWizard_BooleanEqualsRatherThanNullCheckCleanUp_name;
    public static String CleanUpRefactoringWizard_BooleanEqualsRatherThanNullCheckCleanUp_description;
    public static String CleanUpRefactoringWizard_BooleanEqualsRatherThanNullCheckCleanUp_reason;
    public static String CleanUpRefactoringWizard_XORRatherThanDuplicateConditionsCleanUp_name;
    public static String CleanUpRefactoringWizard_XORRatherThanDuplicateConditionsCleanUp_description;
    public static String CleanUpRefactoringWizard_XORRatherThanDuplicateConditionsCleanUp_reason;
    public static String CleanUpRefactoringWizard_ORConditionRatherThanRedundantClausesCleanUp_name;
    public static String CleanUpRefactoringWizard_ORConditionRatherThanRedundantClausesCleanUp_description;
    public static String CleanUpRefactoringWizard_ORConditionRatherThanRedundantClausesCleanUp_reason;
    public static String CleanUpRefactoringWizard_TernaryOperatorRatherThanDuplicateConditionsCleanUp_name;
    public static String CleanUpRefactoringWizard_TernaryOperatorRatherThanDuplicateConditionsCleanUp_description;
    public static String CleanUpRefactoringWizard_TernaryOperatorRatherThanDuplicateConditionsCleanUp_reason;
    public static String CleanUpRefactoringWizard_WorkWithNullCheckedExpressionFirstCleanUp_name;
    public static String CleanUpRefactoringWizard_WorkWithNullCheckedExpressionFirstCleanUp_description;
    public static String CleanUpRefactoringWizard_WorkWithNullCheckedExpressionFirstCleanUp_reason;
    public static String CleanUpRefactoringWizard_InvertEqualsCleanUp_name;
    public static String CleanUpRefactoringWizard_InvertEqualsCleanUp_description;
    public static String CleanUpRefactoringWizard_InvertEqualsCleanUp_reason;
    public static String CleanUpRefactoringWizard_ComparisonCleanUp_name;
    public static String CleanUpRefactoringWizard_ComparisonCleanUp_description;
    public static String CleanUpRefactoringWizard_ComparisonCleanUp_reason;
    public static String CleanUpRefactoringWizard_DoubleCompareRatherThanEqualityCleanUp_name;
    public static String CleanUpRefactoringWizard_DoubleCompareRatherThanEqualityCleanUp_description;
    public static String CleanUpRefactoringWizard_DoubleCompareRatherThanEqualityCleanUp_reason;
    public static String CleanUpRefactoringWizard_RemoveUnneededThisExpressionCleanUp_name;
    public static String CleanUpRefactoringWizard_RemoveUnneededThisExpressionCleanUp_description;
    public static String CleanUpRefactoringWizard_RemoveUnneededThisExpressionCleanUp_reason;
    public static String CleanUpRefactoringWizard_AggregateConstructorRatherThanGWTMethodCleanUp_name;
    public static String CleanUpRefactoringWizard_AggregateConstructorRatherThanGWTMethodCleanUp_description;
    public static String CleanUpRefactoringWizard_AggregateConstructorRatherThanGWTMethodCleanUp_reason;
    public static String CleanUpRefactoringWizard_StandardMethodRatherThanLibraryMethodCleanUp_name;
    public static String CleanUpRefactoringWizard_StandardMethodRatherThanLibraryMethodCleanUp_description;
    public static String CleanUpRefactoringWizard_StandardMethodRatherThanLibraryMethodCleanUp_reason;
    public static String CleanUpRefactoringWizard_StringRatherThanNewStringCleanUp_name;
    public static String CleanUpRefactoringWizard_StringRatherThanNewStringCleanUp_description;
    public static String CleanUpRefactoringWizard_StringRatherThanNewStringCleanUp_reason;
    public static String CleanUpRefactoringWizard_StringCleanUp_name;
    public static String CleanUpRefactoringWizard_StringCleanUp_description;
    public static String CleanUpRefactoringWizard_StringCleanUp_reason;
    public static String CleanUpRefactoringWizard_StringValueOfRatherThanConcatCleanUp_name;
    public static String CleanUpRefactoringWizard_StringValueOfRatherThanConcatCleanUp_description;
    public static String CleanUpRefactoringWizard_StringValueOfRatherThanConcatCleanUp_reason;
    public static String CleanUpRefactoringWizard_BigNumberCleanUp_name;
    public static String CleanUpRefactoringWizard_BigNumberCleanUp_description;
    public static String CleanUpRefactoringWizard_BigNumberCleanUp_reason;
    public static String CleanUpRefactoringWizard_OppositeComparisonRatherThanNegativeExpressionCleanUp_name;
    public static String CleanUpRefactoringWizard_OppositeComparisonRatherThanNegativeExpressionCleanUp_description;
    public static String CleanUpRefactoringWizard_OppositeComparisonRatherThanNegativeExpressionCleanUp_reason;
    public static String CleanUpRefactoringWizard_RemoveEmptyIfCleanUp_name;
    public static String CleanUpRefactoringWizard_RemoveEmptyIfCleanUp_description;
    public static String CleanUpRefactoringWizard_RemoveEmptyIfCleanUp_reason;
    public static String CleanUpRefactoringWizard_InlineCodeRatherThanPeremptoryConditionCleanUp_name;
    public static String CleanUpRefactoringWizard_InlineCodeRatherThanPeremptoryConditionCleanUp_description;
    public static String CleanUpRefactoringWizard_InlineCodeRatherThanPeremptoryConditionCleanUp_reason;
    public static String CleanUpRefactoringWizard_RemoveUselessBlockCleanUp_name;
    public static String CleanUpRefactoringWizard_RemoveUselessBlockCleanUp_description;
    public static String CleanUpRefactoringWizard_RemoveUselessBlockCleanUp_reason;
    public static String CleanUpRefactoringWizard_RemoveEmptyStatementCleanUp_name;
    public static String CleanUpRefactoringWizard_RemoveEmptyStatementCleanUp_description;
    public static String CleanUpRefactoringWizard_RemoveEmptyStatementCleanUp_reason;
    public static String CleanUpRefactoringWizard_SingleDeclarationsRatherThanMultiDeclarationCleanUp_name;
    public static String CleanUpRefactoringWizard_SingleDeclarationsRatherThanMultiDeclarationCleanUp_description;
    public static String CleanUpRefactoringWizard_SingleDeclarationsRatherThanMultiDeclarationCleanUp_reason;
    public static String CleanUpRefactoringWizard_EndOfMethodRatherThanReturnCleanUp_name;
    public static String CleanUpRefactoringWizard_EndOfMethodRatherThanReturnCleanUp_description;
    public static String CleanUpRefactoringWizard_EndOfMethodRatherThanReturnCleanUp_reason;
    public static String CleanUpRefactoringWizard_DoWhileRatherThanWhileCleanUp_name;
    public static String CleanUpRefactoringWizard_DoWhileRatherThanWhileCleanUp_description;
    public static String CleanUpRefactoringWizard_DoWhileRatherThanWhileCleanUp_reason;
    public static String CleanUpRefactoringWizard_DoWhileRatherThanDuplicateCodeCleanUp_name;
    public static String CleanUpRefactoringWizard_DoWhileRatherThanDuplicateCodeCleanUp_description;
    public static String CleanUpRefactoringWizard_DoWhileRatherThanDuplicateCodeCleanUp_reason;
    public static String CleanUpRefactoringWizard_IfRatherThanWhileAndFallsThroughCleanUp_name;
    public static String CleanUpRefactoringWizard_IfRatherThanWhileAndFallsThroughCleanUp_description;
    public static String CleanUpRefactoringWizard_IfRatherThanWhileAndFallsThroughCleanUp_reason;
    public static String CleanUpRefactoringWizard_SuperCallRatherThanUselessOverridingCleanUp_name;
    public static String CleanUpRefactoringWizard_SuperCallRatherThanUselessOverridingCleanUp_description;
    public static String CleanUpRefactoringWizard_SuperCallRatherThanUselessOverridingCleanUp_reason;
    public static String CleanUpRefactoringWizard_CollapseIfStatementCleanUp_name;
    public static String CleanUpRefactoringWizard_CollapseIfStatementCleanUp_description;
    public static String CleanUpRefactoringWizard_CollapseIfStatementCleanUp_reason;
    public static String CleanUpRefactoringWizard_CommonCodeInIfElseStatementCleanUp_name;
    public static String CleanUpRefactoringWizard_CommonCodeInIfElseStatementCleanUp_description;
    public static String CleanUpRefactoringWizard_CommonCodeInIfElseStatementCleanUp_reason;
    public static String CleanUpRefactoringWizard_OppositeConditionRatherThanDuplicateConditionCleanUp_name;
    public static String CleanUpRefactoringWizard_OppositeConditionRatherThanDuplicateConditionCleanUp_description;
    public static String CleanUpRefactoringWizard_OppositeConditionRatherThanDuplicateConditionCleanUp_reason;
    public static String CleanUpRefactoringWizard_OneConditionRatherThanUnreachableBlockCleanUp_name;
    public static String CleanUpRefactoringWizard_OneConditionRatherThanUnreachableBlockCleanUp_description;
    public static String CleanUpRefactoringWizard_OneConditionRatherThanUnreachableBlockCleanUp_reason;
    public static String CleanUpRefactoringWizard_MergeConditionalBlocksCleanUp_name;
    public static String CleanUpRefactoringWizard_MergeConditionalBlocksCleanUp_description;
    public static String CleanUpRefactoringWizard_MergeConditionalBlocksCleanUp_reason;
    public static String CleanUpRefactoringWizard_OneIfRatherThanDuplicateBlocksThatFallThroughCleanUp_name;
    public static String CleanUpRefactoringWizard_OneIfRatherThanDuplicateBlocksThatFallThroughCleanUp_description;
    public static String CleanUpRefactoringWizard_OneIfRatherThanDuplicateBlocksThatFallThroughCleanUp_reason;
    public static String CleanUpRefactoringWizard_OneCodeThatFallsThroughRatherThanRedundantBlocksCleanUp_name;
    public static String CleanUpRefactoringWizard_OneCodeThatFallsThroughRatherThanRedundantBlocksCleanUp_description;
    public static String CleanUpRefactoringWizard_OneCodeThatFallsThroughRatherThanRedundantBlocksCleanUp_reason;
    public static String CleanUpRefactoringWizard_ElseRatherThanOppositeConditionCleanUp_name;
    public static String CleanUpRefactoringWizard_ElseRatherThanOppositeConditionCleanUp_description;
    public static String CleanUpRefactoringWizard_ElseRatherThanOppositeConditionCleanUp_reason;
    public static String CleanUpRefactoringWizard_GenericMapRatherThanRawMapCleanUp_name;
    public static String CleanUpRefactoringWizard_GenericMapRatherThanRawMapCleanUp_description;
    public static String CleanUpRefactoringWizard_GenericMapRatherThanRawMapCleanUp_reason;
    public static String CleanUpRefactoringWizard_GenericListRatherThanRawListCleanUp_name;
    public static String CleanUpRefactoringWizard_GenericListRatherThanRawListCleanUp_description;
    public static String CleanUpRefactoringWizard_GenericListRatherThanRawListCleanUp_reason;
    public static String CleanUpRefactoringWizard_UseDiamondOperatorCleanUp_name;
    public static String CleanUpRefactoringWizard_UseDiamondOperatorCleanUp_description;
    public static String CleanUpRefactoringWizard_UseDiamondOperatorCleanUp_reason;
    public static String CleanUpRefactoringWizard_UseMultiCatchCleanUp_name;
    public static String CleanUpRefactoringWizard_UseMultiCatchCleanUp_description;
    public static String CleanUpRefactoringWizard_UseMultiCatchCleanUp_reason;
    public static String CleanUpRefactoringWizard_CollectionContainsCleanUp_name;
    public static String CleanUpRefactoringWizard_CollectionContainsCleanUp_description;
    public static String CleanUpRefactoringWizard_CollectionContainsCleanUp_reason;
    public static String CleanUpRefactoringWizard_CollectionCleanUp_name;
    public static String CleanUpRefactoringWizard_CollectionCleanUp_description;
    public static String CleanUpRefactoringWizard_CollectionCleanUp_reason;
    public static String CleanUpRefactoringWizard_AllInOneMethodRatherThanLoopCleanUp_name;
    public static String CleanUpRefactoringWizard_AllInOneMethodRatherThanLoopCleanUp_description;
    public static String CleanUpRefactoringWizard_AllInOneMethodRatherThanLoopCleanUp_reason;
    public static String CleanUpRefactoringWizard_ObjectsEqualsRatherThanEqualsAndNullCheckCleanUp_name;
    public static String CleanUpRefactoringWizard_ObjectsEqualsRatherThanEqualsAndNullCheckCleanUp_description;
    public static String CleanUpRefactoringWizard_ObjectsEqualsRatherThanEqualsAndNullCheckCleanUp_reason;
    public static String CleanUpRefactoringWizard_BreakRatherThanPassiveIterationsCleanUp_name;
    public static String CleanUpRefactoringWizard_BreakRatherThanPassiveIterationsCleanUp_description;
    public static String CleanUpRefactoringWizard_BreakRatherThanPassiveIterationsCleanUp_reason;
    public static String CleanUpRefactoringWizard_UpdateSetRatherThanTestingFirstCleanUp_name;
    public static String CleanUpRefactoringWizard_UpdateSetRatherThanTestingFirstCleanUp_description;
    public static String CleanUpRefactoringWizard_UpdateSetRatherThanTestingFirstCleanUp_reason;
    public static String CleanUpRefactoringWizard_IsEmptyRatherThanSizeCleanUp_name;
    public static String CleanUpRefactoringWizard_IsEmptyRatherThanSizeCleanUp_description;
    public static String CleanUpRefactoringWizard_IsEmptyRatherThanSizeCleanUp_reason;
    public static String CleanUpRefactoringWizard_MapCleanUp_name;
    public static String CleanUpRefactoringWizard_MapCleanUp_description;
    public static String CleanUpRefactoringWizard_MapCleanUp_reason;
    public static String CleanUpRefactoringWizard_EntrySetRatherThanKeySetAndValueSearchCleanUp_name;
    public static String CleanUpRefactoringWizard_EntrySetRatherThanKeySetAndValueSearchCleanUp_description;
    public static String CleanUpRefactoringWizard_EntrySetRatherThanKeySetAndValueSearchCleanUp_reason;
    public static String CleanUpRefactoringWizard_MethodOnMapRatherThanMethodOnKeySetCleanUp_name;
    public static String CleanUpRefactoringWizard_MethodOnMapRatherThanMethodOnKeySetCleanUp_description;
    public static String CleanUpRefactoringWizard_MethodOnMapRatherThanMethodOnKeySetCleanUp_reason;
    public static String CleanUpRefactoringWizard_NoAssignmentInIfConditionCleanUp_name;
    public static String CleanUpRefactoringWizard_NoAssignmentInIfConditionCleanUp_description;
    public static String CleanUpRefactoringWizard_NoAssignmentInIfConditionCleanUp_reason;
    public static String CleanUpRefactoringWizard_DeclarationOutsideLoopRatherThanInsideCleanUp_name;
    public static String CleanUpRefactoringWizard_DeclarationOutsideLoopRatherThanInsideCleanUp_description;
    public static String CleanUpRefactoringWizard_DeclarationOutsideLoopRatherThanInsideCleanUp_reason;
    public static String CleanUpRefactoringWizard_IfElseIfCleanUp_name;
    public static String CleanUpRefactoringWizard_IfElseIfCleanUp_description;
    public static String CleanUpRefactoringWizard_IfElseIfCleanUp_reason;
    public static String CleanUpRefactoringWizard_CommonIfInIfElseCleanUp_name;
    public static String CleanUpRefactoringWizard_CommonIfInIfElseCleanUp_description;
    public static String CleanUpRefactoringWizard_CommonIfInIfElseCleanUp_reason;
    public static String CleanUpRefactoringWizard_StringBuilderCleanUp_name;
    public static String CleanUpRefactoringWizard_StringBuilderCleanUp_description;
    public static String CleanUpRefactoringWizard_StringBuilderCleanUp_reason;
    public static String CleanUpRefactoringWizard_StringBuilderMethodRatherThanReassignationCleanUp_name;
    public static String CleanUpRefactoringWizard_StringBuilderMethodRatherThanReassignationCleanUp_description;
    public static String CleanUpRefactoringWizard_StringBuilderMethodRatherThanReassignationCleanUp_reason;
    public static String CleanUpRefactoringWizard_StringBuilderRatherThanStringBufferCleanUp_name;
    public static String CleanUpRefactoringWizard_StringBuilderRatherThanStringBufferCleanUp_description;
    public static String CleanUpRefactoringWizard_StringBuilderRatherThanStringBufferCleanUp_reason;
    public static String CleanUpRefactoringWizard_HashMapRatherThanHashtableCleanUp_name;
    public static String CleanUpRefactoringWizard_HashMapRatherThanHashtableCleanUp_description;
    public static String CleanUpRefactoringWizard_HashMapRatherThanHashtableCleanUp_reason;
    public static String CleanUpRefactoringWizard_ArrayListRatherThanVectorCleanUp_name;
    public static String CleanUpRefactoringWizard_ArrayListRatherThanVectorCleanUp_description;
    public static String CleanUpRefactoringWizard_ArrayListRatherThanVectorCleanUp_reason;
    public static String CleanUpRefactoringWizard_ArrayDequeRatherThanStackCleanUp_name;
    public static String CleanUpRefactoringWizard_ArrayDequeRatherThanStackCleanUp_description;
    public static String CleanUpRefactoringWizard_ArrayDequeRatherThanStackCleanUp_reason;
    public static String CleanUpRefactoringWizard_SetRatherThanMapCleanUp_name;
    public static String CleanUpRefactoringWizard_SetRatherThanMapCleanUp_description;
    public static String CleanUpRefactoringWizard_SetRatherThanMapCleanUp_reason;
    public static String CleanUpRefactoringWizard_ArrayListRatherThanLinkedListCleanUp_name;
    public static String CleanUpRefactoringWizard_ArrayListRatherThanLinkedListCleanUp_description;
    public static String CleanUpRefactoringWizard_ArrayListRatherThanLinkedListCleanUp_reason;
    public static String CleanUpRefactoringWizard_SetRatherThanListCleanUp_name;
    public static String CleanUpRefactoringWizard_SetRatherThanListCleanUp_description;
    public static String CleanUpRefactoringWizard_SetRatherThanListCleanUp_reason;
    public static String CleanUpRefactoringWizard_HashMapRatherThanTreeMapCleanUp_name;
    public static String CleanUpRefactoringWizard_HashMapRatherThanTreeMapCleanUp_description;
    public static String CleanUpRefactoringWizard_HashMapRatherThanTreeMapCleanUp_reason;
    public static String CleanUpRefactoringWizard_HashSetRatherThanTreeSetCleanUp_name;
    public static String CleanUpRefactoringWizard_HashSetRatherThanTreeSetCleanUp_description;
    public static String CleanUpRefactoringWizard_HashSetRatherThanTreeSetCleanUp_reason;
    public static String CleanUpRefactoringWizard_UseStringContainsCleanUp_name;
    public static String CleanUpRefactoringWizard_UseStringContainsCleanUp_description;
    public static String CleanUpRefactoringWizard_UseStringContainsCleanUp_reason;
    public static String CleanUpRefactoringWizard_CommentsCleanUp_name;
    public static String CleanUpRefactoringWizard_CommentsCleanUp_description;
    public static String CleanUpRefactoringWizard_CommentsCleanUp_reason;
    public static String CleanUpRefactoringWizard_RemoveFieldsDefaultValuesCleanUp_name;
    public static String CleanUpRefactoringWizard_RemoveFieldsDefaultValuesCleanUp_description;
    public static String CleanUpRefactoringWizard_RemoveFieldsDefaultValuesCleanUp_reason;
    public static String CleanUpRefactoringWizard_StaticConstantRatherThanInstanceConstantCleanUp_name;
    public static String CleanUpRefactoringWizard_StaticConstantRatherThanInstanceConstantCleanUp_description;
    public static String CleanUpRefactoringWizard_StaticConstantRatherThanInstanceConstantCleanUp_reason;
    public static String CleanUpRefactoringWizard_RemoveOverridenAssignmentCleanUp_name;
    public static String CleanUpRefactoringWizard_RemoveOverridenAssignmentCleanUp_description;
    public static String CleanUpRefactoringWizard_RemoveOverridenAssignmentCleanUp_reason;
    public static String CleanUpRefactoringWizard_Java7HashRatherThanEclipseJava6HashCleanUp_name;
    public static String CleanUpRefactoringWizard_Java7HashRatherThanEclipseJava6HashCleanUp_description;
    public static String CleanUpRefactoringWizard_Java7HashRatherThanEclipseJava6HashCleanUp_reason;
    public static String CleanUpRefactoringWizard_HotSpotIntrinsicedAPIsCleanUp_name;
    public static String CleanUpRefactoringWizard_HotSpotIntrinsicedAPIsCleanUp_description;
    public static String CleanUpRefactoringWizard_HotSpotIntrinsicedAPIsCleanUp_reason;
    public static String CleanUpRefactoringWizard_AnnotationCleanUp_name;
    public static String CleanUpRefactoringWizard_AnnotationCleanUp_description;
    public static String CleanUpRefactoringWizard_AnnotationCleanUp_reason;
    public static String CleanUpRefactoringWizard_TryWithResourceCleanUp_name;
    public static String CleanUpRefactoringWizard_TryWithResourceCleanUp_description;
    public static String CleanUpRefactoringWizard_TryWithResourceCleanUp_reason;
    public static String CleanUpRefactoringWizard_TestNGAssertCleanUp_name;
    public static String CleanUpRefactoringWizard_TestNGAssertCleanUp_description;
    public static String CleanUpRefactoringWizard_TestNGAssertCleanUp_reason;
    public static String CleanUpRefactoringWizard_JUnitAssertCleanUp_name;
    public static String CleanUpRefactoringWizard_JUnitAssertCleanUp_description;
    public static String CleanUpRefactoringWizard_JUnitAssertCleanUp_reason;
    public static String CleanUpRefactoringWizard_RemoveEmptyLinesCleanUp_name;
    public static String CleanUpRefactoringWizard_RemoveEmptyLinesCleanUp_description;
    public static String CleanUpRefactoringWizard_RemoveEmptyLinesCleanUp_reason;
    public static String CleanUpRefactoringWizard_RemoveEmptySuperConstrInvocationCleanUp_name;
    public static String CleanUpRefactoringWizard_RemoveEmptySuperConstrInvocationCleanUp_description;
    public static String CleanUpRefactoringWizard_RemoveEmptySuperConstrInvocationCleanUp_reason;
    public static String CleanUpRefactoringWizard_ImplicitDefaultConstructorRatherThanWrittenOneCleanUp_name;
    public static String CleanUpRefactoringWizard_ImplicitDefaultConstructorRatherThanWrittenOneCleanUp_description;
    public static String CleanUpRefactoringWizard_ImplicitDefaultConstructorRatherThanWrittenOneCleanUp_reason;
    public static String CleanUpRefactoringWizard_AndroidWakeLockCleanUp_name;
    public static String CleanUpRefactoringWizard_AndroidWakeLockCleanUp_description;
    public static String CleanUpRefactoringWizard_AndroidWakeLockCleanUp_reason;
    public static String CleanUpRefactoringWizard_AndroidViewHolderCleanUp_name;
    public static String CleanUpRefactoringWizard_AndroidViewHolderCleanUp_description;
    public static String CleanUpRefactoringWizard_AndroidViewHolderCleanUp_reason;
    public static String CleanUpRefactoringWizard_LogParametersRatherThanLogMessageCleanUp_name;
    public static String CleanUpRefactoringWizard_LogParametersRatherThanLogMessageCleanUp_description;
    public static String CleanUpRefactoringWizard_LogParametersRatherThanLogMessageCleanUp_reason;
    public static String CleanUpRefactoringWizard_NamedMethodRatherThanLogLevelParameterCleanUp_name;
    public static String CleanUpRefactoringWizard_NamedMethodRatherThanLogLevelParameterCleanUp_description;
    public static String CleanUpRefactoringWizard_NamedMethodRatherThanLogLevelParameterCleanUp_reason;
    public static String CleanUpRefactoringWizard_EnumMapRatherThanHashMapCleanUp_name;
    public static String CleanUpRefactoringWizard_EnumMapRatherThanHashMapCleanUp_description;
    public static String CleanUpRefactoringWizard_EnumMapRatherThanHashMapCleanUp_reason;
    public static String CleanUpRefactoringWizard_EnumSetRatherThanHashSetCleanUp_name;
    public static String CleanUpRefactoringWizard_EnumSetRatherThanHashSetCleanUp_description;
    public static String CleanUpRefactoringWizard_EnumSetRatherThanHashSetCleanUp_reason;
    public static String CleanUpRefactoringWizard_RemoveUncheckedThrowsClausesCleanUp_name;
    public static String CleanUpRefactoringWizard_RemoveUncheckedThrowsClausesCleanUp_description;
    public static String CleanUpRefactoringWizard_RemoveUncheckedThrowsClausesCleanUp_reason;
    public static String CleanUpRefactoringWizard_CapitalizeLongLiteralCleanUp_name;
    public static String CleanUpRefactoringWizard_CapitalizeLongLiteralCleanUp_description;
    public static String CleanUpRefactoringWizard_CapitalizeLongLiteralCleanUp_reason;
    public static String CleanUpRefactoringWizard_SwitchCleanUp_name;
    public static String CleanUpRefactoringWizard_SwitchCleanUp_description;
    public static String CleanUpRefactoringWizard_SwitchCleanUp_reason;
    public static String CleanUpRefactoringWizard_IfRatherThanTwoSwitchCasesCleanUp_name;
    public static String CleanUpRefactoringWizard_IfRatherThanTwoSwitchCasesCleanUp_description;
    public static String CleanUpRefactoringWizard_IfRatherThanTwoSwitchCasesCleanUp_reason;
    public static String CleanUpRefactoringWizard_RemoveSemiColonCleanUp_name;
    public static String CleanUpRefactoringWizard_RemoveSemiColonCleanUp_description;
    public static String CleanUpRefactoringWizard_RemoveSemiColonCleanUp_reason;
    public static String CleanUpRefactoringWizard_AddBracketsToControlStatementCleanUp_name;
    public static String CleanUpRefactoringWizard_AddBracketsToControlStatementCleanUp_description;
    public static String CleanUpRefactoringWizard_AddBracketsToControlStatementCleanUp_reason;
    public static String CleanUpRefactoringWizard_RemoveUnnecessaryLocalBeforeReturnCleanUp_name;
    public static String CleanUpRefactoringWizard_RemoveUnnecessaryLocalBeforeReturnCleanUp_description;
    public static String CleanUpRefactoringWizard_RemoveUnnecessaryLocalBeforeReturnCleanUp_reason;
    public static String CleanUpRefactoringWizard_RedundantModifiersCleanUp_name;
    public static String CleanUpRefactoringWizard_RedundantModifiersCleanUp_description;
    public static String CleanUpRefactoringWizard_RedundantModifiersCleanUp_reason;
    public static String CleanUpRefactoringWizard_RemoveUnnecessaryCastCleanUp_name;
    public static String CleanUpRefactoringWizard_RemoveUnnecessaryCastCleanUp_description;
    public static String CleanUpRefactoringWizard_RemoveUnnecessaryCastCleanUp_reason;
    public static String CleanUpRefactoringWizard_PushNegationDownCleanUp_name;
    public static String CleanUpRefactoringWizard_PushNegationDownCleanUp_description;
    public static String CleanUpRefactoringWizard_PushNegationDownCleanUp_reason;
    public static String CleanUpRefactoringWizard_SimpleNameRatherThanQualifiedNameCleanUp_name;
    public static String CleanUpRefactoringWizard_SimpleNameRatherThanQualifiedNameCleanUp_description;
    public static String CleanUpRefactoringWizard_SimpleNameRatherThanQualifiedNameCleanUp_reason;
    public static String CleanUpRefactoringWizard_SimplifyExpressionCleanUp_name;
    public static String CleanUpRefactoringWizard_SimplifyExpressionCleanUp_description;
    public static String CleanUpRefactoringWizard_SimplifyExpressionCleanUp_reason;

    static {
        // Initialize resource bundle
        NLS.initializeMessages(BUNDLE_NAME, MultiFixMessages.class);
    }

}

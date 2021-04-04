/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2017 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Switch refactoring
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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.RefactoringRule;
import org.autorefactor.preferences.Preferences;

/** Lists all the available refactoring rules. */
public final class AllCleanUpRules {
	private AllCleanUpRules() {
	}

	/**
	 * Returns the cleanup rules which have been enabled from the Eclipse
	 * preferences.
	 *
	 * @param preferences the preferences
	 * @return the cleanup rules which have been enabled from the Eclipse
	 *             preferences
	 */
	public static List<RefactoringRule> getConfiguredRefactoringRules(final Preferences preferences) {
		List<RefactoringRule> refactorings= getAllCleanUpRules();

		for (final Iterator<RefactoringRule> iter= refactorings.iterator(); iter.hasNext();) {
			RefactoringRule refactoring= iter.next();

			if (!refactoring.isEnabled(preferences)) {
				iter.remove();
			}
		}

		return refactorings;
	}

	/**
	 * Returns all the available cleanup rules.
	 *
	 * @return all the available cleanup rules
	 */
	public static List<RefactoringRule> getAllCleanUpRules() {
		return newArrayList(new ObsoleteAddBracketsToControlStatementCleanUp(),
				new ObsoleteRemoveSemiColonCleanUp(), new RemoveUnnecessaryLocalBeforeReturnCleanUp(),
				new ObsoleteRedundantModifiersCleanUp(),
				new ObsoleteRemoveUnnecessaryCastCleanUp(), new ObsoletePushNegationDownCleanUp(),
				new ObsoleteSimpleNameRatherThanQualifiedNameCleanUp(), new ObsoleteDoubleNegationCleanUp(),
				new RemoveParenthesisCleanUp(),
				// The previous cleanups should run first as they make the code more standard and avoid weird code
				new RedundantTruthCleanUp(),
				new RedundantBooleanCleanUp(),
				new ObsoleteEqualsNullableCleanUp(),
				new ObsoleteLocalVariableRatherThanFieldCleanUp(),
				new IntPrimitiveRatherThanWrapperCleanUp(), new ShortPrimitiveRatherThanWrapperCleanUp(),
				new LongPrimitiveRatherThanWrapperCleanUp(), new DoublePrimitiveRatherThanWrapperCleanUp(),
				new FloatPrimitiveRatherThanWrapperCleanUp(), new CharPrimitiveRatherThanWrapperCleanUp(),
				new BytePrimitiveRatherThanWrapperCleanUp(), new ObsoleteAssignRatherThanFilterThenAssignAnywayCleanUp(),
				new AssignRatherThanTernaryFilterThenAssignAnywayCleanUp(),
				new VectorOldToNewAPICleanUp(), new ObsoleteParsingRatherThanValueOfCleanUp(),
				new ObsoleteValueOfRatherThanInstantiationCleanUp(),
				new ObsoleteAutoBoxingRatherThanExplicitMethodCleanUp(), new ObsoleteUnboxingRatherThanExplicitMethodCleanUp(),
				new ObsoleteBracketsRatherThanArrayInstantiationCleanUp(),
				new ObsoleteRedundantComparatorCleanUp(),
				new ObsoleteLambdaExpressionRatherThanComparatorCleanUp(),
				new ObsoleteLambdaCleanUp(), new ObsoleteLiteralRatherThanBooleanConstantCleanUp(), new BooleanCleanUp(),
				new BooleanPrimitiveRatherThanWrapperCleanUp(), new ObsoleteLazyLogicalRatherThanEagerCleanUp(),
				new BooleanConstantRatherThanValueOfCleanUp(), new BooleanEqualsRatherThanNullCheckCleanUp(),
				new ObsoleteXORRatherThanDuplicateConditionsCleanUp(), new ORConditionRatherThanRedundantClausesCleanUp(),
				new ObsoleteTernaryOperatorRatherThanDuplicateConditionsCleanUp(),
				new ObsoleteOperandFactorizationCleanUp(),
				// Must come after BooleanRefactoring, which may remove some targets
				new ObsoleteInvertEqualsCleanUp(), new ObsoleteComparisonCleanUp(),
				new ObsoleteDoubleCompareRatherThanEqualityCleanUp(), new RemoveUnneededThisExpressionCleanUp(),
				new AggregateConstructorRatherThanGWTMethodCleanUp(),
				new StandardMethodRatherThanLibraryMethodCleanUp(),
				new ObsoleteInstanceofRatherThanIsInstanceCleanUp(),
				new ObsoleteStringRatherThanNewStringCleanUp(),
				new StringCleanUp(),
				new EqualsIgnoreCaseRatherThanCaseShiftCleanUp(),
				new CharacterParameterRatherThanStringCleanUp(),
				new StringValueOfRatherThanConcatCleanUp(),
				new ObsoleteSubstringWithOneParameterRatherThanTwoCleanUp(),
				new TruncatingAppendingRatherThanSubCharactersCleanUp(),
				new BigNumberCleanUp(),
				new ObsoleteComparisonRatherThanEqualsCleanUp(),
				new OppositeComparisonRatherThanNegativeExpressionCleanUp(),
				new ObsoletePrimitiveComparisonRatherThanWrapperComparisonCleanUp(),
				new ObsoleteSerializeRatherThanBoxingAndSerializeCleanUp(),
				new RemoveEmptyIfCleanUp(),
				new NoLoopIterationRatherThanEmptyCheckCleanUp(),
				new InlineCodeRatherThanPeremptoryConditionCleanUp(),
				new RemoveUselessBlockCleanUp(), new RemoveEmptyStatementCleanUp(),
				new SingleDeclarationsRatherThanMultiDeclarationCleanUp(), new ObsoleteEndOfMethodRatherThanReturnCleanUp(),
				new ObsoleteEndOfLoopRatherThanContinueCleanUp(),
				new WhileConditionRatherThanInnerIfCleanUp(),
				new DoWhileRatherThanWhileCleanUp(), new DoWhileRatherThanDuplicateCodeCleanUp(),
				new ObsoleteIfRatherThanWhileAndFallsThroughCleanUp(), new SuperCallRatherThanUselessOverridingCleanUp(),
				new ObsoleteAndConditionRatherThanEmbededIfCleanUp(),
				new ObsoleteDuplicateAlternativeCleanUp(),
				new ObsoleteCommonCodeInIfElseStatementCleanUp(),
				new ObsoleteStaticInnerClassThanNonStaticCleanUp(),
				new ObsoleteOppositeConditionRatherThanDuplicateConditionCleanUp(),
				new ObsoleteOneConditionRatherThanUnreachableBlockCleanUp(), new ObsoleteMergeConditionalBlocksCleanUp(),
				new OneIfRatherThanDuplicateBlocksThatFallThroughCleanUp(),
				new ObsoleteOutsideCodeRatherThanFallingThroughBlocksCleanUp(),
				new ElseRatherThanNegatedConditionCleanUp(), new ObsoleteGenericMapRatherThanRawMapCleanUp(),
				new ObsoleteGenericListRatherThanRawListCleanUp(), new ObsoleteUseDiamondOperatorCleanUp(),
				new NIORatherThanIOCleanUp(),
				new ObsoleteUseMultiCatchCleanUp(),
				new ContainsRatherThanLoopCleanUp(), new ContainsAllRatherThanLoopCleanUp(),
				new DisjointRatherThanLoopCleanUp(),
				new ObsoleteCollectionCleanUp(), new ObsoleteAddAllRatherThanLoopCleanUp(),
				new ObsoleteFillRatherThanLoopCleanUp(),
				new ObsoleteJoinRatherThanLoopCleanUp(),
				new CollectionsAddAllRatherThanAsListCleanUp(),
				new ObsoleteObjectsEqualsRatherThanEqualsAndNullCheckCleanUp(), new ObsoleteBreakRatherThanPassiveIterationsCleanUp(),
				new UpdateSetRatherThanTestingFirstCleanUp(), new IsEmptyRatherThanSizeCleanUp(), new ObsoleteMapCleanUp(),
				new EntrySetRatherThanKeySetAndValueSearchCleanUp(), new MethodOnMapRatherThanMethodOnKeySetCleanUp(),
				new ObsoleteNoAssignmentInIfConditionCleanUp(),
				new ObsoleteIncrementStatementRatherThanIncrementExpressionCleanUp(),
				new DeclarationOutsideLoopRatherThanInsideCleanUp(),
				new ObsoleteIfElseIfCleanUp(), new CommonIfInIfElseCleanUp(),
				new StringBuilderCleanUp(), new StringBuilderMethodRatherThanReassignationCleanUp(),
				new StringBuilderRatherThanStringBufferCleanUp(),
				new ObsoleteStringBuilderRatherThanStringCleanUp(),
				new ObsoleteAtomicObjectRatherThanMonoIndexArrayCleanUp(),
				new ObsoletePatternRatherThanRegExStringCleanUp(),
				new OptimizeRegExCleanUp(),
				new HashMapRatherThanHashtableCleanUp(),
				new ArrayListRatherThanVectorCleanUp(), new ArrayDequeRatherThanStackCleanUp(),
				new SetRatherThanMapCleanUp(), new ArrayListRatherThanLinkedListCleanUp(),
				new SetRatherThanListCleanUp(), new HashMapRatherThanTreeMapCleanUp(),
				new HashSetRatherThanTreeSetCleanUp(), new UseStringContainsCleanUp(), new CommentsCleanUp(),
				new RemoveFieldsDefaultValuesCleanUp(), new StaticConstantRatherThanInstanceConstantCleanUp(),
				new ObsoleteRemoveOverriddenAssignmentCleanUp(), new ObsoleteJava7HashRatherThanEclipseJava6HashCleanUp(),
				new AnnotationCleanUp(), new ObsoleteTryWithResourceCleanUp(),
				new OneTryRatherThanTwoCleanUp(),
				// FIXME it would be nice if it was only enabled when testng jar is detected for
				// the project
				new TestNGAssertCleanUp(),
				new JupiterAssertCleanUp(),
				new JUnitAssertCleanUp(), new AssertJCleanUp(),
				new SeparateAssertionsRatherThanBooleanExpressionCleanUp(),
				new RemoveEmptyLinesCleanUp(),
				new ObsoleteRemoveEmptySuperConstrInvocationCleanUp(),
				new ImplicitDefaultConstructorRatherThanWrittenOneCleanUp(), new AndroidWakeLockCleanUp(),
				new AndroidViewHolderCleanUp(), new LogParametersRatherThanLogMessageCleanUp(),
				new NamedMethodRatherThanLogLevelParameterCleanUp(), new EnumMapRatherThanHashMapCleanUp(),
				new EnumSetRatherThanHashSetCleanUp(), new RemoveUncheckedThrowsClausesCleanUp(),
				new ObsoleteUppercaseNumberSuffixRatherThanLowercaseCleanUp(),
				new FormattedNumberRatherThanPackedNumberCleanUp(),
				new ObsoleteSwitchCleanUp(), new ObsoleteIfRatherThanTwoSwitchCasesCleanUp(),
				new VariableInsideIfRatherThanAboveCleanUp(),

				// Those cleanups should end the list because some other cleanups are primary
				new ObsoleteReduceIndentationCleanUp());
	}

	private static List<RefactoringRule> newArrayList(final RefactoringRule... refactorings) {
		List<RefactoringRule> results= new ArrayList<>(refactorings.length);
		for (RefactoringRule rewrite : refactorings) {
			if (rewrite != null) {
				results.add(rewrite);
			}
		}

		return results;
	}
}

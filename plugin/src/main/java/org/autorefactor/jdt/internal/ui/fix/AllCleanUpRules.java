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
        return newArrayList(new AddBracketsToControlStatementCleanUp(),
                new RemoveSemiColonCleanUp(), new RemoveUnnecessaryLocalBeforeReturnCleanUp(), new RedundantModifiersCleanUp(),
                new RemoveUnnecessaryCastCleanUp(), new PushNegationDownCleanUp(),
                new SimpleNameRatherThanQualifiedNameCleanUp(), new SimplifyExpressionCleanUp(),
                // The previous cleanups should run first as they make the code more standard and avoid weird code
                new LocalVariableRatherThanFieldCleanUp(),
                new IntPrimitiveRatherThanWrapperCleanUp(), new ShortPrimitiveRatherThanWrapperCleanUp(),
                new LongPrimitiveRatherThanWrapperCleanUp(), new DoublePrimitiveRatherThanWrapperCleanUp(),
                new FloatPrimitiveRatherThanWrapperCleanUp(), new CharPrimitiveRatherThanWrapperCleanUp(),
                new BytePrimitiveRatherThanWrapperCleanUp(), new AssignRatherThanFilterThenAssignAnywayCleanUp(),
                new AssignRatherThanTernaryFilterThenAssignAnywayCleanUp(),
                new VectorOldToNewAPICleanUp(), new PrimitiveWrapperCreationCleanUp(),
                new AutoBoxingRatherThanExplicitMethodCleanUp(), new UnboxingRatherThanExplicitMethodCleanUp(),
                new BracketsRatherThanArrayInstantiationCleanUp(), new LambdaExpressionRatherThanComparatorCleanUp(),
                new LambdaCleanUp(), new LiteralRatherThanBooleanConstantCleanUp(), new BooleanCleanUp(),
                new BooleanPrimitiveRatherThanWrapperCleanUp(), new LazyLogicalRatherThanEagerCleanUp(),
                new BooleanConstantRatherThanValueOfCleanUp(), new BooleanEqualsRatherThanNullCheckCleanUp(),
                new XORRatherThanDuplicateConditionsCleanUp(), new ORConditionRatherThanRedundantClausesCleanUp(),
                new TernaryOperatorRatherThanDuplicateConditionsCleanUp(),
                // Must come after BooleanRefactoring, which may remove some targets
                new InvertEqualsCleanUp(), new ComparisonCleanUp(),
                new DoubleCompareRatherThanEqualityCleanUp(), new RemoveUnneededThisExpressionCleanUp(),
                new AggregateConstructorRatherThanGWTMethodCleanUp(),
                new StandardMethodRatherThanLibraryMethodCleanUp(), new StringRatherThanNewStringCleanUp(),
                new StringCleanUp(), new StringValueOfRatherThanConcatCleanUp(), new BigNumberCleanUp(),
                new OppositeComparisonRatherThanNegativeExpressionCleanUp(),
                new RemoveEmptyIfCleanUp(),
                new NoLoopIterationRatherThanEmptyCheckCleanUp(),
                new InlineCodeRatherThanPeremptoryConditionCleanUp(),
                new RemoveUselessBlockCleanUp(), new RemoveEmptyStatementCleanUp(),
                new SingleDeclarationsRatherThanMultiDeclarationCleanUp(), new EndOfMethodRatherThanReturnCleanUp(),
                new DoWhileRatherThanWhileCleanUp(), new DoWhileRatherThanDuplicateCodeCleanUp(),
                new IfRatherThanWhileAndFallsThroughCleanUp(), new SuperCallRatherThanUselessOverridingCleanUp(),
                new AndConditionRatherThanEmbededIfCleanUp(), new CommonCodeInIfElseStatementCleanUp(),
                new StaticInnerClassThanNonStaticCleanUp(),
                new OppositeConditionRatherThanDuplicateConditionCleanUp(),
                new OneConditionRatherThanUnreachableBlockCleanUp(), new MergeConditionalBlocksCleanUp(),
                new OneIfRatherThanDuplicateBlocksThatFallThroughCleanUp(),
                new OneCodeThatFallsThroughRatherThanRedundantBlocksCleanUp(),
                new ElseRatherThanOppositeConditionCleanUp(), new GenericMapRatherThanRawMapCleanUp(),
                new GenericListRatherThanRawListCleanUp(), new UseDiamondOperatorCleanUp(), new UseMultiCatchCleanUp(),
                new ContainsRatherThanLoopCleanUp(), new ContainsAllRatherThanLoopCleanUp(), new CollectionCleanUp(), new AddAllRatherThanLoopCleanUp(),
                new FillRatherThanLoopCleanUp(),
                new ObjectsEqualsRatherThanEqualsAndNullCheckCleanUp(), new BreakRatherThanPassiveIterationsCleanUp(),
                new UpdateSetRatherThanTestingFirstCleanUp(), new IsEmptyRatherThanSizeCleanUp(), new MapCleanUp(),
                new EntrySetRatherThanKeySetAndValueSearchCleanUp(), new MethodOnMapRatherThanMethodOnKeySetCleanUp(),
                new NoAssignmentInIfConditionCleanUp(),
                new IncrementStatementRatherThanIncrementExpressionCleanUp(),
                new DeclarationOutsideLoopRatherThanInsideCleanUp(),
                new IfElseIfCleanUp(), new CommonIfInIfElseCleanUp(),
                new StringBuilderCleanUp(), new StringBuilderMethodRatherThanReassignationCleanUp(),
                new StringBuilderRatherThanStringBufferCleanUp(),
                new StringBuilderRatherThanStringCleanUp(),
                new PatternRatherThanRegExStringCleanUp(),
                new HashMapRatherThanHashtableCleanUp(),
                new ArrayListRatherThanVectorCleanUp(), new ArrayDequeRatherThanStackCleanUp(),
                new SetRatherThanMapCleanUp(), new ArrayListRatherThanLinkedListCleanUp(),
                new SetRatherThanListCleanUp(), new HashMapRatherThanTreeMapCleanUp(),
                new HashSetRatherThanTreeSetCleanUp(), new UseStringContainsCleanUp(), new CommentsCleanUp(),
                new RemoveFieldsDefaultValuesCleanUp(), new StaticConstantRatherThanInstanceConstantCleanUp(),
                new RemoveOverriddenAssignmentCleanUp(), new Java7HashRatherThanEclipseJava6HashCleanUp(),
                new HotSpotIntrinsicedAPIsCleanUp(), new AnnotationCleanUp(), new TryWithResourceCleanUp(),
                // FIXME it would be nice if it was only enabled when testng jar is detected for
                // the project
                new TestNGAssertCleanUp(), new JUnitAssertCleanUp(), new RemoveEmptyLinesCleanUp(),
                new RemoveEmptySuperConstrInvocationCleanUp(),
                new ImplicitDefaultConstructorRatherThanWrittenOneCleanUp(), new AndroidWakeLockCleanUp(),
                new AndroidViewHolderCleanUp(), new LogParametersRatherThanLogMessageCleanUp(),
                new NamedMethodRatherThanLogLevelParameterCleanUp(), new EnumMapRatherThanHashMapCleanUp(),
                new EnumSetRatherThanHashSetCleanUp(), new RemoveUncheckedThrowsClausesCleanUp(),
                new UppercaseNumberSuffixRatherThanLowercaseCleanUp(),
                new FormattedNumberRatherThanPackedNumberCleanUp(),
                new SwitchCleanUp(), new IfRatherThanTwoSwitchCasesCleanUp(),
                new VariableInsideIfRatherThanAboveCleanUp(),

                // Those cleanups should end the list because some other cleanups are prioritary
                new ReduceIndentationCleanUp());
    }

    private static List<RefactoringRule> newArrayList(final RefactoringRule... refactorings) {
        List<RefactoringRule> results= new ArrayList<>(refactorings.length);
        for (RefactoringRule r : refactorings) {
            if (r != null) {
                results.add(r);
            }
        }

        return results;
    }
}

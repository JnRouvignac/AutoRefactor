/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Switch refactoring
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
package org.autorefactor.refactoring.rules;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.autorefactor.AutoRefactorPlugin;
import org.autorefactor.preferences.Preferences;
import org.autorefactor.refactoring.RefactoringRule;

/**
 * Lists all the available refactoring rules.
 */
public final class AllRefactoringRules {

    private AllRefactoringRules() {
        super();
    }

    /**
     * Returns the refactoring rules which have been enabled from the Eclipse preferences.
     *
     * @return the refactoring rules which have been enabled from the Eclipse preferences
     */
    public static List<RefactoringRule> getConfiguredRefactoringRules() {
        final Preferences prefs = AutoRefactorPlugin.getPreferenceHelper();
        final List<RefactoringRule> refactorings = getAllRefactoringRules();
        for (final Iterator<RefactoringRule> iter = refactorings.iterator(); iter.hasNext();) {
            final RefactoringRule refactoring = iter.next();
            if (!refactoring.isEnabled(prefs)) {
                iter.remove();
            }
        }
        return refactorings;
    }

    /**
     * Returns all the available refactoring rules.
     *
     * @return all the available refactoring rules
     */
    public static List<RefactoringRule> getAllRefactoringRules() {
        return newArrayList(
                new RemoveUselessNullCheckRefactoring(),
                new VectorOldToNewAPIRefactoring(),
                new PrimitiveWrapperCreationRefactoring(),
                new BooleanRefactoring(),
                // Must come after BooleanRefactoring, which may remove some targets
                new WorkWithNullCheckedExpressionFirstRefactoring(),
                new AddBracketsToControlStatementRefactoring(),
                new InvertEqualsRefactoring(),
                new SimplifyExpressionRefactoring(),
                new RemoveUnneededThisExpressionRefactoring(),
                new StringRefactoring(),
                new BigDecimalRefactoring(),
                // TODO JNR implement
                // new ForeachRefactoring(),
                new DeadCodeEliminationRefactoring(),
                new CollapseIfStatementRefactoring(),
                new CommonCodeInIfElseStatementRefactoring(),
                // TODO JNR complete it
                // new GenerecizeRefactoring(),
                new UseDiamondOperatorRefactoring(),
                new UseMultiCatchRefactoring(),
                new CollectionContainsRefactoring(),
                new CollectionRefactoring(),
                new MapRefactoring(),
                new MapEliminateKeySetCallsRefactoring(),
                new NoAssignmentInIfConditionRefactoring(),
                new IfElseIfRefactoring(),
                new CommonIfInIfElseRefactoring(),
                // TODO JNR implement
                // new RemoveStupidIdiomaticPatternRefactoring(),
                // TODO JNR - to be completed
                // new ReduceVariableScopeRefactoring(),
                new StringBuilderRefactoring(),
                new UseStringContainsRefactoring(),
                new PushNegationDownRefactoring(),
                new CommentsRefactoring(),
                new RemoveFieldsDefaultValuesRefactoring(),
                new RemoveUnnecessaryLocalBeforeReturnRefactoring(),
                new RemoveUnnecessaryCastRefactoring(),
                new RemoveUselessModifiersRefactoring(),
                new HotSpotIntrinsicedAPIsRefactoring(),
                new AnnotationRefactoring(),
                new RemoveSemiColonRefactoring(),
                // FIXME it would be nice if it was only enabled when testng jar is detected for the project
                new TestNGAssertRefactoring(),
                new ReplaceQualifiedNamesBySimpleNamesRefactoring(),
                new RemoveEmptyLinesRefactoring(),
                new SwitchRefactoring());
    }

    private static List<RefactoringRule> newArrayList(RefactoringRule... refactorings) {
        final List<RefactoringRule> results = new ArrayList<RefactoringRule>(refactorings.length);
        for (RefactoringRule r : refactorings) {
            if (r != null) {
                results.add(r);
            }
        }
        return results;
    }

}

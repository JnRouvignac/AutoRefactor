/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014 Jean-Noël Rouvignac - initial API and implementation
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
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.autorefactor.AutoRefactorPlugin;
import org.autorefactor.refactoring.IRefactoring;
import org.autorefactor.ui.preferences.PreferenceHelper;

/**
 * Lists all the available refactorings.
 */
public class AllRefactorings {

	public static List<IRefactoring> getConfiguredRefactorings() {
		final PreferenceHelper prefs = AutoRefactorPlugin.getPreferenceHelper();
		final List<IRefactoring> refactorings = getAllRefactorings();
		for (final Iterator<IRefactoring> iter = refactorings.iterator(); iter.hasNext();) {
			final IRefactoring refactoring = iter.next();
			if (!prefs.addAngleBracketsToStatementBodies()
				&& refactoring instanceof AddBracketsToControlStatementRefactoring) {
				iter.remove();
			}
		}
		return refactorings;
	}

	public static List<IRefactoring> getAllRefactorings() {
		// TODO JNR Remove reference to Preferences from this method
		final PreferenceHelper prefs = AutoRefactorPlugin.getPreferenceHelper();
		return new ArrayList<IRefactoring>(Arrays.<IRefactoring> asList(
				new VectorOldToNewAPIRefactoring(),
				new PrimitiveWrapperCreationRefactoring(),
				new BooleanRefactoring(),
				new AddBracketsToControlStatementRefactoring(),
				new InvertEqualsRefactoring(),
				new SimplifyExpressionRefactoring(prefs.removeThisForNonStaticMethodAccess()),
				new StringRefactoring(),
				new BigDecimalRefactoring(),
				// TODO JNR implement
				// new ForeachRefactoring(),
				new DeadCodeEliminationRefactoring(),
				new CollapseIfStatementRefactoring(),
				new CommonCodeInIfElseStatementRefactoring(),
				// TODO JNR complete it
				// new GenerecizeRefactoring(),
				new CollectionAddAllRefactoring(),
				new IfStatementRefactoring(),
				// TODO JNR implement
				// new RemoveStupidIdiomaticPatternRefactoring(),
				// TODO JNR - to be completed
				// new ReduceVariableScopeRefactoring(),
				new StringBuilderRefactoring(),
				new CommentsRefactoring(),
				new RemoveFieldsDefaultValuesRefactoring(),
				new RemoveUnnecessaryLocalBeforeReturnRefactoring(),
				new RemoveUselessModifiersRefactoring(),
				new HotSpotIntrinsicedAPIsRefactoring()));
	}

}

/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015-2018 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.jdt.internal.ui.fix.samples_out;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.List;

public class ContainsRatherThanLoopSample {
	private boolean resultField;

	public boolean replaceForeach(List<String> col, String toFind) {
		// Keep this comment
		return col.contains(toFind);
	}

	public boolean replaceForeachInvertedEquals(List<Date> col, Date toFind) {
		// Keep this comment
		return col.contains(toFind);
	}

	public boolean replaceForeachWithNotCollectionContains(List<byte[]> col, byte[] toFind) {
		// Keep this comment
		return !col.contains(toFind);
	}

	public boolean replaceForeachNextStatementAfterTry(List<Integer> col, Integer toFind) {
		try {
			// Keep this comment
			return col.contains(toFind);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return false;
	}

	public boolean replaceForeachNextStatementAfterIf(List<String> col, String toFind, boolean b) {
		if (b) {
			// Keep this comment
			return col.contains(toFind);
		}
		return false;
	}

	public boolean replaceForeachPreviousStatementBeforeTry(List<String> col, String toFind) {
		boolean result = false;
		try {
			// Keep this comment
			result = col.contains(toFind);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return result;
	}

	public boolean replaceForeachPreviousStatementBeforeIf(List<String> col, String toFind, boolean b) {
		boolean result = false;
		if (b) {
			// Keep this comment
			result = col.contains(toFind);
		}
		return result;
	}

	public boolean replaceForeachHoldResultInVariableThenBreak(List<String> col, String toFind) {

		// Keep this comment
		boolean result = col.contains(toFind);
		return result;
	}

	public boolean replaceForeachHoldResultInVariableNoBreak(List<String> col, String toFind) {

		// Keep this comment
		boolean result = col.contains(toFind);
		return result;
	}

	public boolean replaceForeachHoldResultInVariableCannotRemoveVariable(List<String> col, String toFind) {
		// Keep this comment
		boolean result = false;
		;
		result = col.contains(toFind);
		return result;
	}

	public boolean replaceForeachHoldResultInField(List<String> col, String toFind) {
		// Keep this comment
		resultField = col.contains(toFind);
		return resultField;
	}

	public void replaceForeachWithoutVarDeclarationNorReturn(List<String> col, String toFind) {
		System.out.println("Before");
		// Keep this comment
		if (col.contains(toFind)) {
			System.out.println("Found!");
		}
		System.out.println("After");
	}

	public void replaceForeachWithLongCode(List<String> col, String toFind) {
		System.out.println("Before");
		// Keep this comment
		if (col.contains(toFind)) {
			Calendar calendar = GregorianCalendar.getInstance();
			calendar.add(Calendar.DAY_OF_YEAR, 10);
			Date dateInTenDays = calendar.getTime();
			System.out.println(dateInTenDays);
		}
		System.out.println("After");
	}

	public boolean doNotReplaceIterationOnArray(String[] array, String toFind) {
		for (String s : array) {
			if (s.equals(toFind)) {
				return true;
			}
		}
		return false;
	}

	public boolean replaceForCounter(List<String> col, String toFind) {
		// Keep this comment
		return col.contains(toFind);
	}

	public boolean replaceForCounterInvertedCondition(List<String> col, String toFind) {
		// Keep this comment
		return col.contains(toFind);
	}

	public boolean replaceForCounterPrefixedUpdater(List<String> col, String toFind) {
		// Keep this comment
		return col.contains(toFind);
	}

	public boolean replaceForCounterNoVariableDeclaration(List<String> col, String toFind) {
		// Keep this comment
		int i;
		return col.contains(toFind);
	}

	public boolean replaceForCounterNoLoopVariable(List<String> col, String toFind) {
		// Keep this comment
		return col.contains(toFind);
	}

	public boolean replaceForCounterNoLoopVariableInvertedEquals(List<String> col, String toFind) {
		// Keep this comment
		return col.contains(toFind);
	}

	public boolean replaceBackwardLoopOnCollection(List<String> col, String toFind) {
		// Keep this comment
		return col.contains(toFind);
	}

	public boolean replaceForIterator(List<String> col, String toFind) {
		// Keep this comment
		return col.contains(toFind);
	}

	public boolean replaceForIteratorNoLoopVariable(List<String> col, String toFind) {
		// Keep this comment
		return col.contains(toFind);
	}

	public String doNotRefactor1(List<String> col, String toFind) {
		for (String element : col) {
			if (element.equals(toFind)) {
				return element;
			}
		}
		return null;
	}

	public void doNotRefactor2(List<String> col, String toFind) {
		for (String element : col) {
			if (element.equals(toFind)) {
				element.toString();
			}
		}
	}

	public void doNotRefactor3(List<String> col, String toFind) {
		for (String element : col) {
			if (element.equals(toFind)) {
				element.toString();
				break;
			}
		}
	}

	public class DoNotRefactorCollectionImplementation<T> extends ArrayList<T> {
		private static final long serialVersionUID= 8837962990422334107L;

		@Override
		public boolean contains(Object other) {
			for (Object item : this) {
				if (other.equals(item)) {
					return true;
				}
			}
			return false;
		}
	}
}

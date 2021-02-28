/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java "Failed bases.
 *
 * Copyright (C) 2019 Fabrice Tiercelin - initial API and implementation
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
package org.autorefactor.refactoring.rules.samples_in;

import java.util.List;
import java.util.function.Predicate;

public class DeclarationOutsideLoopRatherThanInsideSample {
	public String moveObjectDecl(int count) {
		StringBuilder concat= new StringBuilder();

		for (int i= 0; i < count; i++) {
			// Keep this comment
			String newNumber= String.valueOf(i);
			concat.append(newNumber);
			concat.append(";");
		}

		return concat.toString();
	}

	public String moveArrayDecl(int count) {
		StringBuilder concat= new StringBuilder();

		for (int i= 0; i < count; i++) {
			// Keep this comment
			String newNumber[]= new String[] { String.valueOf(i) };
			concat.append(newNumber);
			concat.append(";");
		}

		return concat.toString();
	}

	public String moveObjectDeclFromLoops(int count) {
		StringBuilder concat= new StringBuilder();

		for (int i= 0; i < count; i++) {
			// Keep this comment
			String newNumber= String.valueOf(i);
			concat.append(newNumber);
			concat.append(";");

			for (int j= 0; j < count; j++) {
				// Keep this comment
				String anotherNewNumber= String.valueOf(j);
				concat.append(anotherNewNumber);
				concat.append(";");
			}
		}

		return concat.toString();
	}

	public int moveFromEnhancedLoop(List<String> myList) {
		int total= 0;

		for (String number : myList) {
			// Keep this comment
			int newNumber= Integer.parseInt(number);
			total+= newNumber;
		}

		return total;
	}

	public int moveFromWhile(int max) {
		int result= 1;

		while (result < max) {
			// Keep this comment
			int newNumber= result * result;
			result+= newNumber;
		}

		return result;
	}

	public int moveFromDoWhile(int max) {
		int result= 1;

		do {
			// Keep this comment
			int newNumber= result * result;
			result+= newNumber;
		} while (result < max);

		return result;
	}

	public int moveComplexType(int max) {
		int result= 1;

		do {
			// Keep this comment
			Class<?>[] complexObject= new Class<?>[0];
			result+= complexObject.length + 1;
		} while (result < max);

		return result;
	}

	public int moveDeclWithoutInit(List<String> myList) {
		int total= 0;

		for (String number : myList) {
			// Keep this comment
			int newNumber;
			newNumber= Integer.parseInt(number);
			total+= newNumber;
		}

		return total;
	}

	public int movePrimitiveTypeDecl(List<String> myList) {
		int total= 0;

		for (String number : myList) {
			// Keep this comment
			int newNumber= Integer.parseInt(number);
			total+= newNumber;
		}

		return total;
	}

	public String moveObjectDecls(List<Integer> myList1, List<Integer> myList2) {
		StringBuilder concat= new StringBuilder();

		for (Integer number : myList1) {
			// Keep this comment
			String newNumber= String.valueOf(number);
			concat.append(newNumber);
		}

		for (Integer number : myList2) {
			// Keep this comment
			String anotherNumber= String.valueOf(number);
			concat.append(anotherNumber);
		}

		return concat.toString();
	}

	public String moveWhenNoConflictsBefore(List<Integer> myList) {
		StringBuilder concat= new StringBuilder();

		{
			String theNumber= "10 and ";
			System.out.println(theNumber);
		}

		for (Integer number : myList) {
			// Keep this comment
			String theNumber= String.valueOf(number);
			concat.append(theNumber);
		}

		return concat.toString();
	}

	public String doNotMoveFinalDecl(List<Integer> myList) {
		StringBuilder concat= new StringBuilder();

		for (Integer number : myList) {
			final String newNumber= String.valueOf(number);
			concat.append(newNumber);
		}

		return concat.toString();
	}

	public void doNotMoveEffectivelyFinalDeclaration(List<Integer> myList, List<String> texts) {
		for (Integer number : myList) {
			String newNumber= String.valueOf(number);
			texts.removeIf(e -> newNumber.equals(e + "0"));
		}
	}

	public void doNotMoveEffectivelyFinalDeclarationInAnonymousClass(List<Integer> myList, List<String> texts) {
		for (Integer number : myList) {
			String newNumber= String.valueOf(number);
			final Predicate<? super String> filter= new Predicate<String>() {

				@Override
				public boolean test(String e) {
					return newNumber.equals(e + "0");
				}
			};
			texts.removeIf(filter);
		}
	}

	public String doNotMoveMultiFragments(List<Integer> myList) {
		StringBuilder concat= new StringBuilder();

		for (Integer number : myList) {
			String newNumber, sameNumber= String.valueOf(number);
			newNumber= "0";
			concat.append(newNumber);
			concat.append("<");
			concat.append(sameNumber);
		}

		return concat.toString();
	}

	public String doNotMoveWhenConflicts(List<Integer> myList) {
		StringBuilder concat= new StringBuilder();

		for (Integer number : myList) {
			String myNumber= String.valueOf(number);
			concat.append(myNumber);
		}

		String myNumber= " and 10";

		return concat.toString() + myNumber;
	}

	public String doNotMoveWhenConflictsInLoopParameter(List<Integer> myList) {
		StringBuilder concat= new StringBuilder();

		for (Integer number : myList) {
			String myNumber= String.valueOf(number);
			concat.append(myNumber);
		}

		for (Integer myNumber : myList) {
			concat.append(myNumber);
		}

		return concat.toString();
	}

	public String doNotMoveWhenConflictsAfter(List<Integer> myList) {
		StringBuilder concat= new StringBuilder();

		for (Integer number : myList) {
			String oneNumber= String.valueOf(number);
			concat.append(oneNumber);
		}

		{
			String oneNumber= " and 10";
			System.out.println(oneNumber);
		}

		return concat.toString();
	}

	public String doNotMoveDeclFromOtherScope(List<Integer> myList) {
		StringBuilder concat= new StringBuilder();

		for (Integer number : myList) {
			if (number > 0) {
				String aNumber= String.valueOf(number);
				concat.append(aNumber);
			}
		}

		return concat.toString();
	}

	public String doNotMoveAnnotatedDecl(List<Object> texts) {
		StringBuilder concat= new StringBuilder();

		for (Object object : texts) {
			@SuppressWarnings("unchecked")
			List<String> text= (List<String>) object;
			concat.append(text);
			concat.append(";");
		}

		return concat.toString();
	}
}

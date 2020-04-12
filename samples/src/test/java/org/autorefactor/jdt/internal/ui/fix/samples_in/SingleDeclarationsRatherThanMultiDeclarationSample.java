/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice Tiercelin - Initial API and implementation
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
package org.autorefactor.jdt.internal.ui.fix.samples_in;

public class SingleDeclarationsRatherThanMultiDeclarationSample {
	/**
	 * Keep this comment.
	 */
	public static final int x = 10, y = 20;

	/**
	 * Keep this comment.
	 * @Deprecated
	 */
	@Deprecated
	public int oldX, oldY;

	/**
	 * Keep this comment.
	 */
	boolean one, two, three;

	public void refactorMultiDecl() {
		// Keep this comment
		boolean one, two, three;

		// Keep this comment
		int four = 4, five = 5, six = 6;
	}

	public void refactorMultiDeclWithModifier() {
		// Keep this comment
		final int seven = 7, height = 8, nine = 9;
	}

	public void doNotRefactorForVar() {
		for (int counter = 0, ten = 10; counter < ten; counter++) {
			System.out.println(counter);
		}
	}
}

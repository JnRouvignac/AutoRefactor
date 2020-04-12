/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice Tiercelin - initial API and implementation
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
package org.autorefactor.jdt.internal.ui.fix.all.samples_in;

public class MergeDuplicateConditionThenSimplifyExprSample {
	public int mergeDuplicateCondition(boolean duplicateCondition, boolean anotherCondition) {
		int i;

		if (!duplicateCondition && anotherCondition) {
			i = 0;
		} else if (!duplicateCondition) {
			i = 10;
		} else {
			i = 20;
		}

		return i;
	}

	public int mergeDuplicateCondition(int i, boolean anotherCondition) {
		int j;

		if ((i > 0) && anotherCondition) {
			j = 0;
		} else if (i > 0) {
			j = 10;
		} else {
			j = 20;
		}

		return j;
	}
}

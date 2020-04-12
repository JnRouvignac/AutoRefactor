/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
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
package org.autorefactor.jdt.internal.ui.fix.samples_out;

import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;

public class AssignRatherThanTernaryFilterThenAssignAnywaySample {
	private final String DEFAULT = "";
	private String input;

	public String refactorLocalVariable1(String input) {
		// Keep this comment
		return input;
	}

	public String doNotRefactorOppositeAssignment(String input) {
		return (input == null) ? input : null;
	}

	public String doNotRefactorLocalVariable(String input) {
		return (input == null) ? DEFAULT : input;
	}

	public String doNotRefactorActiveExpression(List<String> input) {
		return (input.remove(0) == null) ? null : input.remove(0);
	}

	public String refactorLocalVariable2(String input) {
		// Keep this comment
		return input;
	}

	public String refactorLocalVariable3(String input) {
		// Keep this comment
		return input;
	}

	public String refactorLocalVariable4(String input) {
		// Keep this comment
		return input;
	}

	public int removeHardCodedNumber(int input) {
		// Keep this comment
		return input;
	}

	public char removeHardCodedCharacter(char input) {
		// Keep this comment
		return input;
	}

	public int removeHardCodedExpression(int input) {
		// Keep this comment
		return input;
	}

	public String refactorLocalVariable5(String input, boolean isValid) {
		String output = null;
		if (isValid)
			// Keep this comment
			output = input;
		return output;
	}

	public void doNotRefactorFieldAssign(String input) {
		this.input = (input == null) ? DEFAULT : input;
	}

	public void refactorFieldAssign1(String input) {
		// Keep this comment
		this.input = input;
	}

	public void refactorFieldAssign2(String input) {
		// Keep this comment
		this.input = input;
	}

	public void refactorFieldAssign3(String input) {
		// Keep this comment
		this.input = input;
	}

	public void refactorFieldAssign4(String input) {
		// Keep this comment
		this.input = input;
	}
}

/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2019 Fabrice Tiercelin - Initial implementation
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

import java.io.IOException;
import java.util.List;

public class OneConditionRatherThanUnreachableBlockSample {
	public int removeDuplicateCondition(boolean b1, boolean b2) {
		// Keep this comment
		if (b1 && b2) {
			return 0;
		}

		return 2;
	}

	public int removeDuplicateConditionWithElse(int i1, int i2) {
		// Keep this comment
		if (i1 < i2) {
			return 0;
		} else {
			return 2;
		}
	}

	public int removeDuplicateConditionAmongOthers(int i1, int i2) {
		// Keep this comment
		if (i1 == 0) {
			return -1;
		} else if (i1 < i2 + 1) {
			return 0;
		}

		return 2;
	}

	public String doNotCreateUnreachable(int i1, int i2) {
		if (i1 < i2) {
			return "Falls through";
		} else if (i2 > i1) {
			System.out.println("Does not fall through");
		} else {
			return "Falls through too";
		}

		return "I should be reachable";
	}

	public int doNotRemoveDifferentCondition(boolean b1, boolean b2) {
		if (b1 && b2) {
			return 0;
		} else if (b2 || b1) {
			return 1;
		}

		return 2;
	}

	public int doNotRemoveActiveCondition(List<String> myList) {
		if (myList.remove("I will be removed")) {
			return 0;
		} else if (myList.remove("I will be removed")) {
			return 1;
		}

		return 2;
	}

	public int doNotRemoveCaughtCode(boolean b1, boolean b2) {
		try {
			if (b1 && b2) {
				return 0;
			} else if (b2 && b1) {
				throw new IOException();
			}
		} catch (IOException e) {
			System.out.println("I should be reachable");
		}

		return 2;
	}

	public int removeUncaughtCode(boolean b1, boolean b2) throws IOException {
		try {
			// Keep this comment
			if (b1 && b2) {
				return 0;
			}
		} catch (NullPointerException e) {
			System.out.println("I should be reachable");
		}

		return 2;
	}
}

/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.ui;

import org.autorefactor.refactoring.IRefactoring;
import org.eclipse.jdt.core.ICompilationUnit;

/**
 * Holds a test case with all the necessary data.
 */
public class TestCase {

	public final String sampleName;
	public ICompilationUnit sampleIn;
	public ICompilationUnit sampleOut;
	public IRefactoring refactoring;

	public TestCase(String sampleName) {
		this.sampleName = sampleName;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return "Tests for sample: " + sampleName + " (" + getINAndOUT(true)
				+ ") with refactoring: "
				+ (refactoring == null ? "" : refactoring.getClass().getSimpleName());
	}

	public String getINAndOUT(boolean presence) {
		if ((presence && sampleIn != null) || (!presence && sampleIn == null)) {
			if ((presence && sampleOut != null) || (!presence && sampleOut == null)) {
				return "IN and OUT";
			}
			return "IN";
		}
		if ((presence && sampleOut != null) || (!presence && sampleOut == null)) {
			return "OUT";
		}
		return "no sample";
	}

}

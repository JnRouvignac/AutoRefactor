/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Andrei Paikin - Initial API and implementation
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

public class UppercaseNumberSuffixRatherThanLowercaseSample {
	private long usual = 101L;
	private long octal = 0121L;
	private long hex = 0xdafdafdafL;
	private long binary = 0b1110010111L;
	private long withUnderscore = 101_101L;

	private float usualFloat = 101F;
	private float octalFloat = 0121F;

	public float refactorIt() {
		long localVar = 11L;
		float localFloat = 11F;
		return localVar + 333L + localFloat + 11F;
	}

	public float doNotRefactor() {
		long l = 11L;
		float f = 11F;
		return l + 101L + f + 11F;
	}
}

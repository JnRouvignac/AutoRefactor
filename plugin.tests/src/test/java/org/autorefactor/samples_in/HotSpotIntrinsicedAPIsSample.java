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
package org.autorefactor.samples_in;

public class HotSpotIntrinsicedAPIsSample {

	private void replaceBySystemArrayCopyBasic(int[] src, int[] dest) {
		for (int i = 0; i < 3; i++) {
			dest[i] = src[i];
		}
	}

	private void replaceBySystemArrayCopyComplexUpperBound(int[] src, int[] dest) {
		for (int i = 0; i < src.length; i++) {
			dest[i] = src[i];
		}
	}

	private void replaceBySystemArrayCopyAssignIndexVariable(int[] src, int[] dest, int i) {
		for (i = 0; i < 3; i++) {
			dest[i] = src[i];
		}
	}

	private void replaceBySystemArrayCopyWithSrcPos(int[] src, int[] dest) {
		for (int i = 0; i < src.length - 1; i++) {
			dest[i] = src[i + 1];
		}
	}

	private void replaceBySystemArrayCopyWithDestPos(int[] src, int[] dest) {
		for (int i = 0; i < src.length - 1; i++) {
			dest[i + 1] = src[i];
		}
	}

	private void replaceWithArraysCopyOf(int[] src, int[] dest) {
		// FIXME Should use java.util.Arrays.copyOf()
	}

	private void replaceWithArraysCopyOfRange(int[] src, int[] dest) {
		// FIXME Should use java.util.Arrays.copyOfRange()
	}

	private boolean replaceWithArraysEquals(int[] src, int[] dest) {
		if (dest.length != src.length) {
			return false;
		}
		for (int i = 0; i < src.length; i++) {
			if (dest[i] != src[i]) {
				return false;
			}
		}
		return true;
		// FIXME Should use java.util.Arrays.equals()
	}

	private boolean replaceWithArraysEquals2(int[] src, int[] dest) {
		if (dest.length == src.length) {
			for (int i = 0; i < src.length; i++) {
				if (dest[i] != src[i]) {
					return false;
				}
			}
			return true;
		}
		return false;
		// FIXME Should use java.util.Arrays.equals()
	}

	private void replaceWithStringIndexOf(String s) {
		// FIXME Should use java.lang.String.indexOf()
	}

}

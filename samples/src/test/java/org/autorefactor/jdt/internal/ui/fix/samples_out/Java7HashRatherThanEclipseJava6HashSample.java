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
package org.autorefactor.jdt.internal.ui.fix.samples_out;

import java.util.Arrays;
import java.util.Map;
import java.util.Objects;
import java.util.Observable;

public class Java7HashRatherThanEclipseJava6HashSample {
	public class RefactoredClass {
		private Map<Integer, String> innerTextById;
		private Observable innerObservable;
		private String innerText;
		private String[] innerTexts;
		private int[] innerIntegers;
		private char innerChar;
		private byte innerByte;
		private boolean innerBoolean;
		private int innerInt;
		private long innerLong;
		private double innerDouble;
		private short innerShort;
		private float innerFloat;
		private double innerOtherDouble;

		@Override
		public int hashCode() {
			// Keep this comment
			return Objects.hash(getEnclosingInstance().hashCode(), innerBoolean, innerByte, innerChar, innerDouble, innerFloat, innerInt,
					Arrays.hashCode(innerIntegers), innerLong, innerObservable, innerOtherDouble, innerShort, innerText,
					innerTextById, Arrays.hashCode(innerTexts));
		}

		private Java7HashRatherThanEclipseJava6HashSample getEnclosingInstance() {
			return Java7HashRatherThanEclipseJava6HashSample.this;
		}
	}

	public class DoNotRefactorNewClass {
		private boolean innerBoolean;

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result= prime * result + getEnclosingInstance().hashCode();
			result = prime * result + (innerBoolean ? 1231 : 1237);
			return result;
		}

		private Java7HashRatherThanEclipseJava6HashSample getEnclosingInstance() {
			return new Java7HashRatherThanEclipseJava6HashSample();
		}
	}

	public class DoNotRefactorCustomHash {
		private boolean innerBoolean;

		@Override
		public int hashCode() {
			final int prime = 63;
			int result = 1;
			result = prime * result + (innerBoolean ? 1231 : 1237);
			return result;
		}
	}

	public class DoNotRefactorSpecialAssignment {
		private boolean innerBoolean;

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result += prime * result + (innerBoolean ? 1231 : 1237);
			return result;
		}
	}

	private Map<Integer, String> textById;
	private Observable anObservable;
	private String aText;
	private String[] texts;
	private int[] integers;
	private char aChar;
	private byte aByte;
	private boolean aBoolean;
	private int anInt;
	private long aLong;
	private double aDouble;
	private short aShort;
	private float aFloat;
	private double anotherDouble;

	@Override
	public int hashCode() {
		// Keep this comment
		return Objects.hash(aBoolean, aByte, aChar, aFloat, aLong, aDouble, aShort, aText, anInt, anObservable,
				Arrays.hashCode(integers), textById, Arrays.hashCode(texts), anotherDouble);
	}
}

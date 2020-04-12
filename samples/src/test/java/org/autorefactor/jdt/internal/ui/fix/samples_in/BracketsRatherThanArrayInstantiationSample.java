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
package org.autorefactor.jdt.internal.ui.fix.samples_in;

import java.util.Observable;

public class BracketsRatherThanArrayInstantiationSample {
	/**
	 * Keep this comment.
	 */
	private double[] refactorThisDoubleArray = new double[] { 42.42 };

	/**
	 * Keep this comment.
	 */
	private int[][] refactorThis2DimensionArray = new int[][] { { 42 } };

	/**
	 * Keep this comment.
	 */
	private Observable[] refactorThisObserverArray = new Observable[0];

	/**
	 * Keep this comment.
	 */
	private short[] refactorThisShortArray, andThisArrayToo = new short[0];

	private Byte[] doNotRefactorNotInitializedArray = new Byte[10];

	private Object doNotRefactorThisObserverArray = new Observable[0];

	public void refactorArrayInstantiations() {
		// Keep this comment
		double[] refactorLocalDoubleArray = new double[] { 42.42 };
		int[][] refactorLocal2DimensionArray = new int[][] { { 42 } };
		Observable[] refactorLocalObserverArray = new Observable[0];
		short[] refactorThisShortArray, andThisArrayToo = new short[0];
	}

	public void doNotRefactorArrayAssignment() {
		char[] refactorLocalDoubleArray;
		refactorLocalDoubleArray = new char[] { 'a', 'b' };
	}

	public void doNotRefactorArrayInstantiationsInBrackets() {
		boolean[] refactorLocalDoubleArray = (new boolean[] { true });
	}

	public void doNotRefactorCastedArrayInstantiations() {
		Object refactorLocalDoubleArray = (double[]) new double[] { 42.42 };
	}

	public double[] doNotRefactorReturnedArrayInstantiation() {
		return new double[] { 42.42 };
	}

	public void doNotRefactorArrayInstantiationParameter() {
		System.out.println(new double[] { 42.42 });
	}

	public String doNotRefactorArrayInstantiationExpression() {
		return new float[] { 42.42f }.toString();
	}
}

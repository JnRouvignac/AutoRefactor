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

public class RemoveFieldsDefaultValuesSample {

	private static final int MY_CONSTANT = 0;

	private Object obj = null;
	private String st = null;
	private byte by1 = 0x0, by2 = 0;
	private boolean bo = false;
	private char c1 = 0, c2 = '\u0000';
	private short sh = 0;
	private int i = 0;
	private long l1 = 0, l2 = 0l, l3 = 0L;
	private float f1 = 0, f2 = 0f, f3 = 0F, f4 = 0.0f;
	private double d1 = 0, d2 = 0.0;

}

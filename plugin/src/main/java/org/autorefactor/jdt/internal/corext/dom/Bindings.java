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
package org.autorefactor.jdt.internal.corext.dom;

import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ITypeBinding;

/**
 * Copy of org.eclipse.jdt.internal.corext.dom.Bindings class.
 */
public final class Bindings {
	/**
	 * Forbidden.
	 */
	private Bindings() {
	}

	/**
	 * Get the type of the associated primitive wrapper.
	 *
	 * @param wrapperName wrapper fully qualified name
	 * @return The type of the associated primitive wrapper.
	 */
	public static String getUnboxedTypeName(final String wrapperName) {
		if (Long.class.getCanonicalName().equals(wrapperName)) {
			return long.class.getSimpleName();
		}
		if (Integer.class.getCanonicalName().equals(wrapperName)) {
			return int.class.getSimpleName();
		}
		if (Short.class.getCanonicalName().equals(wrapperName)) {
			return short.class.getSimpleName();
		}
		if (Character.class.getCanonicalName().equals(wrapperName)) {
			return char.class.getSimpleName();
		}
		if (Byte.class.getCanonicalName().equals(wrapperName)) {
			return byte.class.getSimpleName();
		}
		if (Boolean.class.getCanonicalName().equals(wrapperName)) {
			return boolean.class.getSimpleName();
		}
		if (Float.class.getCanonicalName().equals(wrapperName)) {
			return float.class.getSimpleName();
		}
		if (Double.class.getCanonicalName().equals(wrapperName)) {
			return double.class.getSimpleName();
		}

		return null;
	}

	/**
	 * Get the wrapper of the associated primitive type.
	 *
	 * @param primitiveName primitive name
	 * @return The wrapper of the associated primitive type.
	 */
	public static String getBoxedTypeName(final String primitiveName) {
		if (long.class.getSimpleName().equals(primitiveName)) {
			return Long.class.getCanonicalName();
		}
		if (int.class.getSimpleName().equals(primitiveName)) {
			return Integer.class.getCanonicalName();
		}
		if (short.class.getSimpleName().equals(primitiveName)) {
			return Short.class.getCanonicalName();
		}
		if (char.class.getSimpleName().equals(primitiveName)) {
			return Character.class.getCanonicalName();
		}
		if (byte.class.getSimpleName().equals(primitiveName)) {
			return Byte.class.getCanonicalName();
		}
		if (boolean.class.getSimpleName().equals(primitiveName)) {
			return Boolean.class.getCanonicalName();
		}
		if (float.class.getSimpleName().equals(primitiveName)) {
			return Float.class.getCanonicalName();
		}
		if (double.class.getSimpleName().equals(primitiveName)) {
			return Double.class.getCanonicalName();
		}

		return null;
	}

	/**
	 * Get the type of the associated primitive wrapper.
	 *
	 * @param type A primitive or wrapper type.
	 * @param ast  The AST.
	 * @return The type of the associated primitive wrapper.
	 */
	public static ITypeBinding getBoxedTypeBinding(final ITypeBinding type, final AST ast) {
		if (!type.isPrimitive()) {
			return type;
		}
		String boxedTypeName= getBoxedTypeName(type.getName());
		if (boxedTypeName == null) {
			return type;
		}
		ITypeBinding boxed= ast.resolveWellKnownType(boxedTypeName);
		if (boxed == null) {
			return type;
		}

		return boxed;
	}
}

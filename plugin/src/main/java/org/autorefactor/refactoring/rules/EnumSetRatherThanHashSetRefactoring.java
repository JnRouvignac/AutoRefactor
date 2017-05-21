/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - initial API and implementation
 * Copyright (C) 2017 Jean-Noël Rouvignac - fix NPE with Eclipse 4.5.2
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
package org.autorefactor.refactoring.rules;

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.arguments;
import static org.autorefactor.refactoring.ASTHelper.instanceOf;

import java.util.Collection;
import java.util.EnumSet;
import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.TypeLiteral;

public final class EnumSetRatherThanHashSetRefactoring extends
AbstractEnumCollectionReplacementRefactoring {

	@Override
	public String getDescription() {
		return "Converts creation of HashSet with enum as a type "
				+ "to invocation of static methods of EnumSet where possible";
	}

	@Override
	public String getName() {
		return "EnumSet instead of HashSet for enum types";
	}

	@Override
	String getImplType() {
		return "java.util.HashSet";
	}

	@Override
	String getInterfaceType() {
		return "java.util.Set";
	}

	/**
	 * Refactoring is not correct if argument for HashSet constructor is a
	 * Collection, but other than EnumSet. <br>
	 * In case of empty collection <code>EnumSet.copyOf</code> will throw an
	 * <code>IllegalArgumentException</code>, <br>
	 * and HashSet(Collection) will not. <br>
	 * 
	 * Other constructors can be replaced with
	 * <code>EnumSet.noneOf(Class)</code> method.
	 * 
	 * @see {@link java.util.EnumSet#copyOf(Collection)}
	 * @see {@link java.util.EnumSet#copyOf(EnumSet)}
	 * @see {@link java.util.EnumSet#noneOf(Class)}
	 * 
	 * @param cic
	 *            - class instance creation node to be replaced
	 * @param type
	 *            - type argument of the declaration
	 */
	@Override
	boolean replace(ClassInstanceCreation cic, Type... types) {

		if (types == null || types.length < 1) {
			return VISIT_SUBTREE;
		}
		Type type = types[0];
		ASTBuilder b = ctx.getASTBuilder();
		List<Expression> arguments = arguments(cic);
		final MethodInvocation invocation;
		if (!arguments.isEmpty()
				&& instanceOf(arguments.get(0), "java.util.Collection")) {
			Expression typeArg = arguments.get(0);
			if (!instanceOf(typeArg, "java.util.EnumSet")) {
				return VISIT_SUBTREE;
			}
			invocation = b.invoke("EnumSet", "copyOf", b.copy(typeArg));
		} else {
			TypeLiteral newTypeLiteral = ctx.getAST().newTypeLiteral();
			newTypeLiteral.setType(b.copy(type));
			invocation = b.invoke("EnumSet", "noneOf", newTypeLiteral);
		}
		ctx.getRefactorings().replace(cic, invocation);
		return DO_NOT_VISIT_SUBTREE;
	}

}

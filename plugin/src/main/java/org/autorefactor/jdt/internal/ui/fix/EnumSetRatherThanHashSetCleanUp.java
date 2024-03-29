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
package org.autorefactor.jdt.internal.ui.fix;

import java.util.Arrays;
import java.util.Collection;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.TypeLiteral;
import org.eclipse.text.edits.TextEditGroup;

/**
 * Replaces HashSet for enum type creation to EnumSet factory static methods.
 */
public final class EnumSetRatherThanHashSetCleanUp extends AbstractEnumCollectionReplacementCleanUp {
	@Override
	public String getName() {
		return MultiFixMessages.EnumSetRatherThanHashSetCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.EnumSetRatherThanHashSetCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.EnumSetRatherThanHashSetCleanUp_reason;
	}

	@Override
	public String getImplType() {
		return HashSet.class.getCanonicalName();
	}

	@Override
	public String getInterfaceType() {
		return Set.class.getCanonicalName();
	}

	@Override
	public Set<String> getClassesToImport() {
		return new HashSet<>(Arrays.asList(EnumSet.class.getCanonicalName()));
	}

	/**
	 * Cleanup is not correct if argument for HashSet constructor is a
	 * Collection, but other than EnumSet. <br>
	 * In case of empty collection <code>EnumSet.copyOf</code> will throw an
	 * <code>IllegalArgumentException</code>, <br>
	 * and HashSet(Collection) will not. <br>
	 * <br>
	 * Other constructors can be replaced with <code>EnumSet.noneOf(Class)</code>
	 * method. <br>
	 * <br>
	 *
	 * @param classInstanceCreation  - class instance creation node to be replaced
	 * @param type - type argument of the declaration
	 * @see java.util.EnumSet#noneOf(Class) <br>
	 */
	@Override
	boolean maybeReplace(final ClassInstanceCreation classInstanceCreation, final Set<String> alreadyImportedClasses, final Set<String> importsToAdd,
			final Type... types) {
		if (types == null || types.length == 0) {
			return true;
		}

		Type type= types[0];
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		List<Expression> arguments= classInstanceCreation.arguments();
		MethodInvocation invocation;
		Name newClassName= ASTNodeFactory.newName(ast, alreadyImportedClasses.contains(EnumSet.class.getCanonicalName()) ? EnumSet.class.getSimpleName() : EnumSet.class.getCanonicalName());

		if (!arguments.isEmpty() && ASTNodes.instanceOf(arguments.get(0), Collection.class.getCanonicalName())) {
			Expression typeArg= arguments.get(0);

			if (!ASTNodes.instanceOf(typeArg, EnumSet.class.getCanonicalName())) {
				return true;
			}

			invocation= ast.newMethodInvocation();
			invocation.setExpression(newClassName);
			invocation.setName(ast.newSimpleName("copyOf")); //$NON-NLS-1$
			invocation.arguments().add(ast.createCopyTarget(ASTNodes.getUnparenthesedExpression(typeArg)));
		} else {
			TypeLiteral newTypeLiteral= cuRewrite.getAST().newTypeLiteral();
			newTypeLiteral.setType(ast.createCopyTarget(type));
			invocation= ast.newMethodInvocation();
			invocation.setExpression(newClassName);
			invocation.setName(ast.newSimpleName("noneOf")); //$NON-NLS-1$
			invocation.arguments().add(newTypeLiteral);
		}

		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.EnumSetRatherThanHashSetCleanUp_description);

		rewrite.replace(classInstanceCreation, invocation, group);
		importsToAdd.add(EnumSet.class.getCanonicalName());
		return false;
	}
}

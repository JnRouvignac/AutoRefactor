/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Fabrice Tiercelin - initial API and implementation
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

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Statement;

/**
 * See {@link #getDescription()} method.
 */
public class JUnitAssertCleanUp extends AbstractUnitTestCleanUp {
	private static final String[] PACKAGE_PATHES= { "junit.framework.", "org.junit." }; //$NON-NLS-1$ //$NON-NLS-2$

	/**
	 * Get the name.
	 *
	 * @return the name.
	 */
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_JUnitAssertCleanUp_name;
	}

	/**
	 * Get the description.
	 *
	 * @return the description.
	 */
	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_JUnitAssertCleanUp_description;
	}

	/**
	 * Get the reason.
	 *
	 * @return the reason.
	 */
	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_JUnitAssertCleanUp_reason;
	}

	@Override
	protected Pair<Expression, Expression> getActualAndExpected(final Expression leftValue,
			final Expression rightValue) {
		return Pair.of(rightValue, leftValue);
	}

	@Override
	public boolean visit(final CompilationUnit node) {
		canUseAssertNotEquals= false;

		for (Object object : node.imports()) {
			ImportDeclaration importDeclaration= (ImportDeclaration) object;
			ITypeBinding typeBinding= resolveTypeBinding(importDeclaration);

			if (ASTNodes.hasType(typeBinding, "org.junit.Assert")) { //$NON-NLS-1$
				for (IMethodBinding mb : typeBinding.getDeclaredMethods()) {
					if (mb.toString().contains("assertNotEquals")) { //$NON-NLS-1$
						canUseAssertNotEquals= true;
						return super.visit(node);
					}
				}
			}
		}

		// New file: reset the value
		return super.visit(node);
	}

	@Override
	public boolean maybeRefactorMethodInvocation(final MethodInvocation node, final Set<String> classesToUseWithImport,
			final Set<String> importsToAdd) {
		List<Expression> args= ASTNodes.arguments(node);
		int i= 0;
		boolean shouldVisit= true;
		while (shouldVisit && i < PACKAGE_PATHES.length) {
			shouldVisit= maybeRefactorMethod(classesToUseWithImport, importsToAdd, node, PACKAGE_PATHES[i], args);
			i++;
		}

		return shouldVisit;
	}

	private boolean maybeRefactorMethod(final Set<String> classesToUseWithImport, final Set<String> importsToAdd,
			final MethodInvocation node, final String unitTestPackagePath, final List<Expression> args) {
		String jUnitClass= unitTestPackagePath + "Assert"; //$NON-NLS-1$

		if (ASTNodes.usesGivenSignature(node, jUnitClass, "assertTrue", boolean.class.getSimpleName())) { //$NON-NLS-1$
			return maybeRefactorStatement(classesToUseWithImport, importsToAdd, node, node, true, args.get(0), null, false);
		}

		if (ASTNodes.usesGivenSignature(node, jUnitClass, "assertTrue", String.class.getCanonicalName(), boolean.class.getSimpleName())) { //$NON-NLS-1$
			return maybeRefactorStatement(classesToUseWithImport, importsToAdd, node, node, true, args.get(1), args.get(0), false);
		}

		if (ASTNodes.usesGivenSignature(node, jUnitClass, "assertFalse", boolean.class.getSimpleName())) { //$NON-NLS-1$
			return maybeRefactorStatement(classesToUseWithImport, importsToAdd, node, node, false, args.get(0), null, false);
		}

		if (ASTNodes.usesGivenSignature(node, jUnitClass, "assertFalse", String.class.getCanonicalName(), boolean.class.getSimpleName())) { //$NON-NLS-1$
			return maybeRefactorStatement(classesToUseWithImport, importsToAdd, node, node, false, args.get(1), args.get(0), false);
		}

		for (Class<?> clazz : new Class<?>[]{boolean.class, int.class, long.class, double.class, float.class, short.class, char.class, byte.class, String.class, Object.class}) {
			if (ASTNodes.usesGivenSignature(node, jUnitClass, "assertEquals", clazz.getCanonicalName(), clazz.getCanonicalName())) { //$NON-NLS-1$
				return maybeRefactorToEquality(classesToUseWithImport, importsToAdd, node, node, true, args.get(1), args.get(0), null);
			}

			if (ASTNodes.usesGivenSignature(node, jUnitClass, "assertEquals", String.class.getCanonicalName(), clazz.getCanonicalName(), clazz.getCanonicalName())) { //$NON-NLS-1$
				return maybeRefactorToEquality(classesToUseWithImport, importsToAdd, node, node, true, args.get(2), args.get(1), args.get(0));
			}

			if (ASTNodes.usesGivenSignature(node, jUnitClass, "assertNotEquals", clazz.getCanonicalName(), clazz.getCanonicalName())) { //$NON-NLS-1$
				return maybeRefactorToEquality(classesToUseWithImport, importsToAdd, node, node, false, args.get(1), args.get(0), null);
			}

			if (ASTNodes.usesGivenSignature(node, jUnitClass, "assertNotEquals", String.class.getCanonicalName(), clazz.getCanonicalName(), clazz.getCanonicalName())) { //$NON-NLS-1$
				return maybeRefactorToEquality(classesToUseWithImport, importsToAdd, node, node, false, args.get(2), args.get(1), args.get(0));
			}
		}

		return true;
	}

	@Override
	public boolean maybeRefactorIfStatement(final IfStatement node, final Set<String> classesToUseWithImport,
			final Set<String> importsToAdd) {
		List<Statement> statements= ASTNodes.asList(node.getThenStatement());

		if (node.getElseStatement() == null && statements.size() == 1) {
			MethodInvocation mi= ASTNodes.asExpression(statements.get(0), MethodInvocation.class);
			int i= 0;
			boolean shouldVisit= true;
			while (shouldVisit && i < PACKAGE_PATHES.length) {
				shouldVisit= maybeRefactorIf(classesToUseWithImport, importsToAdd, node, mi, PACKAGE_PATHES[i]);
				i++;
			}

			return shouldVisit;
		}

		return true;
	}

	private boolean maybeRefactorIf(final Set<String> classesToUseWithImport, final Set<String> importsToAdd,
			final IfStatement node, final MethodInvocation mi, final String unitTestPackagePath) {
		if (ASTNodes.usesGivenSignature(mi, unitTestPackagePath + "Assert", "fail")) { //$NON-NLS-1$ //$NON-NLS-2$
			return maybeRefactorStatement(classesToUseWithImport, importsToAdd, node, mi, false, node.getExpression(), null, true);
		}

		if (ASTNodes.usesGivenSignature(mi, unitTestPackagePath + "Assert", "fail", String.class.getCanonicalName())) { //$NON-NLS-1$ //$NON-NLS-2$
			return maybeRefactorStatement(classesToUseWithImport, importsToAdd, node, mi, false, node.getExpression(), ASTNodes.arguments(mi).get(0), true);
		}

		return true;
	}

	@Override
	protected MethodInvocation invokeQualifiedMethod(Set<String> classesToUseWithImport, Set<String> importsToAdd,
			final ASTNodeFactory ast, final Expression copyOfExpression, final String methodName,
			final Expression copyOfActual, final Expression copyOfExpected, final Expression delta, final Expression failureMessage) {
		List<Expression> arguments= new ArrayList<>(4);

		if (failureMessage != null) {
			arguments.add(ast.createCopyTarget(failureMessage));
		}

		if (copyOfExpected != null) {
			arguments.add(copyOfExpected);
		}

		if (copyOfActual != null) {
			arguments.add(copyOfActual);
		}

		if (delta != null) {
			arguments.add(delta);
		}

		return ast.newMethodInvocation(copyOfExpression, methodName, arguments.toArray(new Expression[arguments.size()]));
	}
}

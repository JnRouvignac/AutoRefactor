/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Adapt for JUnit
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

import org.autorefactor.jdt.core.dom.ASTRewrite;
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

/**
 * See {@link #getDescription()} method.
 * <p>
 * FIXME: Assert.assertNotEquals() exists only since TestNG 6.1. This
 * cleanup should be made conditional on TestNG version.
 * </p>
 */
public class TestNGAssertCleanUp extends AbstractUnitTestCleanUp {
	private static final String TESTNG_CLASS= "org.testng.Assert"; //$NON-NLS-1$

	@Override
	public String getName() {
		return MultiFixMessages.TestNGAssertCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.TestNGAssertCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.TestNGAssertCleanUp_reason;
	}

	@Override
	protected Pair<Expression, Expression> getActualAndExpected(final Expression leftValue,
			final Expression rightValue) {
		return Pair.of(leftValue, rightValue);
	}

	@Override
	public boolean visit(final CompilationUnit visited) {
		canUseAssertNotEquals= false;

		for (Object object : visited.imports()) {
			ImportDeclaration importDeclaration= (ImportDeclaration) object;
			ITypeBinding typeBinding= resolveTypeBinding(importDeclaration);

			if (ASTNodes.hasType(typeBinding, TESTNG_CLASS)) {
				for (IMethodBinding mb : typeBinding.getDeclaredMethods()) {
					if (mb.toString().contains("assertNotEquals")) { //$NON-NLS-1$
						canUseAssertNotEquals= true;
						return super.visit(visited);
					}
				}
			}
		}

		// New file: reset the value
		return super.visit(visited);
	}

	@Override
	public boolean maybeRefactorMethodInvocation(final MethodInvocation visited, final Set<String> classesToUseWithImport,
			final Set<String> importsToAdd) {
		List<Expression> args= visited.arguments();

		if (ASTNodes.usesGivenSignature(visited, TESTNG_CLASS, "assertTrue", boolean.class.getSimpleName())) { //$NON-NLS-1$
			return maybeRefactorStatement(classesToUseWithImport, importsToAdd, visited, visited, true, args.get(0), null, false);
		}

		if (ASTNodes.usesGivenSignature(visited, TESTNG_CLASS, "assertTrue", boolean.class.getSimpleName(), String.class.getCanonicalName())) { //$NON-NLS-1$
			return maybeRefactorStatement(classesToUseWithImport, importsToAdd, visited, visited, true, args.get(0), args.get(1), false);
		}

		if (ASTNodes.usesGivenSignature(visited, TESTNG_CLASS, "assertFalse", boolean.class.getSimpleName())) { //$NON-NLS-1$
			return maybeRefactorStatement(classesToUseWithImport, importsToAdd, visited, visited, false, args.get(0), null, false);
		}

		if (ASTNodes.usesGivenSignature(visited, TESTNG_CLASS, "assertFalse", boolean.class.getSimpleName(), String.class.getCanonicalName())) { //$NON-NLS-1$
			return maybeRefactorStatement(classesToUseWithImport, importsToAdd, visited, visited, false, args.get(0), args.get(1), false);
		}

		if (ASTNodes.usesGivenSignature(visited, TESTNG_CLASS, "assertEquals", boolean.class.getSimpleName(), boolean.class.getSimpleName())) { //$NON-NLS-1$
			if (ASTNodes.getBooleanLiteral(args.get(1)) != null) {
				return maybeRefactorStatement(classesToUseWithImport, importsToAdd, visited, visited, ASTNodes.getBooleanLiteral(args.get(1)), args.get(0), null, true);
			}

			if (ASTNodes.getBooleanLiteral(args.get(0)) != null) {
				return maybeRefactorStatement(classesToUseWithImport, importsToAdd, visited, visited, ASTNodes.getBooleanLiteral(args.get(0)), args.get(1), null, true);
			}
		} else if (ASTNodes.usesGivenSignature(visited, TESTNG_CLASS, "assertEquals", boolean.class.getSimpleName(), boolean.class.getSimpleName(), String.class.getCanonicalName())) { //$NON-NLS-1$
			if (ASTNodes.getBooleanLiteral(args.get(1)) != null) {
				return maybeRefactorStatement(classesToUseWithImport, importsToAdd, visited, visited, ASTNodes.getBooleanLiteral(args.get(1)), args.get(0), args.get(2), true);
			}

			if (ASTNodes.getBooleanLiteral(args.get(0)) != null) {
				return maybeRefactorStatement(classesToUseWithImport, importsToAdd, visited, visited, ASTNodes.getBooleanLiteral(args.get(0)), args.get(1), args.get(2), true);
			}
		}

		for (Class<?> clazz : new Class<?>[]{boolean.class, int.class, long.class, double.class, float.class, short.class, char.class, byte.class, String.class, Object.class}) {
			if (ASTNodes.usesGivenSignature(visited, TESTNG_CLASS, "assertEquals", clazz.getCanonicalName(), clazz.getCanonicalName())) { //$NON-NLS-1$
				return maybeRefactorToEquality(classesToUseWithImport, importsToAdd, visited, visited, true, args.get(0), args.get(1), null);
			}

			if (ASTNodes.usesGivenSignature(visited, TESTNG_CLASS, "assertEquals", clazz.getCanonicalName(), clazz.getCanonicalName(), String.class.getCanonicalName())) { //$NON-NLS-1$
				return maybeRefactorToEquality(classesToUseWithImport, importsToAdd, visited, visited, true, args.get(0), args.get(1), args.get(2));
			}

			if (ASTNodes.usesGivenSignature(visited, TESTNG_CLASS, "assertNotEquals", clazz.getCanonicalName(), clazz.getCanonicalName())) { //$NON-NLS-1$
				return maybeRefactorToEquality(classesToUseWithImport, importsToAdd, visited, visited, false, args.get(0), args.get(1), null);
			}

			if (ASTNodes.usesGivenSignature(visited, TESTNG_CLASS, "assertNotEquals", clazz.getCanonicalName(), clazz.getCanonicalName(), String.class.getCanonicalName())) { //$NON-NLS-1$
				return maybeRefactorToEquality(classesToUseWithImport, importsToAdd, visited, visited, false, args.get(0), args.get(1), args.get(2));
			}
		}

		return true;
	}

	@Override
	public boolean maybeRefactorIfStatement(final IfStatement visited, final Set<String> classesToUseWithImport,
			final Set<String> importsToAdd) {
		MethodInvocation methodInvocation= ASTNodes.asExpression(visited.getThenStatement(), MethodInvocation.class);

		if (visited.getElseStatement() == null && methodInvocation != null) {
			if (ASTNodes.usesGivenSignature(methodInvocation, TESTNG_CLASS, "fail")) { //$NON-NLS-1$
				return maybeRefactorStatement(classesToUseWithImport, importsToAdd, visited, methodInvocation, false, visited.getExpression(), null, true);
			}

			if (ASTNodes.usesGivenSignature(methodInvocation, TESTNG_CLASS, "fail", String.class.getCanonicalName())) { //$NON-NLS-1$
				return maybeRefactorIfObjectsAreNotUsed(classesToUseWithImport, importsToAdd, visited, methodInvocation, false, visited.getExpression(), ((List<Expression>) methodInvocation.arguments()).get(0));
			}
		}

		return true;
	}

	@Override
	protected MethodInvocation invokeQualifiedMethod(final Set<String> classesToUseWithImport, final Set<String> importsToAdd,
			final Expression copyOfExpression, final String methodName, final Expression copyOfActual,
			final Expression copyOfExpected, final Expression delta, final Expression failureMessage) {
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		List<Expression> arguments= new ArrayList<>(4);

		if (copyOfActual != null) {
			arguments.add(copyOfActual);
		}

		if (copyOfExpected != null) {
			arguments.add(copyOfExpected);
		}

		if (delta != null) {
			arguments.add(delta);
		}

		if (failureMessage != null) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();

			arguments.add(ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(failureMessage)));
		}

		return ast.newMethodInvocation(copyOfExpression, methodName, arguments.toArray(new Expression[arguments.size()]));
	}
}

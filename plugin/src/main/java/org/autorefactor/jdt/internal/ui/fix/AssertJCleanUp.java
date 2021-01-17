/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - initial API and implementation
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
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.NullLiteral;

/**
 * See {@link #getDescription()} method.
 */
public class AssertJCleanUp extends AbstractUnitTestCleanUp {
	private static final String FAIL_CLASS= "org.assertj.core.api.Fail"; //$NON-NLS-1$
	private static final String ASSERTIONS_CLASS= "org.assertj.core.api.Assertions"; //$NON-NLS-1$
	private static final String DESCRIPTABLE_INTERFACE= "org.assertj.core.api.Descriptable"; //$NON-NLS-1$
	private static final String ABSTRACT_ASSERT_CLASS= "org.assertj.core.api.AbstractAssert"; //$NON-NLS-1$
	private static final String BOOLEAN_ASSERT_CLASS= "org.assertj.core.api.AbstractBooleanAssert"; //$NON-NLS-1$
	private static final String OFFSET_CLASS= "org.assertj.core.data.Offset"; //$NON-NLS-1$

	private static final String FAIL_METHOD= "fail"; //$NON-NLS-1$
	private static final String IS_NOT_EQUAL_TO_METHOD= "isNotEqualTo"; //$NON-NLS-1$
	private static final String DESCRIBED_AS_METHOD= "describedAs"; //$NON-NLS-1$
	private static final String AS_METHOD= "as"; //$NON-NLS-1$
	private static final String EQUALS_METHOD= "equals"; //$NON-NLS-1$
	private static final String IS_EQUAL_TO_METHOD= "isEqualTo"; //$NON-NLS-1$
	private static final String IS_FALSE_METHOD= "isFalse"; //$NON-NLS-1$
	private static final String IS_TRUE_METHOD= "isTrue"; //$NON-NLS-1$
	private static final String ASSERT_THAT_METHOD= "assertThat"; //$NON-NLS-1$

	/**
	 * Init canUseAssertNotEquals.
	 */
	public AssertJCleanUp() {
		canUseAssertNotEquals= true;
	}

	@Override
	public String getName() {
		return MultiFixMessages.AssertJCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.AssertJCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.AssertJCleanUp_reason;
	}

	@Override
	protected Pair<Expression, Expression> getActualAndExpected(final Expression leftValue,
			final Expression rightValue) {
		return Pair.of(rightValue, leftValue);
	}

	@Override
	public Set<String> getClassesToImport() {
		return new HashSet<>(Arrays.asList(OFFSET_CLASS));
	}

	@Override
	public boolean maybeRefactorMethodInvocation(final MethodInvocation visited, final Set<String> classesToUseWithImport,
			final Set<String> importsToAdd) {
		MethodInvocation actual= ASTNodes.as(visited.getExpression(), MethodInvocation.class);
		Expression message= null;

		if (actual != null
				&& (ASTNodes.usesGivenSignature(actual, DESCRIPTABLE_INTERFACE, AS_METHOD, String.class.getCanonicalName(), Object[].class.getCanonicalName())
						|| ASTNodes.usesGivenSignature(actual, DESCRIPTABLE_INTERFACE, DESCRIBED_AS_METHOD, String.class.getCanonicalName(), Object[].class.getCanonicalName()))) {
			message= actual;
			actual= ASTNodes.as(actual.getExpression(), MethodInvocation.class);
		}

		if (actual != null
				&& ASSERT_THAT_METHOD.equals(actual.getName().getIdentifier())
				&& actual.resolveMethodBinding() != null
				&& ASTNodes.hasType(actual.resolveMethodBinding().getDeclaringClass(), ASSERTIONS_CLASS)
				&& actual.arguments().size() == 1) {
			Expression actualExpression= (Expression) actual.arguments().get(0);

			if (ASTNodes.usesGivenSignature(visited, BOOLEAN_ASSERT_CLASS, IS_TRUE_METHOD)) {
				return maybeRefactorStatement(classesToUseWithImport, importsToAdd,
						visited, actual, true,
						actualExpression, message, false);
			}

			if (ASTNodes.usesGivenSignature(visited, BOOLEAN_ASSERT_CLASS, IS_FALSE_METHOD)) {
				return maybeRefactorStatement(classesToUseWithImport, importsToAdd,
						visited, actual, false,
						actualExpression, message, false);
			}

			if (ASTNodes.usesGivenSignature(visited, BOOLEAN_ASSERT_CLASS, IS_EQUAL_TO_METHOD, boolean.class.getCanonicalName())) {
				final Boolean booleanConstant= ASTNodes.getBooleanLiteral((Expression) visited.arguments().get(0));

				if (booleanConstant != null) {
					return maybeRefactorStatement(classesToUseWithImport, importsToAdd,
							visited, actual, booleanConstant,
							actualExpression, message, true);
				}

				final Boolean actualConstant= ASTNodes.getBooleanLiteral(actualExpression);

				if (actualConstant != null) {
					return maybeRefactorStatement(classesToUseWithImport, importsToAdd,
							visited, actual, actualConstant,
							(Expression) visited.arguments().get(0), message, true);
				}
			} else if (ASTNodes.usesGivenSignature(visited, BOOLEAN_ASSERT_CLASS, IS_NOT_EQUAL_TO_METHOD, boolean.class.getCanonicalName())) {
				final Boolean booleanConstant= ASTNodes.getBooleanLiteral((Expression) visited.arguments().get(0));

				if (booleanConstant != null) {
					return maybeRefactorStatement(classesToUseWithImport, importsToAdd,
							visited, actual, !booleanConstant,
							actualExpression, message, true);
				}

				final Boolean actualConstant= ASTNodes.getBooleanLiteral(actualExpression);

				if (actualConstant != null) {
					return maybeRefactorStatement(classesToUseWithImport, importsToAdd,
							visited, actual, !actualConstant,
							(Expression) visited.arguments().get(0), message, true);
				}
			}

			if (ASTNodes.usesGivenSignature(visited, ABSTRACT_ASSERT_CLASS, EQUALS_METHOD, Object.class.getCanonicalName())
					|| ASTNodes.usesGivenSignature(visited, ABSTRACT_ASSERT_CLASS, IS_EQUAL_TO_METHOD, Object.class.getCanonicalName())
					|| ASTNodes.usesGivenSignature(visited, BOOLEAN_ASSERT_CLASS, IS_EQUAL_TO_METHOD, boolean.class.getCanonicalName())
					|| ASTNodes.usesGivenSignature(visited, "org.assertj.core.api.AbstractIntegerAssert", IS_EQUAL_TO_METHOD, int.class.getCanonicalName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, "org.assertj.core.api.AbstractLongAssert", IS_EQUAL_TO_METHOD, long.class.getCanonicalName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, "org.assertj.core.api.AbstractDoubleAssert", IS_EQUAL_TO_METHOD, double.class.getCanonicalName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, "org.assertj.core.api.AbstractFloatAssert", IS_EQUAL_TO_METHOD, float.class.getCanonicalName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, "org.assertj.core.api.AbstractShortAssert", IS_EQUAL_TO_METHOD, short.class.getCanonicalName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, "org.assertj.core.api.AbstractCharacterAssert", IS_EQUAL_TO_METHOD, char.class.getCanonicalName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, "org.assertj.core.api.AbstractByteAssert", IS_EQUAL_TO_METHOD, byte.class.getCanonicalName())) { //$NON-NLS-1$
				return maybeRefactorToEquality(classesToUseWithImport, importsToAdd, visited,
						actual, true, actualExpression, (Expression) visited.arguments().get(0), message);
			}

			if (ASTNodes.usesGivenSignature(visited, ABSTRACT_ASSERT_CLASS, IS_NOT_EQUAL_TO_METHOD, Object.class.getCanonicalName())
					|| ASTNodes.usesGivenSignature(visited, BOOLEAN_ASSERT_CLASS, IS_NOT_EQUAL_TO_METHOD, boolean.class.getCanonicalName())
					|| ASTNodes.usesGivenSignature(visited, "org.assertj.core.api.AbstractIntegerAssert", IS_NOT_EQUAL_TO_METHOD, int.class.getCanonicalName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, "org.assertj.core.api.AbstractLongAssert", IS_NOT_EQUAL_TO_METHOD, long.class.getCanonicalName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, "org.assertj.core.api.AbstractDoubleAssert", IS_NOT_EQUAL_TO_METHOD, double.class.getCanonicalName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, "org.assertj.core.api.AbstractFloatAssert", IS_NOT_EQUAL_TO_METHOD, float.class.getCanonicalName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, "org.assertj.core.api.AbstractShortAssert", IS_NOT_EQUAL_TO_METHOD, short.class.getCanonicalName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, "org.assertj.core.api.AbstractCharacterAssert", IS_NOT_EQUAL_TO_METHOD, char.class.getCanonicalName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, "org.assertj.core.api.AbstractByteAssert", IS_NOT_EQUAL_TO_METHOD, byte.class.getCanonicalName())) { //$NON-NLS-1$
				return maybeRefactorToEquality(classesToUseWithImport, importsToAdd, visited,
						actual, false, actualExpression, (Expression) visited.arguments().get(0), message);
			}
		}

		return true;
	}

	@Override
	public boolean maybeRefactorIfStatement(final IfStatement visited, final Set<String> classesToUseWithImport,
			final Set<String> importsToAdd) {
		MethodInvocation methodInvocation= ASTNodes.asExpression(visited.getThenStatement(), MethodInvocation.class);

		if (visited.getElseStatement() == null
				&& methodInvocation != null
				&& (ASTNodes.usesGivenSignature(methodInvocation, ASSERTIONS_CLASS, FAIL_METHOD, String.class.getCanonicalName())
						|| ASTNodes.usesGivenSignature(methodInvocation, FAIL_CLASS, FAIL_METHOD, String.class.getCanonicalName())
						|| ASTNodes.usesGivenSignature(methodInvocation, ASSERTIONS_CLASS, FAIL_METHOD, String.class.getCanonicalName(), Object[].class.getCanonicalName())
						|| ASTNodes.usesGivenSignature(methodInvocation, FAIL_CLASS, FAIL_METHOD, String.class.getCanonicalName(), Object[].class.getCanonicalName()))) {
			if (methodInvocation.arguments() == null
					|| methodInvocation.arguments().size() == 1 && ASTNodes.as((Expression) methodInvocation.arguments().get(0), NullLiteral.class) != null) {
				return maybeRefactorStatement(classesToUseWithImport, importsToAdd, visited, methodInvocation, false, visited.getExpression(), null, true);
			}

			return maybeRefactorIfObjectsAreNotUsed(classesToUseWithImport, importsToAdd, visited, methodInvocation, false, visited.getExpression(), methodInvocation);
		}

		return true;
	}

	@Override
	protected MethodInvocation invokeMethod(final Set<String> classesToUseWithImport, final Set<String> importsToAdd,
			final MethodInvocation originalMethod, final String methodName, final Expression copyOfActual,
			final Expression copyOfExpected, final Expression delta, final Expression failureMessage) {
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		String qualifiedClassName= originalMethod.resolveMethodBinding().getDeclaringClass().getQualifiedName();

		Expression qualifiedClass;
		if (!FAIL_METHOD.equals(methodName) && FAIL_CLASS.equals(qualifiedClassName)) {
			qualifiedClassName= ASSERTIONS_CLASS;
			qualifiedClass= null;
		} else if (originalMethod.getExpression() != null) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();

			qualifiedClass= ASTNodes.createMoveTarget(rewrite, originalMethod.getExpression());
		} else {
			qualifiedClass= null;
		}

		if (originalMethod.getExpression() == null && !staticImports.contains(qualifiedClassName + "." + methodName) //$NON-NLS-1$
				&& !staticImports.contains(qualifiedClassName + ".*")) { //$NON-NLS-1$
			qualifiedClass= ASTNodeFactory.newName(ast, qualifiedClassName);
		}

		if (FAIL_METHOD.equals(methodName)) {
			return invokeFail(failureMessage, qualifiedClass);
		}

		return invokeQualifiedMethod(classesToUseWithImport, importsToAdd, qualifiedClass, methodName, copyOfActual, copyOfExpected, delta, failureMessage);
	}

	private MethodInvocation invokeFail(final Expression failureMessage, final Expression qualifiedClass) {
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		if (failureMessage != null) {
			MethodInvocation failureMethod= (MethodInvocation) failureMessage;
			List<Expression> copyOfMessages= new ArrayList<>(failureMethod.arguments().size());

			for (Object message : failureMethod.arguments()) {
				ASTRewrite rewrite= cuRewrite.getASTRewrite();

				copyOfMessages.add(ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression((Expression) message)));
			}

			MethodInvocation newMethodInvocation= ast.newMethodInvocation();
			newMethodInvocation.setExpression(qualifiedClass);
			newMethodInvocation.setName(ast.newSimpleName(FAIL_METHOD));
			newMethodInvocation.arguments().addAll(copyOfMessages);
			return newMethodInvocation;
		}

		MethodInvocation methodInvocation= ast.newMethodInvocation();
		methodInvocation.setExpression(qualifiedClass);
		methodInvocation.setName(ast.newSimpleName(FAIL_METHOD));
		methodInvocation.arguments().add(ast.newNullLiteral());
		return methodInvocation;
	}

	@Override
	protected MethodInvocation invokeQualifiedMethod(final Set<String> classesToUseWithImport, final Set<String> importsToAdd,
			final Expression copyOfClass, final String methodName, final Expression copyOfActual,
			final Expression copyOfExpected, final Expression delta, final Expression failureMessage) {
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		String finalMethodName= getFinalMethodName(methodName);
		MethodInvocation assertionMethod= ast.newMethodInvocation();
		assertionMethod.setExpression(copyOfClass);
		assertionMethod.setName(ast.newSimpleName(ASSERT_THAT_METHOD));
		assertionMethod.arguments().add(copyOfActual);

		if (failureMessage != null) {
			MethodInvocation failureMethod= (MethodInvocation) failureMessage;
			String method= failureMethod.getName().getIdentifier();
			List<Expression> copyOfMessages= new ArrayList<>(failureMethod.arguments().size());

			for (Object message : failureMethod.arguments()) {
				ASTRewrite rewrite= cuRewrite.getASTRewrite();

				copyOfMessages.add(ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression((Expression) message)));
			}

			MethodInvocation newMethodInvocation= ast.newMethodInvocation();
			newMethodInvocation.setExpression(assertionMethod);
			newMethodInvocation.setName(ast.newSimpleName(DESCRIBED_AS_METHOD.equals(method) ? method : AS_METHOD));
			newMethodInvocation.arguments().addAll(copyOfMessages);

			assertionMethod= newMethodInvocation;
		}

		if (copyOfExpected != null) {
			if (delta != null && IS_EQUAL_TO_METHOD.equals(finalMethodName)) {
				importsToAdd.add(OFFSET_CLASS);
				String offsetClassname= classesToUseWithImport.contains(OFFSET_CLASS) ? "Offset" : OFFSET_CLASS; //$NON-NLS-1$

				MethodInvocation offsetMethod= ast.newMethodInvocation();
				offsetMethod.setExpression(ASTNodeFactory.newName(ast, offsetClassname));
				offsetMethod.setName(ast.newSimpleName("offset")); //$NON-NLS-1$
				offsetMethod.arguments().add(delta);
				MethodInvocation newMethodInvocation= ast.newMethodInvocation();
				newMethodInvocation.setExpression(assertionMethod);
				newMethodInvocation.setName(ast.newSimpleName(finalMethodName));
				newMethodInvocation.arguments().add(copyOfExpected);
				newMethodInvocation.arguments().add(offsetMethod);
				return newMethodInvocation;
			}

			MethodInvocation newMethodInvocation= ast.newMethodInvocation();
			newMethodInvocation.setExpression(assertionMethod);
			newMethodInvocation.setName(ast.newSimpleName(finalMethodName));
			newMethodInvocation.arguments().add(copyOfExpected);
			return newMethodInvocation;
		}

		MethodInvocation methodInvocation= ast.newMethodInvocation();
		methodInvocation.setExpression(assertionMethod);
		methodInvocation.setName(ast.newSimpleName(finalMethodName));
		return methodInvocation;
	}

	private String getFinalMethodName(final String methodName) {
		switch (methodName) {
		case "assertTrue": //$NON-NLS-1$
			return IS_TRUE_METHOD;

		case "assertFalse": //$NON-NLS-1$
			return IS_FALSE_METHOD;

		case "assertNull": //$NON-NLS-1$
			return "isNull"; //$NON-NLS-1$

		case "assertNotNull": //$NON-NLS-1$
			return "isNotNull"; //$NON-NLS-1$

		case "assertEquals": //$NON-NLS-1$
			return IS_EQUAL_TO_METHOD;

		case "assertNotEquals": //$NON-NLS-1$
			return IS_NOT_EQUAL_TO_METHOD;

		case "assertSame": //$NON-NLS-1$
			return "isSameAs"; //$NON-NLS-1$

		case "assertNotSame": //$NON-NLS-1$
			return "isNotSameAs"; //$NON-NLS-1$
		}

		return null;
	}
}

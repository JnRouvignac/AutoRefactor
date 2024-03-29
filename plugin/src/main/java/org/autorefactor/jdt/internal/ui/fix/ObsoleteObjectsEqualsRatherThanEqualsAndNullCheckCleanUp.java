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
package org.autorefactor.jdt.internal.ui.fix;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.OrderedInfixExpression;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BooleanLiteral;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.NullLiteral;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class ObsoleteObjectsEqualsRatherThanEqualsAndNullCheckCleanUp extends NewClassImportCleanUp {
	private static final String EQUALS_METHOD= "equals"; //$NON-NLS-1$

	private final class RefactoringWithObjectsClass extends CleanUpWithNewClassImport {
		@Override
		public boolean visit(final IfStatement node) {
			return maybeRefactorIfStatement(node, getClassesToUseWithImport(), getImportsToAdd());
		}
	}

	@Override
	public String getName() {
		return MultiFixMessages.ObsoleteObjectsEqualsRatherThanEqualsAndNullCheckCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ObsoleteObjectsEqualsRatherThanEqualsAndNullCheckCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ObsoleteObjectsEqualsRatherThanEqualsAndNullCheckCleanUp_reason;
	}

	@Override
	public boolean isJavaVersionSupported(final Release javaSeRelease) {
		return javaSeRelease.getMinorVersion() >= 7;
	}

	@Override
	public Set<String> getClassesToImport() {
		return new HashSet<>(Arrays.asList(Objects.class.getCanonicalName()));
	}

	@Override
	public CleanUpWithNewClassImport getRefactoringClassInstance() {
		return new RefactoringWithObjectsClass();
	}

	@Override
	public boolean visit(final IfStatement node) {
		return maybeRefactorIfStatement(node, getAlreadyImportedClasses(node), new HashSet<String>());
	}

	private boolean maybeRefactorIfStatement(final IfStatement node, final Set<String> classesToUseWithImport,
			final Set<String> importsToAdd) {
		if (node.getElseStatement() != null) {
			InfixExpression condition= ASTNodes.as(node.getExpression(), InfixExpression.class);
			List<Statement> thenStatements= ASTNodes.asList(node.getThenStatement());
			List<Statement> elseStatements= ASTNodes.asList(node.getElseStatement());

			if (condition != null
					&& !condition.hasExtendedOperands()
					&& ASTNodes.hasOperator(condition, InfixExpression.Operator.EQUALS, InfixExpression.Operator.NOT_EQUALS)
					&& thenStatements != null
					&& thenStatements.size() == 1
					&& elseStatements != null
					&& elseStatements.size() == 1) {
				OrderedInfixExpression<Expression, NullLiteral> nullityOrderedCondition= ASTNodes.orderedInfix(condition, Expression.class, NullLiteral.class);

				if (nullityOrderedCondition != null && ASTNodes.isPassive(nullityOrderedCondition.getFirstOperand())) {
					return maybeReplaceCode(node, condition, thenStatements, elseStatements, nullityOrderedCondition.getFirstOperand(), classesToUseWithImport,
							importsToAdd);
				}
			}
		}

		return true;
	}

	private boolean maybeReplaceCode(final IfStatement node, final InfixExpression condition,
			final List<Statement> thenStatements, final List<Statement> elseStatements, final Expression firstField,
			final Set<String> classesToUseWithImport, final Set<String> importsToAdd) {
		IfStatement checkNullityStatement;
		IfStatement checkEqualsStatement;

		if (ASTNodes.hasOperator(condition, InfixExpression.Operator.EQUALS)) {
			checkNullityStatement= ASTNodes.as(thenStatements.get(0), IfStatement.class);
			checkEqualsStatement= ASTNodes.as(elseStatements.get(0), IfStatement.class);
		} else {
			checkEqualsStatement= ASTNodes.as(thenStatements.get(0), IfStatement.class);
			checkNullityStatement= ASTNodes.as(elseStatements.get(0), IfStatement.class);
		}

		if (checkNullityStatement != null
				&& checkNullityStatement.getElseStatement() == null
				&& checkEqualsStatement != null
				&& checkEqualsStatement.getElseStatement() == null) {
			InfixExpression nullityCondition= ASTNodes.as(checkNullityStatement.getExpression(), InfixExpression.class);
			ReturnStatement nullityStatement= ASTNodes.as(checkNullityStatement.getThenStatement(), ReturnStatement.class);

			PrefixExpression equalsCondition= ASTNodes.as(checkEqualsStatement.getExpression(), PrefixExpression.class);
			ReturnStatement equalsStatement= ASTNodes.as(checkEqualsStatement.getThenStatement(), ReturnStatement.class);

			if (nullityCondition != null
					&& !nullityCondition.hasExtendedOperands()
					&& ASTNodes.hasOperator(nullityCondition, InfixExpression.Operator.NOT_EQUALS)
					&& nullityStatement != null
					&& equalsCondition != null
					&& ASTNodes.hasOperator(equalsCondition, PrefixExpression.Operator.NOT)
					&& equalsStatement != null) {
				return maybeReplaceEquals(node, firstField, nullityCondition, nullityStatement, equalsCondition,
						equalsStatement, classesToUseWithImport, importsToAdd);
			}
		}

		return true;
	}

	private boolean maybeReplaceEquals(final IfStatement node, final Expression firstField,
			final InfixExpression nullityCondition, final ReturnStatement returnStatement1,
			final PrefixExpression equalsCondition, final ReturnStatement returnStatement2,
			final Set<String> classesToUseWithImport, final Set<String> importsToAdd) {
		OrderedInfixExpression<Expression, NullLiteral> nullityOrderedCondition= ASTNodes.orderedInfix(nullityCondition, Expression.class, NullLiteral.class);
		MethodInvocation equalsMethod= ASTNodes.as(equalsCondition.getOperand(), MethodInvocation.class);

		if (nullityOrderedCondition != null
				&& returnStatement1 != null
				&& returnStatement2 != null
				&& equalsMethod != null
				&& equalsMethod.getExpression() != null
				&& EQUALS_METHOD.equals(equalsMethod.getName().getIdentifier())
				&& equalsMethod.arguments() != null
				&& equalsMethod.arguments().size() == 1) {
			Expression secondField= nullityOrderedCondition.getFirstOperand();

			if (secondField != null
					&& (match(firstField, secondField, equalsMethod.getExpression(),
							(ASTNode) equalsMethod.arguments().get(0))
							|| match(secondField, firstField, equalsMethod.getExpression(),
									(ASTNode) equalsMethod.arguments().get(0)))) {
				BooleanLiteral returnFalse1= ASTNodes.as(returnStatement1.getExpression(), BooleanLiteral.class);
				BooleanLiteral returnFalse2= ASTNodes.as(returnStatement2.getExpression(), BooleanLiteral.class);

				if (returnFalse1 != null
						&& !returnFalse1.booleanValue()
						&& returnFalse2 != null
						&& !returnFalse2.booleanValue()) {
					replaceEquals(node, firstField, classesToUseWithImport, importsToAdd, secondField, returnStatement1);
					return false;
				}
			}
		}

		return true;
	}

	private boolean match(final Expression firstField, final Expression secondField, final Expression thisObject,
			final ASTNode otherObject) {
		return ASTNodes.match(thisObject, firstField) && ASTNodes.match(otherObject, secondField);
	}

	private void replaceEquals(final IfStatement node, final Expression firstField,
			final Set<String> classesToUseWithImport, final Set<String> importsToAdd, final Expression secondField,
			final ReturnStatement returnStatement) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteObjectsEqualsRatherThanEqualsAndNullCheckCleanUp_description);

		String classnameText= addImport(Objects.class, classesToUseWithImport, importsToAdd);

		ReturnStatement copyOfReturnStatement= ASTNodes.createMoveTarget(rewrite, returnStatement);
		MethodInvocation newMethodInvocation= ast.newMethodInvocation();
		newMethodInvocation.setExpression(ASTNodeFactory.newName(ast, classnameText));
		newMethodInvocation.setName(ast.newSimpleName(EQUALS_METHOD));
		newMethodInvocation.arguments().add(ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(firstField)));
		newMethodInvocation.arguments().add(ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(secondField)));

		ASTNodes.replaceButKeepComment(rewrite, node.getExpression(),
				ast.not(newMethodInvocation), group);

		if (node.getThenStatement() instanceof Block) {
			ASTNodes.replaceButKeepComment(rewrite, (ASTNode) ((Block) node.getThenStatement()).statements().get(0), copyOfReturnStatement, group);
		} else {
			ASTNodes.replaceButKeepComment(rewrite, node.getThenStatement(), copyOfReturnStatement, group);
		}

		if (node.getElseStatement() != null) {
			rewrite.remove(node.getElseStatement(), group);
		}
	}
}

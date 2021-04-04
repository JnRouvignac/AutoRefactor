/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2017 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016-2017 Fabrice Tiercelin - Make sure we do not visit again modified nodes
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

import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Bindings;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class StringCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.StringCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.StringCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.StringCleanUp_reason;
	}

	@Override
	public boolean visit(final MethodInvocation visited) {
		ASTNode parent= visited.getParent();
		boolean isStringValueOf= isStringValueOf(visited);

		if (ASTNodes.usesGivenSignature(visited, Object.class.getCanonicalName(), "toString")) { //$NON-NLS-1$
			Expression stringExpression= visited.getExpression();

			if (ASTNodes.hasType(stringExpression, String.class.getCanonicalName())) {
				// If node is already a String, no need to call toString()
				removeToString(visited);
				return false;
			}

			if (parent instanceof InfixExpression && ASTNodes.hasOperator((InfixExpression) parent, InfixExpression.Operator.PLUS)) {
				// If node is in a String context, no need to call toString()
				InfixExpression infixExpression= (InfixExpression) parent;
				Expression leftOperand= infixExpression.getLeftOperand();
				Expression rightOperand= infixExpression.getRightOperand();
				boolean leftOperandIsString= ASTNodes.hasType(leftOperand, String.class.getCanonicalName());
				boolean rightOperandIsString= ASTNodes.hasType(rightOperand, String.class.getCanonicalName());
				MethodInvocation lmi= ASTNodes.as(leftOperand, MethodInvocation.class);
				MethodInvocation rmi= ASTNodes.as(rightOperand, MethodInvocation.class);

				if ((leftOperandIsString || rightOperandIsString)
						&& visited.getLocationInParent() != InfixExpression.LEFT_OPERAND_PROPERTY
						&& visited.getLocationInParent() != InfixExpression.RIGHT_OPERAND_PROPERTY) {
					// Node is in the extended operands
					removeToString(visited);
					return false;
				}

				if (leftOperandIsString && ASTNodes.usesGivenSignature(rmi, Object.class.getCanonicalName(), "toString")) { //$NON-NLS-1$
					removeToString(rmi);
					return false;
				}

				if (rightOperandIsString && visited.getLocationInParent() == InfixExpression.LEFT_OPERAND_PROPERTY) {
					removeToString(lmi);
					return false;
				}
			}
		} else if (isStringValueOf
				&& ASTNodes.hasType((Expression) visited.arguments().get(0), String.class.getCanonicalName())
				&& (visited.arguments().get(0) instanceof StringLiteral || visited.arguments().get(0) instanceof InfixExpression)) {
			removeValueOf(visited);
			return false;
		}

		return true;
	}

	@Override
	public boolean visit(final InfixExpression visited) {
		if (ASTNodes.hasOperator(visited, InfixExpression.Operator.PLUS)) {
			List<Expression> allOperands= ASTNodes.allOperands(visited);

			for (int i= 0; i < allOperands.size(); i++) {
				Expression operand= allOperands.get(i);
				MethodInvocation valueOfMethod= ASTNodes.as(operand, MethodInvocation.class);

				if (valueOfMethod != null && (isStringValueOf(valueOfMethod) || isToStringForPrimitive(valueOfMethod))) {
					// If node is in a String context, no need to call toString()
					if (i == 0) {
						if (ASTNodes.hasType(visited.getRightOperand(), String.class.getCanonicalName()) && !maybeReplaceStringValueOfByArg0(visited.getLeftOperand(), valueOfMethod)) {
							return false;
						}
					} else if (i == 1) {
						if (ASTNodes.hasType(visited.getLeftOperand(), String.class.getCanonicalName()) && !maybeReplaceStringValueOfByArg0(visited.getRightOperand(), valueOfMethod)) {
							return false;
						}
					} else if (!maybeReplaceStringValueOfByArg0(valueOfMethod, valueOfMethod)) {
						// Left or right operation is necessarily a string, so just replace
						return false;
					}
				}
			}
		}

		return true;
	}

	private boolean isToStringForPrimitive(final MethodInvocation visited) {
		return "toString".equals(visited.getName().getIdentifier()) // fast-path //$NON-NLS-1$
				&& (ASTNodes.usesGivenSignature(visited, Boolean.class.getCanonicalName(), "toString", boolean.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(visited, Character.class.getCanonicalName(), "toString", char.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(visited, Byte.class.getCanonicalName(), "toString", byte.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(visited, Short.class.getCanonicalName(), "toString", short.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(visited, Integer.class.getCanonicalName(), "toString", int.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(visited, Long.class.getCanonicalName(), "toString", long.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(visited, Float.class.getCanonicalName(), "toString", float.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(visited, Double.class.getCanonicalName(), "toString", double.class.getSimpleName())); //$NON-NLS-1$
	}

	private boolean isStringValueOf(final MethodInvocation visited) {
		return ASTNodes.hasType(visited.getExpression(), String.class.getCanonicalName()) // fast-path
				&& (ASTNodes.usesGivenSignature(visited, String.class.getCanonicalName(), "valueOf", boolean.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(visited, String.class.getCanonicalName(), "valueOf", char.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(visited, String.class.getCanonicalName(), "valueOf", byte.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(visited, String.class.getCanonicalName(), "valueOf", short.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(visited, String.class.getCanonicalName(), "valueOf", int.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(visited, String.class.getCanonicalName(), "valueOf", long.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(visited, String.class.getCanonicalName(), "valueOf", float.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(visited, String.class.getCanonicalName(), "valueOf", double.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(visited, String.class.getCanonicalName(), "valueOf", Object.class.getCanonicalName())); //$NON-NLS-1$
	}

	private boolean maybeReplaceStringValueOfByArg0(final Expression toReplace, final MethodInvocation methodInvocation) {
		ITypeBinding expectedType= methodInvocation.resolveMethodBinding().getParameterTypes()[0];

		if (expectedType == null) {
			return true;
		}

		replaceStringValueOfByArg0(toReplace, methodInvocation, expectedType);
		return false;
	}

	private void replaceStringValueOfByArg0(final Expression toReplace, final MethodInvocation methodInvocation,
			final ITypeBinding expectedType) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.StringCleanUp_description);

		ITypeBinding actualType= ((Expression) methodInvocation.arguments().get(0)).resolveTypeBinding();

		if (expectedType.equals(actualType) || Bindings.getBoxedTypeBinding(expectedType, methodInvocation.getAST()).equals(actualType)) {
			rewrite.replace(toReplace, ASTNodeFactory.parenthesizeIfNeeded(ast, ASTNodes.createMoveTarget(rewrite, (Expression) methodInvocation.arguments().get(0))), group);
		} else {
			rewrite.replace(toReplace, ast.newCastExpression(ast.type(expectedType.getQualifiedName()), ASTNodes.createMoveTarget(rewrite, (Expression) methodInvocation.arguments().get(0))), group);
		}
	}

	private void removeToString(final MethodInvocation visited) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.StringCleanUp_description);

		if (visited.getExpression() != null) {
			rewrite.replace(visited, ASTNodes.createMoveTarget(rewrite, visited.getExpression()), group);
		} else {
			rewrite.replace(visited, ast.newThisExpression(), group);
		}
	}

	private void removeValueOf(final MethodInvocation visited) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.StringCleanUp_description);

		rewrite.replace(visited, ASTNodeFactory.parenthesizeIfNeeded(ast, ASTNodes.createMoveTarget(rewrite, (Expression) visited.arguments().get(0))), group);
	}
}

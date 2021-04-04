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

		ASTRewrite rewrite= cuRewrite.getASTRewrite();

		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.StringCleanUp_description);

		if (ASTNodes.usesGivenSignature(visited, Object.class.getCanonicalName(), "toString")) { //$NON-NLS-1$
			Expression stringExpression= visited.getExpression();

			if (ASTNodes.hasType(stringExpression, String.class.getCanonicalName())) {
				// If node is already a String, no need to call toString()
				ASTNodes.replaceButKeepComment(rewrite, visited, ASTNodes.createMoveTarget(rewrite, stringExpression), group);
				return false;
			}

			if (parent instanceof InfixExpression) {
				// If node is in a String context, no need to call toString()
				InfixExpression infixExpression= (InfixExpression) parent;
				Expression leftOp= infixExpression.getLeftOperand();
				Expression rightOp= infixExpression.getRightOperand();
				boolean leftOpIsString= ASTNodes.hasType(leftOp, String.class.getCanonicalName());
				boolean rightOpIsString= ASTNodes.hasType(rightOp, String.class.getCanonicalName());
				MethodInvocation lmi= ASTNodes.as(leftOp, MethodInvocation.class);
				MethodInvocation rmi= ASTNodes.as(rightOp, MethodInvocation.class);

				if ((leftOpIsString || rightOpIsString)
						&& visited.getLocationInParent() != InfixExpression.LEFT_OPERAND_PROPERTY
						&& visited.getLocationInParent() != InfixExpression.RIGHT_OPERAND_PROPERTY) {
					// Node is in the extended operands
					ASTNodes.replaceButKeepComment(rewrite, visited, replaceToString(visited.getExpression()), group);
					return false;
				}

				if (leftOpIsString && ASTNodes.usesGivenSignature(rmi, Object.class.getCanonicalName(), "toString")) { //$NON-NLS-1$
					ASTNodes.replaceButKeepComment(rewrite, rmi, replaceToString(rmi.getExpression()), group);
					return false;
				}

				if (rightOpIsString && visited.getLocationInParent() == InfixExpression.LEFT_OPERAND_PROPERTY) {
					ASTNodes.replaceButKeepComment(rewrite, lmi, replaceToString(lmi.getExpression()), group);
					return false;
				}
			}
		} else if (isStringValueOf && ASTNodes.hasType((Expression) visited.arguments().get(0), String.class.getCanonicalName())) {
			if ((Expression) visited.arguments().get(0) instanceof StringLiteral || (Expression) visited.arguments().get(0) instanceof InfixExpression) {
				ASTNodes.replaceButKeepComment(rewrite, visited, ASTNodeFactory.parenthesizeIfNeeded(ast, ASTNodes.createMoveTarget(rewrite, (Expression) visited.arguments().get(0))), group);
				return false;
			}
		} else if (parent instanceof InfixExpression && (isStringValueOf || isToStringForPrimitive(visited))) {
			// If node is in a String context, no need to call toString()
			InfixExpression infixExpression= (InfixExpression) parent;
			Expression lo= infixExpression.getLeftOperand();
			Expression ro= infixExpression.getRightOperand();

			if (visited.equals(lo)) {
				if (ASTNodes.hasType(ro, String.class.getCanonicalName())) {
					return maybeReplaceStringValueOfByArg0(lo, visited);
				}
			} else if (visited.equals(ro)) {
				if (ASTNodes.hasType(lo, String.class.getCanonicalName())
						// Do not refactor left and right operand at the same time
						// to avoid compilation errors post cleanup
						&& !rewrite.hasBeenRefactored(lo)) {
					return maybeReplaceStringValueOfByArg0(ro, visited);
				}
			} else {
				// Left or right operation is necessarily a string, so just replace
				return maybeReplaceStringValueOfByArg0(visited, visited);
			}
		}

		return true;
	}

	private boolean maybeReplaceStringValueOfByArg0(final Expression toReplace, final MethodInvocation methodInvocation) {
		ITypeBinding expectedType= methodInvocation.resolveMethodBinding().getParameterTypes()[0];

		if (expectedType == null) {
			return true;
		}

		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.StringCleanUp_description);

		ITypeBinding actualType= ((Expression) methodInvocation.arguments().get(0)).resolveTypeBinding();

		if (expectedType.equals(actualType) || Bindings.getBoxedTypeBinding(expectedType, methodInvocation.getAST()).equals(actualType)) {
			ASTNodes.replaceButKeepComment(rewrite, toReplace, ASTNodeFactory.parenthesizeIfNeeded(ast, ASTNodes.createMoveTarget(rewrite, (Expression) methodInvocation.arguments().get(0))), group);
		} else {
			ASTNodes.replaceButKeepComment(rewrite, toReplace, ast.newCastExpression(ast.type(expectedType.getQualifiedName()), ASTNodes.createMoveTarget(rewrite, (Expression) methodInvocation.arguments().get(0))), group);
		}

		return false;
	}

	private Expression replaceToString(final Expression expression) {
		if (expression != null) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			return ASTNodes.createMoveTarget(rewrite, expression);
		}

		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		return ast.newThisExpression();
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
}

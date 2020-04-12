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

import java.util.concurrent.atomic.AtomicBoolean;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Bindings;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.CharacterLiteral;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.StringLiteral;

/** See {@link #getDescription()} method. */
public class StringCleanUp extends AbstractCleanUpRule {
	/**
	 * Get the name.
	 *
	 * @return the name.
	 */
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_StringCleanUp_name;
	}

	/**
	 * Get the description.
	 *
	 * @return the description.
	 */
	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_StringCleanUp_description;
	}

	/**
	 * Get the reason.
	 *
	 * @return the reason.
	 */
	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_StringCleanUp_reason;
	}

	@Override
	public boolean visit(final MethodInvocation node) {
		ASTNode parent= node.getParent();
		boolean isStringValueOf= isStringValueOf(node);

		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		ASTRewrite rewrite= cuRewrite.getASTRewrite();

		if (ASTNodes.usesGivenSignature(node, Object.class.getCanonicalName(), "toString")) { //$NON-NLS-1$
			Expression stringExpression= node.getExpression();

			if (ASTNodes.hasType(stringExpression, String.class.getCanonicalName())) {
				// If node is already a String, no need to call toString()
				rewrite.replace(node, rewrite.createMoveTarget(stringExpression), null);
				return false;
			}

			if (parent instanceof InfixExpression) {
				// If node is in a String context, no need to call toString()
				InfixExpression ie= (InfixExpression) parent;
				Expression leftOp= ie.getLeftOperand();
				Expression rightOp= ie.getRightOperand();
				boolean leftOpIsString= ASTNodes.hasType(leftOp, String.class.getCanonicalName());
				boolean rightOpIsString= ASTNodes.hasType(rightOp, String.class.getCanonicalName());
				MethodInvocation lmi= ASTNodes.as(leftOp, MethodInvocation.class);
				MethodInvocation rmi= ASTNodes.as(rightOp, MethodInvocation.class);

				if ((leftOpIsString || rightOpIsString)
						&& node.getLocationInParent() != InfixExpression.LEFT_OPERAND_PROPERTY
						&& node.getLocationInParent() != InfixExpression.RIGHT_OPERAND_PROPERTY) {
					// Node is in the extended operands
					rewrite.replace(node, replaceToString(node.getExpression()), null);
					return false;
				}

				if (leftOpIsString && ASTNodes.usesGivenSignature(rmi, Object.class.getCanonicalName(), "toString")) { //$NON-NLS-1$
					rewrite.replace(rmi, replaceToString(rmi.getExpression()), null);
					return false;
				}

				if (rightOpIsString && node.getLocationInParent() == InfixExpression.LEFT_OPERAND_PROPERTY) {
					rewrite.replace(lmi, replaceToString(lmi.getExpression()), null);
					return false;
				}
			}
		} else if (isStringValueOf && ASTNodes.hasType(ASTNodes.arguments(node).get(0), String.class.getCanonicalName())) {
			if (ASTNodes.arguments(node).get(0) instanceof StringLiteral || ASTNodes.arguments(node).get(0) instanceof InfixExpression) {
				rewrite.replace(node, ast.parenthesizeIfNeeded(rewrite.createMoveTarget(ASTNodes.arguments(node).get(0))), null);
				return false;
			}
		} else if (parent instanceof InfixExpression && (isStringValueOf || isToStringForPrimitive(node))) {
			// If node is in a String context, no need to call toString()
			InfixExpression ie= (InfixExpression) parent;
			Expression lo= ie.getLeftOperand();
			Expression ro= ie.getRightOperand();

			if (node.equals(lo)) {
				if (ASTNodes.hasType(ro, String.class.getCanonicalName())) {
					return maybeReplaceStringValueOfByArg0(lo, node);
				}
			} else if (node.equals(ro)) {
				if (ASTNodes.hasType(lo, String.class.getCanonicalName())
						// Do not refactor left and right operand at the same time
						// to avoid compilation errors post cleanup
						&& !rewrite.hasBeenRefactored(lo)) {
					return maybeReplaceStringValueOfByArg0(ro, node);
				}
			} else {
				// Left or right operation is necessarily a string, so just replace
				return maybeReplaceStringValueOfByArg0(node, node);
			}
		} else if (ASTNodes.usesGivenSignature(node, String.class.getCanonicalName(), "equals", Object.class.getCanonicalName())) { //$NON-NLS-1$
			MethodInvocation leftInvocation= ASTNodes.as(node.getExpression(), MethodInvocation.class);
			MethodInvocation rightInvocation= ASTNodes.as(ASTNodes.arguments(node).get(0), MethodInvocation.class);

			if (leftInvocation != null && rightInvocation != null
					&& (ASTNodes.usesGivenSignature(leftInvocation, String.class.getCanonicalName(), "toLowerCase") //$NON-NLS-1$
							&& ASTNodes.usesGivenSignature(rightInvocation, String.class.getCanonicalName(), "toLowerCase") //$NON-NLS-1$
							|| ASTNodes.usesGivenSignature(leftInvocation, String.class.getCanonicalName(), "toUpperCase") //$NON-NLS-1$
									&& ASTNodes.usesGivenSignature(rightInvocation, String.class.getCanonicalName(), "toUpperCase"))) { //$NON-NLS-1$
				Expression leftExpression= leftInvocation.getExpression();
				Expression rightExpression= rightInvocation.getExpression();
				rewrite.replace(node, ast.invoke(rewrite.createMoveTarget(leftExpression), "equalsIgnoreCase", rewrite.createMoveTarget(rightExpression)), null); //$NON-NLS-1$
				return false;
			}
		} else if (ASTNodes.usesGivenSignature(node, String.class.getCanonicalName(), "equalsIgnoreCase", String.class.getCanonicalName())) { //$NON-NLS-1$
			AtomicBoolean isRefactoringNeeded= new AtomicBoolean(false);

			Expression leftExpression= getReducedStringExpression(node.getExpression(), isRefactoringNeeded);
			Expression rightExpression= getReducedStringExpression(ASTNodes.arguments(node).get(0), isRefactoringNeeded);

			if (isRefactoringNeeded.get()) {
				rewrite.replace(node, ast.invoke(rewrite.createMoveTarget(leftExpression), "equalsIgnoreCase", rewrite.createMoveTarget(rightExpression)), null); //$NON-NLS-1$
				return false;
			}
		} else if (ASTNodes.usesGivenSignature(node, String.class.getCanonicalName(), "indexOf", String.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(node, String.class.getCanonicalName(), "lastIndexOf", String.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(node, String.class.getCanonicalName(), "indexOf", String.class.getCanonicalName(), Integer.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(node, String.class.getCanonicalName(), "lastIndexOf", String.class.getCanonicalName(), Integer.class.getCanonicalName())) { //$NON-NLS-1$
			StringLiteral stringLiteral= ASTNodes.as(ASTNodes.arguments(node).get(0), StringLiteral.class);

			if (stringLiteral != null) {
				String value= stringLiteral.getLiteralValue();

				if (value.length() == 1) {
					CharacterLiteral replacement= ast.charLiteral();
					replacement.setCharValue(value.charAt(0));
					rewrite.replace(stringLiteral, replacement, null);
					return false;
				}
			}
		}

		return true;
	}

	private Expression getReducedStringExpression(final Expression stringExpression, final AtomicBoolean isRefactoringNeeded) {
		MethodInvocation casingInvocation= ASTNodes.as(stringExpression, MethodInvocation.class);

		if (casingInvocation != null && (ASTNodes.usesGivenSignature(casingInvocation, String.class.getCanonicalName(), "toLowerCase") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(casingInvocation, String.class.getCanonicalName(), "toUpperCase"))) { //$NON-NLS-1$
			isRefactoringNeeded.set(true);
			return casingInvocation.getExpression();
		}

		return stringExpression;
	}

	private boolean maybeReplaceStringValueOfByArg0(final Expression toReplace, final MethodInvocation mi) {
		ITypeBinding expectedType= mi.resolveMethodBinding().getParameterTypes()[0];

		if (expectedType == null) {
			return true;
		}

		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		ASTRewrite rewrite= cuRewrite.getASTRewrite();

		ITypeBinding actualType= ASTNodes.arguments(mi).get(0).resolveTypeBinding();

		if (expectedType.equals(actualType) || Bindings.getBoxedTypeBinding(expectedType, mi.getAST()).equals(actualType)) {
			cuRewrite.getASTRewrite().replace(toReplace, ast.parenthesizeIfNeeded(rewrite.createMoveTarget(ASTNodes.arguments(mi).get(0))), null);
		} else {
			cuRewrite.getASTRewrite().replace(toReplace, ast.cast(ast.type(expectedType.getQualifiedName()), rewrite.createMoveTarget(ASTNodes.arguments(mi).get(0))), null);
		}

		return false;
	}

	private Expression replaceToString(final Expression expression) {
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		if (expression != null) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			return rewrite.createMoveTarget(expression);
		}

		return ast.this0();
	}

	private boolean isToStringForPrimitive(final MethodInvocation node) {
		return "toString".equals(node.getName().getIdentifier()) // fast-path //$NON-NLS-1$
				&& (ASTNodes.usesGivenSignature(node, Boolean.class.getCanonicalName(), "toString", boolean.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, Character.class.getCanonicalName(), "toString", char.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, Byte.class.getCanonicalName(), "toString", byte.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, Short.class.getCanonicalName(), "toString", short.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, Integer.class.getCanonicalName(), "toString", int.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, Long.class.getCanonicalName(), "toString", long.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, Float.class.getCanonicalName(), "toString", float.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, Double.class.getCanonicalName(), "toString", double.class.getSimpleName())); //$NON-NLS-1$
	}

	private boolean isStringValueOf(final MethodInvocation node) {
		return ASTNodes.hasType(node.getExpression(), String.class.getCanonicalName()) // fast-path
				&& (ASTNodes.usesGivenSignature(node, String.class.getCanonicalName(), "valueOf", boolean.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, String.class.getCanonicalName(), "valueOf", char.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, String.class.getCanonicalName(), "valueOf", byte.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, String.class.getCanonicalName(), "valueOf", short.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, String.class.getCanonicalName(), "valueOf", int.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, String.class.getCanonicalName(), "valueOf", long.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, String.class.getCanonicalName(), "valueOf", float.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, String.class.getCanonicalName(), "valueOf", double.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, String.class.getCanonicalName(), "valueOf", Object.class.getCanonicalName())); //$NON-NLS-1$
	}
}

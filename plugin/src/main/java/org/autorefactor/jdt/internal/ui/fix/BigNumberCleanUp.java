/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2018 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Make sure we do not visit again modified nodes
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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.JavaConstants;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.StringLiteral;

/** See {@link #getDescription()} method. */
public class BigNumberCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_BigNumberCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_BigNumberCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_BigNumberCleanUp_reason;
	}

	@Override
	public boolean visit(final ClassInstanceCreation node) {
		ITypeBinding typeBinding= node.getType().resolveBinding();

		if (node.getAnonymousClassDeclaration() == null
				&& ASTNodes.hasType(typeBinding, BigDecimal.class.getCanonicalName(), BigInteger.class.getCanonicalName())
				&& ((List<Expression>) node.arguments()).size() == 1) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();

			Expression arg0= ((List<Expression>) node.arguments()).get(0);

			if (arg0 instanceof NumberLiteral && ASTNodes.hasType(typeBinding, BigDecimal.class.getCanonicalName())) {
				String token= ((NumberLiteral) arg0).getToken().replaceFirst("[lLfFdD]$", "").replace("_", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

				if (token.contains(".")) { //$NON-NLS-1$
					// Only instantiation from double, not from integer
					rewrite.replace(arg0, getStringLiteral(token), null);
					return false;
				}

				if (getJavaMinorVersion() >= 5) {
					if (JavaConstants.ZERO_LONG_LITERAL_RE.matcher(token).matches()) {
						return replaceWithQualifiedName(node, typeBinding, "ZERO"); //$NON-NLS-1$
					}

					if (JavaConstants.ONE_LONG_LITERAL_RE.matcher(token).matches()) {
						return replaceWithQualifiedName(node, typeBinding, "ONE"); //$NON-NLS-1$
					}

					if (JavaConstants.TEN_LONG_LITERAL_RE.matcher(token).matches()) {
						return replaceWithQualifiedName(node, typeBinding, "TEN"); //$NON-NLS-1$
					}

					rewrite.replace(node, getValueOf(typeBinding.getName(), token), null);
					return false;
				}
			} else if (arg0 instanceof StringLiteral) {
				if (getJavaMinorVersion() < 5) {
					return true;
				}

				String literalValue= ((StringLiteral) arg0).getLiteralValue().replaceFirst("[lLfFdD]$", ""); //$NON-NLS-1$ //$NON-NLS-2$

				if (literalValue.contains(".") && literalValue.contains("_")) { //$NON-NLS-1$ //$NON-NLS-2$
					// Only instantiation from double, not from integer
					rewrite.replace(arg0, getStringLiteral(literalValue.replace("_", "")), null); //$NON-NLS-1$ //$NON-NLS-2$
					return false;
				}

				if (literalValue.matches("0+")) { //$NON-NLS-1$
					return replaceWithQualifiedName(node, typeBinding, "ZERO"); //$NON-NLS-1$
				}

				if (literalValue.matches("0+1")) { //$NON-NLS-1$
					return replaceWithQualifiedName(node, typeBinding, "ONE"); //$NON-NLS-1$
				}

				if (literalValue.matches("0+10")) { //$NON-NLS-1$
					return replaceWithQualifiedName(node, typeBinding, "TEN"); //$NON-NLS-1$
				}

				if (literalValue.matches("\\d+")) { //$NON-NLS-1$
					rewrite.replace(node, getValueOf(typeBinding.getName(), literalValue), null);
					return false;
				}
			}
		}

		return true;
	}

	private boolean replaceWithQualifiedName(final ASTNode node, final ITypeBinding typeBinding, final String field) {
		cuRewrite.getASTRewrite().replace(node, cuRewrite.getASTBuilder().name(typeBinding.getName(), field), null);
		return false;
	}

	private ASTNode getValueOf(final String name, final String numberLiteral) {
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		return ast.newMethodInvocation(name, "valueOf", ast.number(numberLiteral)); //$NON-NLS-1$
	}

	private StringLiteral getStringLiteral(final String numberLiteral) {
		return cuRewrite.getASTBuilder().string(numberLiteral);
	}

	@Override
	public boolean visit(final PrefixExpression node) {
		MethodInvocation methodInvocation= ASTNodes.as(node.getOperand(), MethodInvocation.class);
		return !ASTNodes.hasOperator(node, PrefixExpression.Operator.NOT) || methodInvocation == null || maybeReplaceEquals(false, node, methodInvocation);
	}

	@Override
	public boolean visit(final MethodInvocation node) {
		if (node.getExpression() == null) {
			return true;
		}

		if (getJavaMinorVersion() >= 5 && (ASTNodes.usesGivenSignature(node, BigInteger.class.getCanonicalName(), "valueOf", long.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(node, BigDecimal.class.getCanonicalName(), "valueOf", long.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(node, BigDecimal.class.getCanonicalName(), "valueOf", double.class.getSimpleName()))) { //$NON-NLS-1$
			ITypeBinding typeBinding= node.getExpression().resolveTypeBinding();
			Expression arg0= ((List<Expression>) node.arguments()).get(0);

			if (arg0 instanceof NumberLiteral) {
				String token= ((NumberLiteral) arg0).getToken().replaceFirst("[lLfFdD]$", ""); //$NON-NLS-1$ //$NON-NLS-2$

				if (token.contains(".") && ASTNodes.hasType(typeBinding, BigDecimal.class.getCanonicalName())) { //$NON-NLS-1$
					cuRewrite.getASTRewrite().replace(node,
							getClassInstanceCreatorNode(node.getExpression(), token), null);
				} else if (JavaConstants.ZERO_LONG_LITERAL_RE.matcher(token).matches()) {
					replaceWithQualifiedName(node, typeBinding, "ZERO"); //$NON-NLS-1$
				} else if (JavaConstants.ONE_LONG_LITERAL_RE.matcher(token).matches()) {
					replaceWithQualifiedName(node, typeBinding, "ONE"); //$NON-NLS-1$
				} else if (JavaConstants.TEN_LONG_LITERAL_RE.matcher(token).matches()) {
					replaceWithQualifiedName(node, typeBinding, "TEN"); //$NON-NLS-1$
				} else {
					return true;
				}

				return false;
			}
		} else if (!(node.getParent() instanceof PrefixExpression)
				|| !ASTNodes.hasOperator((PrefixExpression) node.getParent(), PrefixExpression.Operator.NOT)) {
			return maybeReplaceEquals(true, node, node);
		}

		return true;
	}

	private boolean maybeReplaceEquals(final boolean isPositive, final Expression node, final MethodInvocation methodInvocation) {
		if (ASTNodes.usesGivenSignature(methodInvocation, BigDecimal.class.getCanonicalName(), "equals", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, BigInteger.class.getCanonicalName(), "equals", Object.class.getCanonicalName())) { //$NON-NLS-1$
			Expression arg0= ((List<Expression>) methodInvocation.arguments()).get(0);

			if (ASTNodes.hasType(arg0, BigDecimal.class.getCanonicalName(), BigInteger.class.getCanonicalName())) {
				ASTRewrite rewrite= cuRewrite.getASTRewrite();

				if (isInStringAppend(methodInvocation.getParent())) {
					ASTNodeFactory ast= cuRewrite.getASTBuilder();

					rewrite.replace(node, ast.parenthesize(getCompareToNode(isPositive, methodInvocation)), null);
				} else {
					rewrite.replace(node, getCompareToNode(isPositive, methodInvocation), null);
				}

				return false;
			}
		}

		return true;
	}

	private boolean isInStringAppend(final ASTNode node) {
		if (node instanceof InfixExpression) {
			InfixExpression expression= (InfixExpression) node;

			if (ASTNodes.hasOperator(expression, InfixExpression.Operator.PLUS) || ASTNodes.hasType(expression.getLeftOperand(), String.class.getCanonicalName())
					|| ASTNodes.hasType(expression.getRightOperand(), String.class.getCanonicalName())) {
				return true;
			}
		}

		return false;
	}

	private ASTNode getClassInstanceCreatorNode(final Expression expression, final String numberLiteral) {
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		String fullyQualifiedName;
		if (expression instanceof Name) {
			fullyQualifiedName= ((Name) expression).getFullyQualifiedName();
		} else if (expression instanceof FieldAccess) {
			fullyQualifiedName= ((FieldAccess) expression).getName().getFullyQualifiedName();
		} else {
			throw new IllegalArgumentException();
		}

		return ast.new0(fullyQualifiedName, ast.string(numberLiteral));
	}

	private InfixExpression getCompareToNode(final boolean isPositive, final MethodInvocation node) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		MethodInvocation methodInvocation= ast.newMethodInvocation(ASTNodes.createMoveTarget(rewrite, node.getExpression()), "compareTo", ASTNodes.createMoveTarget(rewrite, ((List<Expression>) node.arguments()).get(0))); //$NON-NLS-1$

		return ast.infixExpression(methodInvocation, isPositive ? InfixExpression.Operator.EQUALS : InfixExpression.Operator.NOT_EQUALS, ast.int0(0));
	}
}

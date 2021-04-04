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

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.JavaConstants;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.text.edits.TextEditGroup;

/**
 * Some actions of this rule follow the Sonar recommendations:
 * https://rules.sonarsource.com/java/RSPEC-2111
 */
public class BigNumberCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.BigNumberCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.BigNumberCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.BigNumberCleanUp_reason;
	}

	@Override
	public boolean visit(final ClassInstanceCreation visited) {
		ITypeBinding typeBinding= visited.getType().resolveBinding();

		if (visited.getAnonymousClassDeclaration() == null
				&& ASTNodes.hasType(typeBinding, BigDecimal.class.getCanonicalName(),
						BigInteger.class.getCanonicalName())
				&& visited.arguments().size() == 1) {
			Expression arg0= (Expression) visited.arguments().get(0);

			if (arg0 instanceof NumberLiteral && ASTNodes.hasType(typeBinding, BigDecimal.class.getCanonicalName())) {
				String token= ((NumberLiteral) arg0).getToken().replaceFirst("[lLfFdD]$", "").replace("_", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

				if (token.contains(".")) { //$NON-NLS-1$
					replaceWithNumberText(arg0, token);
					return false;
				}

				if (getJavaMinorVersion() >= 5) {
					if (JavaConstants.ZERO_LONG_LITERAL_RE.matcher(token).matches()) {
						replaceWithQualifiedName(visited, typeBinding, "ZERO"); //$NON-NLS-1$
						return false;
					}

					if (JavaConstants.ONE_LONG_LITERAL_RE.matcher(token).matches()) {
						replaceWithQualifiedName(visited, typeBinding, "ONE"); //$NON-NLS-1$
						return false;
					}

					if (JavaConstants.TEN_LONG_LITERAL_RE.matcher(token).matches()) {
						replaceWithQualifiedName(visited, typeBinding, "TEN"); //$NON-NLS-1$
						return false;
					}

					replaceByValueOf(visited, typeBinding, token);
					return false;
				}
			} else if (arg0 instanceof StringLiteral && getJavaMinorVersion() >= 5) {
				String literalValue= ((StringLiteral) arg0).getLiteralValue().replaceFirst("[lLfFdD]$", ""); //$NON-NLS-1$ //$NON-NLS-2$

				if (literalValue.matches("0+")) { //$NON-NLS-1$
					replaceWithQualifiedName(visited, typeBinding, "ZERO"); //$NON-NLS-1$
					return false;
				}

				if (literalValue.matches("0*1")) { //$NON-NLS-1$
					replaceWithQualifiedName(visited, typeBinding, "ONE"); //$NON-NLS-1$
					return false;
				}

				if (literalValue.matches("0*10")) { //$NON-NLS-1$
					replaceWithQualifiedName(visited, typeBinding, "TEN"); //$NON-NLS-1$
					return false;
				}
			}
		}

		return true;
	}

	private StringLiteral getStringLiteral(final String numberLiteral) {
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		return ast.newStringLiteral(numberLiteral);
	}

	@Override
	public boolean visit(final MethodInvocation visited) {
		if (visited.getExpression() == null) {
			return true;
		}

		if (getJavaMinorVersion() >= 5 && (ASTNodes.usesGivenSignature(visited, BigInteger.class.getCanonicalName(),
				"valueOf", long.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(visited, BigDecimal.class.getCanonicalName(), "valueOf", //$NON-NLS-1$
						long.class.getSimpleName())
				|| ASTNodes.usesGivenSignature(visited, BigDecimal.class.getCanonicalName(), "valueOf", //$NON-NLS-1$
						double.class.getSimpleName()))) {
			ITypeBinding typeBinding= visited.getExpression().resolveTypeBinding();
			Expression arg0= (Expression) visited.arguments().get(0);

			if (arg0 instanceof NumberLiteral) {
				String token= ((NumberLiteral) arg0).getToken().replaceFirst("[lLfFdD]$", "").replace("_", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

				if (token.contains(".") && ASTNodes.hasType(typeBinding, BigDecimal.class.getCanonicalName())) { //$NON-NLS-1$
					TextEditGroup group= new TextEditGroup(MultiFixMessages.BigNumberCleanUp_description);
					ASTRewrite rewrite= cuRewrite.getASTRewrite();

					rewrite.replace(visited, getClassInstanceCreatorNode(visited.getExpression(), token), group);
				} else if (JavaConstants.ZERO_LONG_LITERAL_RE.matcher(token).matches()) {
					replaceWithQualifiedName(visited, typeBinding, "ZERO"); //$NON-NLS-1$
				} else if (JavaConstants.ONE_LONG_LITERAL_RE.matcher(token).matches()) {
					replaceWithQualifiedName(visited, typeBinding, "ONE"); //$NON-NLS-1$
				} else if (JavaConstants.TEN_LONG_LITERAL_RE.matcher(token).matches()) {
					replaceWithQualifiedName(visited, typeBinding, "TEN"); //$NON-NLS-1$
				} else {
					return true;
				}

				return false;
			}
		}

		return true;
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

		return ast.newClassInstanceCreation(fullyQualifiedName, ast.newStringLiteral(numberLiteral));
	}

	private void replaceWithNumberText(Expression arg0, String numberText) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.BigNumberCleanUp_description);

		// Only instantiation from double, not from integer
		rewrite.replace(arg0, getStringLiteral(numberText), group);
	}

	private void replaceByValueOf(final ClassInstanceCreation visited, final ITypeBinding typeBinding,
			final String numberText) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.BigNumberCleanUp_description);

		MethodInvocation valueOf= ast.newMethodInvocation();
		valueOf.setExpression(ASTNodeFactory.newName(ast, typeBinding.getName()));
		valueOf.setName(ast.newSimpleName("valueOf")); //$NON-NLS-1$
		valueOf.arguments().add(ast.newNumberLiteral(numberText));

		rewrite.replace(visited, valueOf, group);
	}

	private void replaceWithQualifiedName(final ASTNode visited, final ITypeBinding typeBinding, final String field) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.BigNumberCleanUp_description);

		ASTNodes.replaceButKeepComment(rewrite, visited, ASTNodeFactory.newName(ast, typeBinding.getName(), field),
				group);
	}
}

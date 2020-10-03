/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Separate the code.
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
import org.eclipse.jdt.core.dom.BooleanLiteral;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class BooleanConstantRatherThanValueOfCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.BooleanConstantRatherThanValueOfCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.BooleanConstantRatherThanValueOfCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.BooleanConstantRatherThanValueOfCleanUp_reason;
	}

	@Override
	public boolean visit(final MethodInvocation node) {
		if (ASTNodes.usesGivenSignature(node, Boolean.class.getCanonicalName(), "valueOf", boolean.class.getSimpleName())) { //$NON-NLS-1$
			BooleanLiteral literal= ASTNodes.as((Expression) node.arguments().get(0), BooleanLiteral.class);

			if (literal != null) {
				replaceMethod(node, literal.booleanValue());
				return false;
			}
		} else if (ASTNodes.usesGivenSignature(node, Boolean.class.getCanonicalName(), "valueOf", String.class.getCanonicalName())) { //$NON-NLS-1$
			StringLiteral literal= ASTNodes.as((Expression) node.arguments().get(0), StringLiteral.class);

			if (literal != null) {
				if ("true".equals(literal.getLiteralValue())) { //$NON-NLS-1$
					replaceMethod(node, true);
					return false;
				}

				if ("false".equals(literal.getLiteralValue())) { //$NON-NLS-1$
					replaceMethod(node, false);
					return false;
				}
			}
		}

		return true;
	}

	private void replaceMethod(final MethodInvocation node, final boolean literal) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.BooleanConstantRatherThanValueOfCleanUp_description);

		FieldAccess fieldAccess= ast.getAST().newFieldAccess();
		Name expression= ASTNodes.as(node.getExpression(), Name.class);

		if (expression != null) {
			fieldAccess.setExpression(ASTNodes.createMoveTarget(rewrite, expression));
		}

		fieldAccess.setName(ast.newSimpleName(literal ? "TRUE" : "FALSE")); //$NON-NLS-1$ //$NON-NLS-2$
		ASTNodes.replaceButKeepComment(rewrite, node, fieldAccess, group);
	}
}

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

import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class UpdateSetRatherThanTestingFirstCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.UpdateSetRatherThanTestingFirstCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.UpdateSetRatherThanTestingFirstCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.UpdateSetRatherThanTestingFirstCleanUp_reason;
	}

	@Override
	public boolean visit(final IfStatement visited) {
		Statement elseStatement= visited.getElseStatement();
		Statement thenStatement= visited.getThenStatement();
		PrefixExpression prefixExpression= ASTNodes.as(visited.getExpression(), PrefixExpression.class);

		if (ASTNodes.hasOperator(prefixExpression, PrefixExpression.Operator.NOT)) {
			return maybeReplaceSetContains(visited, prefixExpression.getOperand(), thenStatement, elseStatement, false);
		}

		return maybeReplaceSetContains(visited, visited.getExpression(), elseStatement, thenStatement, true);
	}

	private boolean maybeReplaceSetContains(final IfStatement ifStmtToReplace, final Expression ifExpression,
			final Statement statement, final Statement oppositeStatement, final boolean negate) {
		return maybeReplaceSetContains(ifStmtToReplace, ifExpression, statement, oppositeStatement, negate, "add") //$NON-NLS-1$
				&& maybeReplaceSetContains(ifStmtToReplace, ifExpression, oppositeStatement, statement, !negate, "remove"); //$NON-NLS-1$
	}

	private boolean maybeReplaceSetContains(final IfStatement ifStmtToReplace, final Expression ifExpression,
			final Statement statement, final Statement oppositeStatement, final boolean negate, final String methodName) {
		List<Statement> statements= ASTNodes.asList(statement);
		MethodInvocation miContains= ASTNodes.as(ifExpression, MethodInvocation.class);

		if (!statements.isEmpty() && ASTNodes.usesGivenSignature(miContains, Set.class.getCanonicalName(), "contains", Object.class.getCanonicalName())) { //$NON-NLS-1$
			Statement firstStatement= statements.get(0);
			MethodInvocation miAddOrRemove= ASTNodes.asExpression(firstStatement, MethodInvocation.class);

			if (ASTNodes.usesGivenSignature(miAddOrRemove, Set.class.getCanonicalName(), methodName, Object.class.getCanonicalName())
					&& ASTNodes.match(miContains.getExpression(), miAddOrRemove.getExpression())
					&& ASTNodes.match(((List<Expression>) miContains.arguments()).get(0), ((List<Expression>) miAddOrRemove.arguments()).get(0))) {
				ASTRewrite rewrite= cuRewrite.getASTRewrite();
				ASTNodeFactory ast= cuRewrite.getASTBuilder();
				TextEditGroup group= new TextEditGroup(MultiFixMessages.UpdateSetRatherThanTestingFirstCleanUp_description);

				if (statements.size() == 1 && ASTNodes.asList(oppositeStatement).isEmpty()) {
					// Only one statement: replace if statement with col.add() (or col.remove())
					ASTNodes.replaceButKeepComment(rewrite, ifStmtToReplace, ASTNodes.createMoveTarget(rewrite, firstStatement), group);
				} else {
					// There are other statements, replace the if condition with col.add() (or
					// col.remove())
					rewrite.replace(ifStmtToReplace.getExpression(), negate ? ast.negate(miAddOrRemove, true) : ASTNodes.createMoveTarget(rewrite, miAddOrRemove), group);
					rewrite.remove(firstStatement, group);
				}

				return false;
			}
		}

		return true;
	}
}

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

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.ASTSemanticMatcher;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class ElseRatherThanNegatedConditionCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.ElseRatherThanNegatedConditionCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ElseRatherThanNegatedConditionCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ElseRatherThanNegatedConditionCleanUp_reason;
	}

	@Override
	public boolean visit(final IfStatement visited) {
		IfStatement secondIf= ASTNodes.as(visited.getElseStatement(), IfStatement.class);

		if (secondIf != null
				&& (secondIf.getElseStatement() == null || !ASTNodes.isExceptionExpected(visited))
				&& (!ASTNodes.fallsThrough(visited.getThenStatement()) || !ASTNodes.fallsThrough(secondIf.getThenStatement()) || ASTNodes.getNextStatement(visited) == null)
				&& ASTNodes.isPassive(visited.getExpression())
				&& ASTNodes.isPassive(secondIf.getExpression())
				&& ASTSemanticMatcher.INSTANCE.matchNegative(visited.getExpression(), secondIf.getExpression())) {
			removeCondition(secondIf);
			return false;
		}

		return true;
	}

	private void removeCondition(final IfStatement secondIf) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ElseRatherThanNegatedConditionCleanUp_description);

		ASTNodes.replaceButKeepComment(rewrite, secondIf, ASTNodes.createMoveTarget(rewrite, secondIf.getThenStatement()), group);
	}
}

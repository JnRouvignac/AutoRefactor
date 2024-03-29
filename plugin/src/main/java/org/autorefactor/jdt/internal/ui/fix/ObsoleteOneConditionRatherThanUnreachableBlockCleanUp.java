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
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class ObsoleteOneConditionRatherThanUnreachableBlockCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.ObsoleteOneConditionRatherThanUnreachableBlockCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ObsoleteOneConditionRatherThanUnreachableBlockCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ObsoleteOneConditionRatherThanUnreachableBlockCleanUp_reason;
	}

	@Override
	public boolean visit(final IfStatement visited) {
		IfStatement secondIf= ASTNodes.as(visited.getElseStatement(), IfStatement.class);

		if (secondIf != null
				&& !ASTNodes.isExceptionExpected(visited)
				&& (secondIf.getElseStatement() == null || !ASTNodes.fallsThrough(visited.getThenStatement()) || ASTNodes.fallsThrough(secondIf.getThenStatement()) || !ASTNodes.fallsThrough(secondIf.getElseStatement()) || ASTNodes.getNextStatement(visited) == null)
				&& ASTNodes.isPassive(visited.getExpression())
				&& ASTNodes.isPassive(secondIf.getExpression())
				&& ASTNodes.match(visited.getExpression(), secondIf.getExpression())) {
			refactorCondition(secondIf);

			return false;
		}

		return true;
	}

	private void refactorCondition(final IfStatement secondIf) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteOneConditionRatherThanUnreachableBlockCleanUp_description);

		if (secondIf.getElseStatement() == null) {
			rewrite.remove(secondIf, group);
		} else {
			ASTNodes.replaceButKeepComment(rewrite, secondIf, ASTNodes.createMoveTarget(rewrite, secondIf.getElseStatement()), group);
		}
	}
}

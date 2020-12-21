/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016-2017 Jean-Noël Rouvignac - initial API and implementation
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
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class OneTryRatherThanTwoCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.OneTryRatherThanTwoCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.OneTryRatherThanTwoCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.OneTryRatherThanTwoCleanUp_reason;
	}

	@Override
	public boolean isJavaVersionSupported(final Release javaSeRelease) {
		return javaSeRelease.getMinorVersion() >= 7;
	}

	@Override
	public boolean visit(final TryStatement visited) {
		List<Statement> tryStatements= ASTNodes.asList(visited.getBody());

		if (!tryStatements.isEmpty()) {
			TryStatement innerTryStatement= ASTNodes.as(tryStatements.get(0), TryStatement.class);

			if (innerTryStatement != null
					&& !innerTryStatement.resources().isEmpty()
					&& innerTryStatement.getFinally() == null
					&& innerTryStatement.catchClauses().isEmpty()) {
				collapseTryStatements(visited, innerTryStatement);
				return false;
			}
		}

		return true;
	}

	@SuppressWarnings("deprecation")
	private void collapseTryStatements(final TryStatement visited, final TryStatement innerTryStatement) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.OneTryRatherThanTwoCleanUp_description);

		rewrite.insertLast(visited, TryStatement.RESOURCES_PROPERTY, ast.copyRange((List<VariableDeclarationExpression>) innerTryStatement.resources()), group);
		List<Statement> innerStatements= ASTNodes.asList(innerTryStatement.getBody());

		if (innerStatements == null || innerStatements.isEmpty()) {
			rewrite.removeButKeepComment(innerTryStatement, group);
		} else {
			ASTNodes.replaceButKeepComment(rewrite, innerTryStatement, ASTNodes.createMoveTarget(rewrite, innerStatements.get(0)), group);

			for (int i= innerStatements.size() - 1; i > 1; i--) {
				rewrite.insertAfter(ASTNodes.createMoveTarget(rewrite, innerStatements.get(i)), innerTryStatement, group);
			}
		}
	}
}

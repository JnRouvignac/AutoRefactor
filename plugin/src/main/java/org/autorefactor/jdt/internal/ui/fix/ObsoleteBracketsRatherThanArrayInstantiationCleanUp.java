/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice Tiercelin - initial API and implementation
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
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ArrayCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class ObsoleteBracketsRatherThanArrayInstantiationCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.ObsoleteBracketsRatherThanArrayInstantiationCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ObsoleteBracketsRatherThanArrayInstantiationCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ObsoleteBracketsRatherThanArrayInstantiationCleanUp_reason;
	}

	@Override
	public boolean visit(final ArrayCreation visited) {
		if (visited.getInitializer() != null || isVoid(visited)) {
			ITypeBinding arrayType= visited.resolveTypeBinding();
			ITypeBinding destinationType= ASTNodes.getTargetType(visited);

			if (Utils.equalNotNull(arrayType, destinationType) && isDestinationAllowed(visited)) {
				refactorWithInitializer(visited);
				return false;
			}
		}

		return true;
	}

	private boolean isVoid(final ArrayCreation visited) {
		List<Expression> dimensions= visited.dimensions();

		for (Expression dimension : dimensions) {
			if (!Long.valueOf(0L).equals(ASTNodes.getIntegerLiteral(dimension))) {
				return false;
			}
		}

		return true;
	}

	private boolean isDestinationAllowed(final ASTNode visited) {
		int parentType= visited.getParent().getNodeType();

		return parentType == ASTNode.FIELD_DECLARATION
				|| parentType == ASTNode.VARIABLE_DECLARATION_EXPRESSION
				|| parentType == ASTNode.VARIABLE_DECLARATION_FRAGMENT
				|| parentType == ASTNode.VARIABLE_DECLARATION_STATEMENT;
	}

	private void refactorWithInitializer(final ArrayCreation visited) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteBracketsRatherThanArrayInstantiationCleanUp_description);

		if (visited.getInitializer() != null) {
			ASTNodes.replaceButKeepComment(rewrite, visited, ASTNodes.createMoveTarget(rewrite, visited.getInitializer()), group);
		} else {
			ASTNodes.replaceButKeepComment(rewrite, visited, ast.newArrayInitializer(), group);
		}
	}
}

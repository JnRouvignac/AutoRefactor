/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017-2018 Fabrice Tiercelin - initial API and implementation
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
import org.autorefactor.jdt.internal.corext.dom.VarDefinitionsUsesVisitor;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class ObsoleteRemoveOverriddenAssignmentCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.ObsoleteRemoveOverriddenAssignmentCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ObsoleteRemoveOverriddenAssignmentCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ObsoleteRemoveOverriddenAssignmentCleanUp_reason;
	}

	@Override
	public boolean visit(final VariableDeclarationStatement visited) {
		VariableDeclarationFragment fragment= ASTNodes.getUniqueFragment(visited);

		if (fragment != null
				&& fragment.getInitializer() != null
				&& ASTNodes.isPassiveWithoutFallingThrough(fragment.getInitializer())) {
			SimpleName varName= fragment.getName();
			IVariableBinding variable= fragment.resolveBinding();
			Statement stmtToInspect= ASTNodes.getNextSibling(visited);
			boolean isOverridden= false;

			while (stmtToInspect != null) {
				if (!new VarDefinitionsUsesVisitor(variable, stmtToInspect, true).getReads().isEmpty()) {
					return true;
				}

				Assignment assignment= ASTNodes.asExpression(stmtToInspect, Assignment.class);

				if (assignment != null && ASTNodes.isSameVariable(varName, assignment.getLeftHandSide())) {
					if (!ASTNodes.hasOperator(assignment, Assignment.Operator.ASSIGN)) {
						return true;
					}

					isOverridden= true;
					break;
				}

				stmtToInspect= ASTNodes.getNextSibling(stmtToInspect);
			}

			if (isOverridden) {
				TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteRemoveOverriddenAssignmentCleanUp_description);
				ASTRewrite rewrite= cuRewrite.getASTRewrite();

				rewrite.remove(fragment.getInitializer(), group);
				return false;
			}
		}

		return true;
	}
}

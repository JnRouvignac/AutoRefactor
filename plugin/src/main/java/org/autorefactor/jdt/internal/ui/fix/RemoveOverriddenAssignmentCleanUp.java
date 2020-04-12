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

import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.VarDefinitionsUsesVisitor;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/** See {@link #getDescription()} method. */
public class RemoveOverriddenAssignmentCleanUp extends AbstractCleanUpRule {
	/**
	 * Get the name.
	 *
	 * @return the name.
	 */
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_RemoveOverriddenAssignmentCleanUp_name;
	}

	/**
	 * Get the description.
	 *
	 * @return the description.
	 */
	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_RemoveOverriddenAssignmentCleanUp_description;
	}

	/**
	 * Get the reason.
	 *
	 * @return the reason.
	 */
	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_RemoveOverriddenAssignmentCleanUp_reason;
	}

	@Override
	public boolean visit(final VariableDeclarationStatement node) {
		if (node.fragments() != null && node.fragments().size() == 1) {
			VariableDeclarationFragment fragment= (VariableDeclarationFragment) node.fragments().get(0);

			if (fragment.getInitializer() != null && ASTNodes.isPassiveWithoutFallingThrough(fragment.getInitializer())) {
				SimpleName varName= fragment.getName();
				IVariableBinding variable= fragment.resolveBinding();
				Statement stmtToInspect= ASTNodes.getNextSibling(node);
				boolean isOverridden= false;
				boolean isRead= false;

				while (stmtToInspect != null && !isOverridden && !isRead) {
					Assignment assignment= ASTNodes.asExpression(stmtToInspect, Assignment.class);

					if (assignment != null && ASTNodes.isSameVariable(varName, assignment.getLeftHandSide())) {
						if (ASTNodes.hasOperator(assignment, Assignment.Operator.ASSIGN)) {
							isOverridden= true;
						} else {
							isRead= true;
						}
					}

					isRead|= !new VarDefinitionsUsesVisitor(variable, stmtToInspect, true).find().getReads().isEmpty();
					stmtToInspect= ASTNodes.getNextSibling(stmtToInspect);
				}

				if (isOverridden && !isRead) {
					cuRewrite.getASTRewrite().remove(fragment.getInitializer(), null);
					return false;
				}
			}
		}

		return true;
	}
}

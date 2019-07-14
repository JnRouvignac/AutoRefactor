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

import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.VISIT_SUBTREE;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.asExpression;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.getNextSibling;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.hasOperator;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.isPassive;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.isSameVariable;
import static org.eclipse.jdt.core.dom.Assignment.Operator.ASSIGN;

import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/** See {@link #getDescription()} method. */
public class RemoveOverridenAssignmentCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_RemoveOverridenAssignmentCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_RemoveOverridenAssignmentCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_RemoveOverridenAssignmentCleanUp_reason;
    }

    @Override
    public boolean visit(final VariableDeclarationStatement node) {
        if (node.fragments() != null && node.fragments().size() == 1) {
            final VariableDeclarationFragment fragment= (VariableDeclarationFragment) node.fragments().get(0);

            if (fragment.getInitializer() != null && isPassive(fragment.getInitializer())) {
                final SimpleName varName= fragment.getName();
                final IVariableBinding variable= fragment.resolveBinding();
                Statement stmtToInspect= getNextSibling(node);
                boolean isOverridden= false;
                boolean isRead= false;

                while (stmtToInspect != null && !isOverridden && !isRead) {
                    final Assignment assignment= asExpression(stmtToInspect, Assignment.class);
                    if (assignment != null && isSameVariable(varName, assignment.getLeftHandSide())) {
                        if (hasOperator(assignment, ASSIGN)) {
                            isOverridden= true;
                        } else {
                            isRead= true;
                        }
                    }

                    isRead|= !new VariableDefinitionsUsesVisitor(variable, stmtToInspect).find().getUses().isEmpty();

                    stmtToInspect= getNextSibling(stmtToInspect);
                }

                if (isOverridden && !isRead) {
                    ctx.getRefactorings().remove(fragment.getInitializer());
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
        }
        return VISIT_SUBTREE;
    }
}

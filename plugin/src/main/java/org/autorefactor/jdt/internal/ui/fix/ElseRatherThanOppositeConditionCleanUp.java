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

import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.as;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.fallsThrough;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.isExceptionExpected;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.isPassive;

import org.autorefactor.jdt.internal.corext.dom.ASTBuilder;
import org.autorefactor.jdt.internal.corext.dom.ASTSemanticMatcher;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.eclipse.jdt.core.dom.IfStatement;

/** See {@link #getDescription()} method. */
public class ElseRatherThanOppositeConditionCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_ElseRatherThanOppositeConditionCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_ElseRatherThanOppositeConditionCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_ElseRatherThanOppositeConditionCleanUp_reason;
    }

    @Override
    public boolean visit(final IfStatement node) {
        final IfStatement secondIf= as(node.getElseStatement(), IfStatement.class);
        final ASTSemanticMatcher matcher= new ASTSemanticMatcher();

        if (secondIf != null && isPassive(node.getExpression()) && isPassive(secondIf.getExpression())
                && matcher.matchOpposite(node.getExpression(), secondIf.getExpression())
                && (secondIf.getElseStatement() == null || !isExceptionExpected(node))
                && (!fallsThrough(node.getThenStatement()) || !fallsThrough(secondIf.getThenStatement()))) {
            removeCondition(secondIf);

            return false;
        }

        return true;
    }

    private void removeCondition(final IfStatement secondIf) {
        final ASTBuilder b= this.ctx.getASTBuilder();
        final Refactorings r= this.ctx.getRefactorings();

        r.replace(secondIf, b.copy(secondIf.getThenStatement()));
    }
}

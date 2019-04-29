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
package org.autorefactor.refactoring.rules;

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.as;
import static org.autorefactor.refactoring.ASTHelper.getFirstAncestorOrNull;
import static org.autorefactor.refactoring.ASTHelper.isPassive;
import static org.autorefactor.refactoring.ASTHelper.fallsThrough;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.ASTSemanticMatcher;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.BodyDeclaration;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.TryStatement;

/** See {@link #getDescription()} method. */
public class ElseRatherThanOppositeConditionRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Else rather than opposite condition";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
                + "Remove a condition on an else that is opposite to the condition of the previous if."
                + " Beware! It may change the behavior if the code is transpiled in JavaScript for NaN values.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves readibility.";
    }

    @Override
    public boolean visit(final IfStatement node) {
        final IfStatement secondIf = as(node.getElseStatement(), IfStatement.class);
        final ASTSemanticMatcher matcher = new ASTSemanticMatcher();
        final ASTNode tryStmt = getFirstAncestorOrNull(node, TryStatement.class, BodyDeclaration.class);

        if (secondIf != null && isPassive(node.getExpression())
                && isPassive(secondIf.getExpression())
                && matcher.matchOpposite(node.getExpression(), secondIf.getExpression())
                && (!(tryStmt instanceof TryStatement) || secondIf.getElseStatement() == null)
                && (!fallsThrough(node.getThenStatement()) || !fallsThrough(secondIf.getThenStatement()))) {
            removeCondition(secondIf);

            return DO_NOT_VISIT_SUBTREE;
        }

        return VISIT_SUBTREE;
    }

    private void removeCondition(final IfStatement secondIf) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        final Refactorings r = this.ctx.getRefactorings();

        r.replace(secondIf, b.copy(secondIf.getThenStatement()));
    }
}

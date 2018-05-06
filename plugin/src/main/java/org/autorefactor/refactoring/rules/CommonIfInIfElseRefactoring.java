/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2017 Fabrice Tiercelin - Avoid to break the workflow
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
import static org.autorefactor.refactoring.ASTHelper.isPassive;
import static org.autorefactor.refactoring.ASTHelper.match;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.ASTSemanticMatcher;
import org.eclipse.jdt.core.dom.IfStatement;

/**
 * Refactors:
 *
 * <pre>
 * if (a) {
 *   if (moveAroundIfElse()) {
 *     ...
 *   }
 * } else {
 *   if (moveAroundIfElse()) {
 *     ...
 *   }
 * }
 * </pre>
 *
 * into
 *
 * <pre>
 * if (moveAroundIfElse()) {
 *   if (a) {
 *     ...
 *   } else {
 *     ...
 *   }
 * }
 * </pre>
 *
 * @see #getDescription()
 */
public class CommonIfInIfElseRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Move common inner if statement from then/else clauses around outer if statement";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Moves an inner if statement around the outer if condition,"
            + " when the inner if condition is common to both if/else clauses of the outer if statement.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces the coding, reading, debugging and testing cost.";
    }

    @Override
    public boolean visit(IfStatement node) {
        final IfStatement thenInnerIfStmt = as(node.getThenStatement(), IfStatement.class);
        final IfStatement elseInnerIfStmt = as(node.getElseStatement(), IfStatement.class);
        if (isPassive(node.getExpression())
                && thenInnerIfStmt != null
                && elseInnerIfStmt != null
                && thenInnerIfStmt.getElseStatement() == null
                && elseInnerIfStmt.getElseStatement() == null
                && isPassive(thenInnerIfStmt.getExpression())
                && match(new ASTSemanticMatcher(), thenInnerIfStmt.getExpression(), elseInnerIfStmt.getExpression())) {
            final ASTBuilder b = this.ctx.getASTBuilder();
            this.ctx.getRefactorings().replace(node,
                    b.if0(b.move(thenInnerIfStmt.getExpression()),
                            b.block(
                                    b.if0(b.move(node.getExpression()),
                                            b.move(thenInnerIfStmt.getThenStatement()),
                                            b.move(elseInnerIfStmt.getThenStatement())))));
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }
}

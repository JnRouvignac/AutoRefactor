/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Separate the code.
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
import static org.autorefactor.refactoring.ASTHelper.arg0;
import static org.autorefactor.refactoring.ASTHelper.as;
import static org.autorefactor.refactoring.ASTHelper.asExpression;
import static org.autorefactor.refactoring.ASTHelper.asList;
import static org.autorefactor.refactoring.ASTHelper.hasOperator;
import static org.autorefactor.refactoring.ASTHelper.isMethod;
import static org.autorefactor.refactoring.ASTHelper.match;
import static org.autorefactor.util.Utils.getFirst;
import static org.eclipse.jdt.core.dom.PrefixExpression.Operator.NOT;

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.ASTSemanticMatcher;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.Statement;

/** See {@link #getDescription()} method. */
public class UpdateSetRatherThanTestingFirstRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Update set rather than testing first";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Set related refactorings:\n"
            + "- replaces calls to Set.contains() immediately followed by Set.add()"
            + " with straight calls to Set.add(),\n"
            + "- replaces calls to Set.contains() immediately followed by Set.remove()"
            + " with straight calls to Set.remove().";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces the coding, reading and debugging time.";
    }

    @Override
    public boolean visit(IfStatement node) {
        final Statement elseStmt = node.getElseStatement();
        final Statement thenStmt = node.getThenStatement();
        final PrefixExpression pe = as(node.getExpression(), PrefixExpression.class);
        if (hasOperator(pe, NOT)) {
            return maybeReplaceSetContains(node, pe.getOperand(), thenStmt, elseStmt, false);
        } else {
            return maybeReplaceSetContains(node, node.getExpression(), elseStmt, thenStmt, true);
        }
    }

    private boolean maybeReplaceSetContains(final IfStatement ifStmtToReplace,
            final Expression ifExpr, final Statement stmt, final Statement oppositeStmt, final boolean negate) {
        return maybeReplaceSetContains(ifStmtToReplace, ifExpr, stmt, oppositeStmt, negate, "add")
                && maybeReplaceSetContains(ifStmtToReplace, ifExpr, oppositeStmt, stmt, !negate, "remove");
    }

    private boolean maybeReplaceSetContains(final IfStatement ifStmtToReplace,
            final Expression ifExpr, final Statement stmt, final Statement oppositeStmt,
            final boolean negate, final String methodName) {
        final List<Statement> stmts = asList(stmt);
        final MethodInvocation miContains = as(ifExpr, MethodInvocation.class);
        if (!stmts.isEmpty()
                && isMethod(miContains, "java.util.Set", "contains", "java.lang.Object")) {
            final Statement firstStmt = getFirst(stmts);
            final MethodInvocation miAddOrRemove = asExpression(firstStmt, MethodInvocation.class);
            final ASTSemanticMatcher astMatcher = new ASTSemanticMatcher();
            if (isMethod(miAddOrRemove, "java.util.Set", methodName, "java.lang.Object")
                    && match(astMatcher, miContains.getExpression(), miAddOrRemove.getExpression())
                    && match(astMatcher, arg0(miContains), arg0(miAddOrRemove))) {
                final ASTBuilder b = this.ctx.getASTBuilder();
                final Refactorings r = this.ctx.getRefactorings();

                if (stmts.size() == 1 && asList(oppositeStmt).isEmpty()) {
                    // Only one statement: replace if statement with col.add() (or col.remove())
                    r.replace(ifStmtToReplace, b.move(firstStmt));
                    return DO_NOT_VISIT_SUBTREE;
                } else {
                    // There are other statements, replace the if condition with col.add() (or col.remove())
                    r.replace(ifStmtToReplace.getExpression(),
                            negate ? b.negate(miAddOrRemove, ASTBuilder.Copy.MOVE) : b.move(miAddOrRemove));
                    r.remove(firstStmt);
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
        }
        return VISIT_SUBTREE;
    }
}

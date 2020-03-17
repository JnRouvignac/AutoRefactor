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
package org.autorefactor.jdt.internal.ui.fix;

import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.Statement;

/** See {@link #getDescription()} method. */
public class UpdateSetRatherThanTestingFirstCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_UpdateSetRatherThanTestingFirstCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_UpdateSetRatherThanTestingFirstCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_UpdateSetRatherThanTestingFirstCleanUp_reason;
    }

    @Override
    public boolean visit(final IfStatement node) {
        Statement elseStatement= node.getElseStatement();
        Statement thenStatement= node.getThenStatement();
        PrefixExpression pe= ASTNodes.as(node.getExpression(), PrefixExpression.class);

        if (ASTNodes.hasOperator(pe, PrefixExpression.Operator.NOT)) {
            return maybeReplaceSetContains(node, pe.getOperand(), thenStatement, elseStatement, false);
        }

        return maybeReplaceSetContains(node, node.getExpression(), elseStatement, thenStatement, true);
    }

    private boolean maybeReplaceSetContains(final IfStatement ifStmtToReplace, final Expression ifExpression,
            final Statement statement, final Statement oppositeStatement, final boolean negate) {
        return maybeReplaceSetContains(ifStmtToReplace, ifExpression, statement, oppositeStatement, negate, "add") //$NON-NLS-1$
                && maybeReplaceSetContains(ifStmtToReplace, ifExpression, oppositeStatement, statement, !negate, "remove"); //$NON-NLS-1$
    }

    private boolean maybeReplaceSetContains(final IfStatement ifStmtToReplace, final Expression ifExpression,
            final Statement statement, final Statement oppositeStatement, final boolean negate, final String methodName) {
        List<Statement> statements= ASTNodes.asList(statement);
        MethodInvocation miContains= ASTNodes.as(ifExpression, MethodInvocation.class);

        if (!statements.isEmpty() && ASTNodes.usesGivenSignature(miContains, Set.class.getCanonicalName(), "contains", Object.class.getCanonicalName())) { //$NON-NLS-1$
            Statement firstStatement= Utils.getFirst(statements);
            MethodInvocation miAddOrRemove= ASTNodes.asExpression(firstStatement, MethodInvocation.class);

            if (ASTNodes.usesGivenSignature(miAddOrRemove, Set.class.getCanonicalName(), methodName, Object.class.getCanonicalName())
                    && ASTNodes.match(miContains.getExpression(), miAddOrRemove.getExpression())
                    && ASTNodes.match(ASTNodes.arguments(miContains).get(0), ASTNodes.arguments(miAddOrRemove).get(0))) {
                ASTNodeFactory b= cuRewrite.getASTBuilder();
                ASTRewrite rewrite= cuRewrite.getASTRewrite();

                if (statements.size() == 1 && ASTNodes.asList(oppositeStatement).isEmpty()) {
                    // Only one statement: replace if statement with col.add() (or col.remove())
                    rewrite.replace(ifStmtToReplace, b.createMoveTarget(firstStatement));
                } else {
                    // There are other statements, replace the if condition with col.add() (or
                    // col.remove())
                    rewrite.replace(ifStmtToReplace.getExpression(),
                            negate ? b.negate(miAddOrRemove, ASTNodeFactory.Copy.MOVE) : b.createMoveTarget(miAddOrRemove));
                    rewrite.remove(firstStatement);
                }

                return false;
            }
        }

        return true;
    }
}

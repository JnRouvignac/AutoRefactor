/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.ASTNode.*;

/** See {@link #getDescription()} method. */
public class TryWithResourceRefactoring extends AbstractRefactoringRule {

    @Override
    public String getDescription() {
        return "";
    }

    @Override
    public String getName() {
        return "Use try-with-resource";
    }

    @Override
    public boolean visit(TryStatement node) {
        final List<Statement> tryStmts = asList(node.getBody());
        if (tryStmts.size() >= 1 && tryStmts.get(0).getNodeType() == TRY_STATEMENT) {
            final TryStatement innerTryStmt = as(tryStmts.get(0), TryStatement.class);
            if (innerTryStmt != null
                    && !innerTryStmt.resources().isEmpty()
                    && innerTryStmt.catchClauses().isEmpty()) {
                return collapseTryStatements(node, innerTryStmt);
            }
        }

        final Statement previousStmt = getPreviousStatement(node);
        final VariableDeclarationStatement previousDeclStmt = as(previousStmt, VariableDeclarationStatement.class);
        if (previousDeclStmt == null) {
            return VISIT_SUBTREE;
        }

        final List<VariableDeclarationFragment> previousDeclFragments = fragments(previousDeclStmt);
        final List<Statement> finallyStmts = asList(node.getFinally());
        if (previousDeclFragments.size() == 1 && finallyStmts.size() >= 1) {
            final VariableDeclarationFragment previousDeclFragment = previousDeclFragments.get(0);

            final Statement finallyStmt = finallyStmts.get(0);
            final ExpressionStatement es = as(finallyStmt, ExpressionStatement.class);
            final IfStatement is = as(finallyStmt, IfStatement.class);
            if (es != null) {
                final MethodInvocation mi = as(es.getExpression(), MethodInvocation.class);
                final Expression miExpr = mi.getExpression();
                if (isMethod(mi, "java.io.Closeable", "close")
                        && isSameVariable(previousDeclFragment, miExpr)) {
                    final Refactorings r = ctx.getRefactorings();
                    final ASTBuilder b = ctx.getASTBuilder();
                    r.remove(previousStmt);
                    r.insertFirst(node, TryStatement.RESOURCES_PROPERTY, b.toDeclareExpr(previousDeclStmt));
                    r.remove(finallyStmts.size() == 1 ? node.getFinally() : finallyStmt);
                    return DO_NOT_VISIT_SUBTREE;
                }
            } else if (is != null
                    && asList(is.getThenStatement()).size() == 1
                    && asList(is.getElseStatement()).isEmpty()) {
                final Statement tryStmt = tryStmts.get(0);
                final Assignment assignResource = asExpression(tryStmt, Assignment.class);
                if (assignResource != null) {
                    final Expression nullCheckedExpr = getNullCheckedExpression(is.getExpression());

                    final Statement thenStmt = asList(is.getThenStatement()).get(0);
                    final MethodInvocation mi = asExpression(thenStmt, MethodInvocation.class);
                    final Expression miExpr = mi.getExpression();
                    if (isMethod(mi, "java.io.Closeable", "close")
                            && areSameVariables(
                                    previousDeclFragment, assignResource.getLeftHandSide(), nullCheckedExpr, miExpr)) {
                        final Refactorings r = ctx.getRefactorings();
                        final ASTBuilder b = ctx.getASTBuilder();
                        r.remove(previousStmt);
                        r.insertFirst(node, TryStatement.RESOURCES_PROPERTY,
                                b.declareExpr(b.move(previousDeclStmt.getType()),
                                              b.move(previousDeclFragment.getName()),
                                              b.move(assignResource.getRightHandSide())));
                        r.remove(tryStmt);
                        r.remove(finallyStmts.size() == 1 ? node.getFinally() : finallyStmt);
                        return DO_NOT_VISIT_SUBTREE;
                    }
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean collapseTryStatements(TryStatement outerTryStmt, TryStatement innerTryStmt) {
        final Refactorings r = ctx.getRefactorings();
        final ASTBuilder b = ctx.getASTBuilder();
        r.insertLast(outerTryStmt, TryStatement.RESOURCES_PROPERTY, b.copyRange(resources(innerTryStmt)));
        r.replace(innerTryStmt, b.move(innerTryStmt.getBody()));
        return DO_NOT_VISIT_SUBTREE;
    }
}

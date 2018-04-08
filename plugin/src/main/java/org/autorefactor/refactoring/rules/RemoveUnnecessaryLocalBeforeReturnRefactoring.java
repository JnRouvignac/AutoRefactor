/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2017 Fabrice TIERCELIN - Avoid side effect
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
import static org.autorefactor.refactoring.ASTHelper.asExpression;
import static org.autorefactor.refactoring.ASTHelper.getAncestorOrNull;
import static org.autorefactor.refactoring.ASTHelper.getPreviousSibling;
import static org.autorefactor.refactoring.ASTHelper.getUniqueFragment;
import static org.autorefactor.refactoring.ASTHelper.hasOperator;
import static org.autorefactor.refactoring.ASTHelper.isSameLocalVariable;
import static org.eclipse.jdt.core.dom.Assignment.Operator.ASSIGN;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.BlockSubVisitor;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ArrayInitializer;
import org.eclipse.jdt.core.dom.ArrayType;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/** See {@link #getDescription()} method. */
public class RemoveUnnecessaryLocalBeforeReturnRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Remove unnecessary local before return";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Removes unnecessary local variable declaration"
            + " or unnecessary variable assignment before a return statement.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces code to focus attention on code that matters. It also improves time performance.";
    }

    @Override
    public boolean visit(Block node) {
        final ReturnStatementVisitor returnStatementVisitor = new ReturnStatementVisitor(ctx, node);
        node.accept(returnStatementVisitor);
        return returnStatementVisitor.getResult();
    }

    private static final class ReturnStatementVisitor extends BlockSubVisitor {

        public ReturnStatementVisitor(final RefactoringContext ctx, final Block startNode) {
            super(ctx, startNode);
        }

        @Override
        public boolean visit(ReturnStatement node) {
            final Statement previousSibling = getPreviousSibling(node);
            if (!getCtx().getRefactorings().hasBeenRefactored(previousSibling)
                    && previousSibling instanceof VariableDeclarationStatement) {
                final VariableDeclarationStatement vds = (VariableDeclarationStatement) previousSibling;
                final VariableDeclarationFragment vdf = getUniqueFragment(vds);

                if (vdf != null && isSameLocalVariable(node.getExpression(), vdf.getName())) {
                    removeVariable(node, vds, vdf);
                    setResult(DO_NOT_VISIT_SUBTREE);
                    return DO_NOT_VISIT_SUBTREE;
                }
            } else {
                final Assignment as = asExpression(previousSibling, Assignment.class);
                if (hasOperator(as, ASSIGN)
                        && isSameLocalVariable(node.getExpression(), as.getLeftHandSide())
                        && !isUsedAfterReturn(((IVariableBinding) ((Name) as.getLeftHandSide()).resolveBinding()),
                                node)) {
                    replaceReturnStatement(node, previousSibling, as.getRightHandSide());
                    setResult(DO_NOT_VISIT_SUBTREE);
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
            return VISIT_SUBTREE;
        }

        private boolean isUsedAfterReturn(final IVariableBinding varToSearch, final ASTNode scopeNode) {
            final TryStatement tryStmt =
                    getAncestorOrNull(scopeNode, TryStatement.class);
            if (tryStmt == null) {
                return false;
            } else {
                if (tryStmt.getFinally() != null) {
                    final VariableDefinitionsUsesVisitor variableUseVisitor =
                            new VariableDefinitionsUsesVisitor(varToSearch, tryStmt.getFinally()).find();
                    if (!variableUseVisitor.getUses().isEmpty()) {
                        return true;
                    }
                }
                return isUsedAfterReturn(varToSearch, tryStmt);
            }
        }

        private void removeVariable(final ReturnStatement node, final VariableDeclarationStatement vds,
                final VariableDeclarationFragment vdf) {
            final Expression returnExpr = vdf.getInitializer();
            if (returnExpr instanceof ArrayInitializer) {
                final ASTBuilder b = getCtx().getASTBuilder();
                final ReturnStatement newReturnStmt =
                        b.return0(b.newArray(
                                b.copy((ArrayType) vds.getType()),
                                b.move((ArrayInitializer) returnExpr)));
                replaceReturnStatementForArray(node, vds, newReturnStmt);
            } else {
                replaceReturnStatement(node, vds, returnExpr);
            }
        }

        private void replaceReturnStatementForArray(final ReturnStatement node, final Statement previousSibling,
                final ReturnStatement newReturnStmt) {
            final Refactorings r = getCtx().getRefactorings();
            r.remove(previousSibling);
            r.replace(node, newReturnStmt);
        }

        private void replaceReturnStatement(final ReturnStatement node, final Statement previousSibling,
                final Expression returnExpr) {
            final ASTBuilder b = getCtx().getASTBuilder();
            final Refactorings r = getCtx().getRefactorings();
            r.remove(previousSibling);
            r.replace(node, b.return0(b.move(returnExpr)));
        }
    }
}

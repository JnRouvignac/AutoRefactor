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
package org.autorefactor.jdt.internal.ui.fix;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.autorefactor.jdt.internal.corext.dom.VarDefinitionsUsesVisitor;
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
public class RemoveUnnecessaryLocalBeforeReturnCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_RemoveUnnecessaryLocalBeforeReturnCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_RemoveUnnecessaryLocalBeforeReturnCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_RemoveUnnecessaryLocalBeforeReturnCleanUp_reason;
    }

    @Override
    public boolean visit(final Block node) {
        final ReturnStatementVisitor returnStatementVisitor= new ReturnStatementVisitor(ctx, node);
        node.accept(returnStatementVisitor);
        return returnStatementVisitor.getResult();
    }

    private static final class ReturnStatementVisitor extends BlockSubVisitor {
        public ReturnStatementVisitor(final RefactoringContext ctx, final Block startNode) {
            super(ctx, startNode);
        }

        @Override
        public boolean visit(final ReturnStatement node) {
            final Statement previousSibling= ASTNodes.getPreviousSibling(node);
            if (!ctx.getRefactorings().hasBeenRefactored(previousSibling)
                    && previousSibling instanceof VariableDeclarationStatement) {
                final VariableDeclarationStatement vds= (VariableDeclarationStatement) previousSibling;
                final VariableDeclarationFragment vdf= ASTNodes.getUniqueFragment(vds);

                if (vdf != null && ASTNodes.isSameLocalVariable(node.getExpression(), vdf.getName())) {
                    removeVariable(node, vds, vdf);
                    setResult(false);
                    return false;
                }
            } else {
                final Assignment as= ASTNodes.asExpression(previousSibling, Assignment.class);
                if (ASTNodes.hasOperator(as, Assignment.Operator.ASSIGN) && ASTNodes.isSameLocalVariable(node.getExpression(), as.getLeftHandSide())
                        && as.getLeftHandSide() instanceof Name
                        && !isUsedAfterReturn((IVariableBinding) ((Name) as.getLeftHandSide()).resolveBinding(),
                                node)) {
                    replaceReturnStatement(node, previousSibling, as.getRightHandSide());
                    setResult(false);
                    return false;
                }
            }

            return true;
        }

        private boolean isUsedAfterReturn(final IVariableBinding varToSearch, final ASTNode scopeNode) {
            final TryStatement tryStatement= ASTNodes.getAncestorOrNull(scopeNode, TryStatement.class);
            if (tryStatement == null) {
                return false;
            }
            if (tryStatement.getFinally() != null) {
                final VarDefinitionsUsesVisitor variableUseVisitor= new VarDefinitionsUsesVisitor(
                        varToSearch, tryStatement.getFinally(), true).find();
                if (!variableUseVisitor.getReads().isEmpty()) {
                    return true;
                }
            }

            return isUsedAfterReturn(varToSearch, tryStatement);
        }

        private void removeVariable(final ReturnStatement node, final VariableDeclarationStatement vds,
                final VariableDeclarationFragment vdf) {
            final Expression returnExpression= vdf.getInitializer();
            if (returnExpression instanceof ArrayInitializer) {
                final ASTNodeFactory b= ctx.getASTBuilder();
                final ReturnStatement newReturnStatement= b
                        .return0(b.newArray(b.createCopyTarget((ArrayType) vds.getType()), b.createMoveTarget((ArrayInitializer) returnExpression)));
                replaceReturnStatementForArray(node, vds, newReturnStatement);
            } else {
                replaceReturnStatement(node, vds, returnExpression);
            }
        }

        private void replaceReturnStatementForArray(final ReturnStatement node, final Statement previousSibling,
                final ReturnStatement newReturnStatement) {
            final Refactorings r= ctx.getRefactorings();
            r.remove(previousSibling);
            r.replace(node, newReturnStatement);
        }

        private void replaceReturnStatement(final ReturnStatement node, final Statement previousSibling,
                final Expression returnExpression) {
            final ASTNodeFactory b= ctx.getASTBuilder();
            final Refactorings r= ctx.getRefactorings();
            r.remove(previousSibling);
            r.replace(node, b.return0(b.createMoveTarget(returnExpression)));
        }
    }
}

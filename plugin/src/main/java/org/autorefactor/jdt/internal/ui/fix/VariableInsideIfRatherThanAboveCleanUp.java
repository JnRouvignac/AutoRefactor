/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - initial API and implementation
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

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.autorefactor.jdt.internal.corext.dom.VarOccurrenceVisitor;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/** See {@link #getDescription()} method. */
public class VariableInsideIfRatherThanAboveCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_VariableInsideIfRatherThanAboveCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_VariableInsideIfRatherThanAboveCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_VariableInsideIfRatherThanAboveCleanUp_reason;
    }

    @Override
    public boolean visit(final Block node) {
        VariableAndIfVisitor newAndPutAllMethodVisitor= new VariableAndIfVisitor(ctx, node);
        node.accept(newAndPutAllMethodVisitor);
        return newAndPutAllMethodVisitor.getResult();
    }

    private static final class VariableAndIfVisitor extends BlockSubVisitor {
        public VariableAndIfVisitor(final RefactoringContext ctx, final Block startNode) {
            super(ctx, startNode);
        }

        @Override
        public boolean visit(final IfStatement node) {
            Statement variableAssignment= ASTNodes.getPreviousSibling(node);
            SimpleName variable= getVariable(variableAssignment);

            if (variable == null) {
                return true;
            }

            HashSet<String> variableToFind= new HashSet<>(Arrays.asList(variable.getIdentifier()));
            VarOccurrenceVisitor varOccurrenceVisitor= new VarOccurrenceVisitor(variableToFind, true);
            varOccurrenceVisitor.visitNode(node.getExpression());

            for (Statement statement : ASTNodes.getNextSiblings(node)) {
                varOccurrenceVisitor.visitNode(statement);
            }

            if (varOccurrenceVisitor.isVarUsed()) {
                return true;
            }

            varOccurrenceVisitor= new VarOccurrenceVisitor(variableToFind, true);
            varOccurrenceVisitor.visitNode(node.getThenStatement());

            if (varOccurrenceVisitor.isVarUsed()) {
                if (node.getElseStatement() != null) {
                    varOccurrenceVisitor= new VarOccurrenceVisitor(variableToFind, true);
                    varOccurrenceVisitor.visitNode(node.getElseStatement());

                    if (varOccurrenceVisitor.isVarUsed()) {
                        return true;
                    }
                }

                return maybeMoveAssignment(variableAssignment, node.getThenStatement());
            }

            if (node.getElseStatement() != null) {
                varOccurrenceVisitor= new VarOccurrenceVisitor(variableToFind, true);
                varOccurrenceVisitor.visitNode(node.getElseStatement());

                return !varOccurrenceVisitor.isVarUsed() || maybeMoveAssignment(variableAssignment, node.getElseStatement());
            }

            return true;
        }

        private SimpleName getVariable(final Statement variableAssignment) {
            VariableDeclarationStatement variableDeclarationStatement= ASTNodes.as(variableAssignment, VariableDeclarationStatement.class);

            if (variableDeclarationStatement != null && variableDeclarationStatement.fragments().size() == 1) {
                VariableDeclarationFragment fragment= (VariableDeclarationFragment) variableDeclarationStatement.fragments().get(0);

                if (fragment.getInitializer() == null || ASTNodes.isPassiveWithoutFallingThrough(fragment.getInitializer())) {
                    return fragment.getName();
                }
            }

            return null;
        }

        private boolean maybeMoveAssignment(final Statement variableAssignment, final Statement statement) {
            List<Statement> statements= ASTNodes.asList(statement);

            if (statements.isEmpty()) {
                return true;
            }

            moveAssignmentInsideIf(variableAssignment, statement, statements);
            return false;
        }

        private void moveAssignmentInsideIf(final Statement variableAssignment, final Statement statement,
                final List<Statement> statements) {
            Refactorings r= this.ctx.getRefactorings();
            ASTNodeFactory b= this.ctx.getASTBuilder();

            if (statement instanceof Block) {
                r.insertBefore(b.createMoveTarget(variableAssignment), statements.get(0));
                r.remove(variableAssignment);
            } else {
                List<Statement> copyOfThenStatements= b.createMoveTarget(statements);
                copyOfThenStatements.add(0, b.createMoveTarget(variableAssignment));
                Block block= b.block(copyOfThenStatements);
                r.replace(statement, block);
            }
        }
    }
}

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

import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.dom.VarDefinitionsUsesVisitor;
import org.autorefactor.jdt.internal.corext.refactoring.structure.CompilationUnitRewrite;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.IfStatement;
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
        VariableAndIfVisitor newAndPutAllMethodVisitor= new VariableAndIfVisitor(cuRewrite, node);
        node.accept(newAndPutAllMethodVisitor);
        return newAndPutAllMethodVisitor.getResult();
    }

    private static final class VariableAndIfVisitor extends BlockSubVisitor {
        public VariableAndIfVisitor(final CompilationUnitRewrite cuRewrite, final Block startNode) {
            super(cuRewrite, startNode);
        }

        @Override
        public boolean visit(final IfStatement node) {
            if (getResult()) {
                Statement variableAssignment= ASTNodes.getPreviousSibling(node);
                VariableDeclarationFragment variable= getVariable(variableAssignment);

                if (variable == null || isVarUsed(variable, node.getExpression())) {
                    return true;
                }

                for (Statement statement : ASTNodes.getNextSiblings(node)) {
                    if (isVarUsed(variable, statement)) {
                        return true;
                    }
                }

                if (isVarUsed(variable, node.getThenStatement())) {
                    if (node.getElseStatement() != null && isVarUsed(variable, node.getElseStatement())) {
                        return true;
                    }

                    return maybeMoveAssignment(variableAssignment, node.getThenStatement());
                }

                if (node.getElseStatement() != null) {
                    return !isVarUsed(variable, node.getElseStatement()) || maybeMoveAssignment(variableAssignment, node.getElseStatement());
                }
            }

            return true;
        }

        private boolean isVarUsed(final VariableDeclarationFragment variable, final ASTNode astNode) {
            VarDefinitionsUsesVisitor varOccurrenceVisitor= new VarDefinitionsUsesVisitor(variable.resolveBinding(), astNode, true).find();
            return !varOccurrenceVisitor.getWrites().isEmpty() || !varOccurrenceVisitor.getReads().isEmpty();
        }

        private VariableDeclarationFragment getVariable(final Statement variableAssignment) {
            VariableDeclarationStatement variableDeclarationStatement= ASTNodes.as(variableAssignment, VariableDeclarationStatement.class);

            if (variableDeclarationStatement != null && variableDeclarationStatement.fragments().size() == 1) {
                VariableDeclarationFragment fragment= (VariableDeclarationFragment) variableDeclarationStatement.fragments().get(0);

                if (fragment.getInitializer() == null || ASTNodes.isPassiveWithoutFallingThrough(fragment.getInitializer())) {
                    return fragment;
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
            setResult(false);
            return false;
        }

        private void moveAssignmentInsideIf(final Statement variableAssignment, final Statement statement,
                final List<Statement> statements) {
            ASTRewrite rewrite= cuRewrite.getASTRewrite();
            ASTNodeFactory ast= cuRewrite.getASTBuilder();

            if (statement instanceof Block) {
                rewrite.insertBefore(rewrite.createMoveTarget(variableAssignment), statements.get(0), null);
                rewrite.remove(variableAssignment, null);
            } else {
                List<Statement> copyOfThenStatements= rewrite.createMoveTarget(statements);
                copyOfThenStatements.add(0, rewrite.createMoveTarget(variableAssignment));
                Block block= ast.block(copyOfThenStatements);
                rewrite.replace(statement, block, null);
            }
        }
    }
}

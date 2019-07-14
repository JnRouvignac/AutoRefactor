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

import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.VISIT_SUBTREE;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.as;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.asList;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.getLocalVariableIdentifiers;
import static org.eclipse.jdt.core.dom.Modifier.isFinal;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.internal.corext.dom.ASTBuilder;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IExtendedModifier;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.jdt.core.dom.WhileStatement;

/** See {@link #getDescription()} method. */
public class DeclarationOutsideLoopRatherThanInsideCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_DeclarationOutsideLoopRatherThanInsideCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_DeclarationOutsideLoopRatherThanInsideCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_DeclarationOutsideLoopRatherThanInsideCleanUp_reason;
    }

    @Override
    public boolean visit(final Block node) {
        final List<Statement> blockStmt= asList(node);
        boolean result= VISIT_SUBTREE;

        List<Statement> forStmts;
        for (int i= 0; i < blockStmt.size(); i++) {
            final Statement stmt= blockStmt.get(i);
            final ForStatement forStatement= as(stmt, ForStatement.class);
            final EnhancedForStatement enhancedForStatement= as(stmt, EnhancedForStatement.class);
            final WhileStatement whileStatement= as(stmt, WhileStatement.class);
            final DoStatement doStatement= as(stmt, DoStatement.class);
            forStmts= null;

            if (forStatement != null) {
                forStmts= asList(forStatement.getBody());
            } else if (enhancedForStatement != null) {
                forStmts= asList(enhancedForStatement.getBody());
            } else if (whileStatement != null) {
                forStmts= asList(whileStatement.getBody());
            } else if (doStatement != null) {
                forStmts= asList(doStatement.getBody());
            }

            if (forStmts != null) {
                final Set<String> varNames= new HashSet<String>();

                for (int j= 0; j < i; j++) {
                    if (!(blockStmt.get(j) instanceof Block)) {
                        varNames.addAll(getLocalVariableIdentifiers(blockStmt.get(j), false));
                    }
                }
                for (int j= i + 1; j < blockStmt.size(); j++) {
                    varNames.addAll(getLocalVariableIdentifiers(blockStmt.get(j), true));
                }

                final List<VariableDeclarationStatement> candidates= new ArrayList<VariableDeclarationStatement>();

                for (final Statement forStmt : forStmts) {
                    final VariableDeclarationStatement decl= as(forStmt, VariableDeclarationStatement.class);

                    if (decl != null && !isFinal(decl.getModifiers()) && !hasAnnotation(decl.modifiers())
                            && decl.fragments() != null && decl.fragments().size() == 1) {
                        final VariableDeclarationFragment fragment= (VariableDeclarationFragment) decl.fragments()
                                .get(0);
                        final String id= fragment.getName().getIdentifier();

                        if (!varNames.contains(id)) {
                            candidates.add(decl);
                            varNames.add(id);
                        }
                    }
                }

                final ASTBuilder b= this.ctx.getASTBuilder();
                final Refactorings r= this.ctx.getRefactorings();

                for (final VariableDeclarationStatement candidate : candidates) {
                    moveDeclaration(b, r, stmt, candidate);
                    result= DO_NOT_VISIT_SUBTREE;
                }
            }
        }

        return result;
    }

    @SuppressWarnings("unchecked")
    private boolean hasAnnotation(final List<?> modifiers) {
        for (final IExtendedModifier em : (List<IExtendedModifier>) modifiers) {
            if (!em.isModifier()) {
                return true;
            }
        }

        return false;
    }

    private void moveDeclaration(final ASTBuilder b, final Refactorings r, final Statement stmt,
            final VariableDeclarationStatement varToMove) {
        final VariableDeclarationFragment fragment= (VariableDeclarationFragment) varToMove.fragments().get(0);

        if (fragment.getInitializer() != null) {
            final Type copyOfType= b.copy(varToMove.getType());
            final SimpleName name= fragment.getName();
            r.insertBefore(b.declareStmt(copyOfType, b.declareFragment(b.copy(name))), stmt);
            r.replace(varToMove,
                    b.toStmt(b.assign(b.copy(name), Assignment.Operator.ASSIGN, b.copy(fragment.getInitializer()))));
        } else {
            r.insertBefore(b.copy(varToMove), stmt);
            r.remove(varToMove);
        }
    }
}

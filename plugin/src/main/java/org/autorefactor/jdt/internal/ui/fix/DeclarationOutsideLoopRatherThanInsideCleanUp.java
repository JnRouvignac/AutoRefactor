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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.Dimension;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IExtendedModifier;
import org.eclipse.jdt.core.dom.Modifier;
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
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_DeclarationOutsideLoopRatherThanInsideCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_DeclarationOutsideLoopRatherThanInsideCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_DeclarationOutsideLoopRatherThanInsideCleanUp_reason;
    }

    @Override
    public boolean visit(final Block node) {
        final List<Statement> blockStatement= ASTNodes.asList(node);
        boolean result= true;

        List<Statement> forStatements;
        for (int i= 0; i < blockStatement.size(); i++) {
            final Statement statement= blockStatement.get(i);
            final ForStatement forStatement= ASTNodes.as(statement, ForStatement.class);
            final EnhancedForStatement enhancedForStatement= ASTNodes.as(statement, EnhancedForStatement.class);
            final WhileStatement whileStatement= ASTNodes.as(statement, WhileStatement.class);
            final DoStatement doStatement= ASTNodes.as(statement, DoStatement.class);
            forStatements= null;

            if (forStatement != null) {
                forStatements= ASTNodes.asList(forStatement.getBody());
            } else if (enhancedForStatement != null) {
                forStatements= ASTNodes.asList(enhancedForStatement.getBody());
            } else if (whileStatement != null) {
                forStatements= ASTNodes.asList(whileStatement.getBody());
            } else if (doStatement != null) {
                forStatements= ASTNodes.asList(doStatement.getBody());
            }

            if (forStatements != null) {
                final Set<String> varNames= new HashSet<>();

                for (int j= 0; j < i; j++) {
                    if (!(blockStatement.get(j) instanceof Block)) {
                        varNames.addAll(ASTNodes.getLocalVariableIdentifiers(blockStatement.get(j), false));
                    }
                }
                for (int j= i + 1; j < blockStatement.size(); j++) {
                    varNames.addAll(ASTNodes.getLocalVariableIdentifiers(blockStatement.get(j), true));
                }

                final List<VariableDeclarationStatement> candidates= new ArrayList<>();

                for (Statement declarationStatement : forStatements) {
                    final VariableDeclarationStatement decl= ASTNodes.as(declarationStatement, VariableDeclarationStatement.class);

                    if (decl != null && !Modifier.isFinal(decl.getModifiers()) && !hasAnnotation(decl.modifiers())
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

                final ASTNodeFactory b= this.ctx.getASTBuilder();
                final Refactorings r= this.ctx.getRefactorings();

                for (VariableDeclarationStatement candidate : candidates) {
                    moveDeclaration(b, r, statement, candidate);
                    result= false;
                }
            }
        }

        return result;
    }

    @SuppressWarnings("unchecked")
    private boolean hasAnnotation(final List<?> modifiers) {
        for (IExtendedModifier em : (List<IExtendedModifier>) modifiers) {
            if (em.isAnnotation()) {
                return true;
            }
        }

        return false;
    }

    private void moveDeclaration(final ASTNodeFactory b, final Refactorings r, final Statement statement,
            final VariableDeclarationStatement varToMove) {
        final VariableDeclarationFragment fragment= (VariableDeclarationFragment) varToMove.fragments().get(0);

        if (fragment.getInitializer() != null) {
            final Type copyOfType= b.copy(varToMove.getType());
            final SimpleName name= fragment.getName();
            VariableDeclarationFragment newFragment= b.declareFragment(b.copy(name));
            @SuppressWarnings("unchecked")
            List<Dimension> extraDimensions= fragment.extraDimensions();
            @SuppressWarnings("unchecked")
            List<Dimension> newExtraDimensions= newFragment.extraDimensions();
            newExtraDimensions.addAll(b.move(extraDimensions));
            VariableDeclarationStatement newDeclareStatement= b.declareStatement(copyOfType, newFragment);
            @SuppressWarnings("unchecked")
            List<IExtendedModifier> modifiers= varToMove.modifiers();
            @SuppressWarnings("unchecked")
            List<IExtendedModifier> newModifiers= newDeclareStatement.modifiers();

            for (IExtendedModifier iExtendedModifier : modifiers) {
                Modifier modifier= (Modifier) iExtendedModifier;

                if (!modifier.isPrivate() && !modifier.isStatic()) {
                    newModifiers.add(b.move(modifier));
                }
            }

            r.insertBefore(newDeclareStatement, statement);
            r.replace(varToMove,
                    b.toStatement(b.assign(b.copy(name), Assignment.Operator.ASSIGN, b.move(fragment.getInitializer()))));
        } else {
            r.insertBefore(b.move(varToMove), statement);
            r.remove(varToMove);
        }
    }
}

/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice Tiercelin - Initial API and implementation
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

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.Javadoc;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/** See {@link #getDescription()} method. */
public class SingleDeclarationsRatherThanMultiDeclarationCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Single declarations rather than multi declaration";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Write only one variable declaration per line.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It makes the code more standard. It fixes the Sonar RSPEC-1659.";
    }

    @Override
    public boolean visit(final FieldDeclaration node) {
        return visitMultiDeclaration(node, node.modifiers(), node.getType(), node.fragments(), node.getJavadoc());
    }

    @Override
    public boolean visit(final VariableDeclarationStatement node) {
        return visitMultiDeclaration(node, node.modifiers(), node.getType(), node.fragments(), null);
    }

    @SuppressWarnings("rawtypes")
    private boolean visitMultiDeclaration(final ASTNode node, final List modifiers, final Type type,
            final List fragments, final Javadoc docComment) {
        if (fragments != null && fragments.size() > 1) {
            refactorMultiDeclaration(node, modifiers, type, fragments, docComment);

            return DO_NOT_VISIT_SUBTREE;
        }

        return VISIT_SUBTREE;
    }

    @SuppressWarnings("rawtypes")
    private void refactorMultiDeclaration(final ASTNode node, final List modifiers, final Type type,
            final List fragments, final Javadoc docComment) {
        final ASTBuilder b = this.ctx.getASTBuilder();

        for (int i = fragments.size() - 1; 0 <= i; i--) {
            final VariableDeclarationFragment fragment = (VariableDeclarationFragment) fragments.get(i);

            final SimpleName copyOfFragment = b.copy(fragment.getName());
            final Type copyOfType = b.copy(type);
            final Expression copyOfInitializer;
            if (fragment.getInitializer() != null) {
                copyOfInitializer = b.copy(fragment.getInitializer());
            } else {
                copyOfInitializer = null;
            }

            final VariableDeclarationFragment newFragment = b.declareFragment(copyOfFragment, copyOfInitializer);
            final ASTNode newNode;
            if (node instanceof VariableDeclarationStatement) {
                final VariableDeclarationStatement newStmt = b.declareStmt(copyOfType, newFragment);
                updateModifiers(b, modifiers, newStmt.modifiers());
                newNode = newStmt;
            } else {
                final FieldDeclaration newField = b.declareField(copyOfType, newFragment);
                if (docComment != null) {
                    newField.setJavadoc(b.copy(docComment));
                }
                updateModifiers(b, modifiers, newField.modifiers());
                newNode = newField;
            }

            if (i > 0) {
                ctx.getRefactorings().insertAfter(newNode, node);
            } else {
                ctx.getRefactorings().replace(node, newNode);
            }
        }
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    private void updateModifiers(final ASTBuilder b, final List modifiers, final List newModifiers) {
        newModifiers.clear();
        for (final Object modifier : modifiers) {
            newModifiers.add(b.copy((ASTNode) modifier));
        }
    }
}

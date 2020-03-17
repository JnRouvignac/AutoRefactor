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

import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
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
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_SingleDeclarationsRatherThanMultiDeclarationCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_SingleDeclarationsRatherThanMultiDeclarationCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_SingleDeclarationsRatherThanMultiDeclarationCleanUp_reason;
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

            return false;
        }

        return true;
    }

    @SuppressWarnings("rawtypes")
    private void refactorMultiDeclaration(final ASTNode node, final List modifiers, final Type type,
            final List fragments, final Javadoc docComment) {
        ASTNodeFactory b= this.cuRewrite.getASTBuilder();

        for (int i= fragments.size() - 1; 0 <= i; i--) {
            VariableDeclarationFragment fragment= (VariableDeclarationFragment) fragments.get(i);

            SimpleName copyOfFragment= b.createCopyTarget(fragment.getName());
            Type copyOfType= b.createCopyTarget(type);
            Expression copyOfInitializer;
            if (fragment.getInitializer() != null) {
                copyOfInitializer= b.createCopyTarget(fragment.getInitializer());
            } else {
                copyOfInitializer= null;
            }

            VariableDeclarationFragment newFragment= b.declareFragment(copyOfFragment, copyOfInitializer);
            ASTNode newNode;
            if (node instanceof VariableDeclarationStatement) {
                VariableDeclarationStatement newStatement= b.declareStatement(copyOfType, newFragment);
                updateModifiers(b, modifiers, newStatement.modifiers());
                newNode= newStatement;
            } else {
                FieldDeclaration newField= b.declareField(copyOfType, newFragment);
                if (docComment != null) {
                    newField.setJavadoc(b.createCopyTarget(docComment));
                }
                updateModifiers(b, modifiers, newField.modifiers());
                newNode= newField;
            }

            if (i > 0) {
                cuRewrite.getRefactorings().insertAfter(newNode, node);
            } else {
                cuRewrite.getRefactorings().replace(node, newNode);
            }
        }
    }

    @SuppressWarnings({ "rawtypes", "unchecked" }) // $NON-NLS-2$
    private void updateModifiers(final ASTNodeFactory b, final List modifiers, final List newModifiers) {
        newModifiers.clear();
        for (Object modifier : modifiers) {
            newModifiers.add(b.createCopyTarget((ASTNode) modifier));
        }
    }
}

/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice TIERCELIN - initial API and implementation
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
import static org.autorefactor.refactoring.ASTHelper.hasType;
import static org.autorefactor.refactoring.ASTHelper.modifiers;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.IExtendedModifier;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

/** See {@link #getDescription()} method. */
public class StaticConstantRatherThanInstanceConstantRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Static constant rather than instance constant";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Add the static modifier to the initialized final primitive or wrapper fields.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves the space performance.";
    }

    @Override
    public boolean visit(FieldDeclaration node) {
        if (node.getType().isPrimitiveType()
                || hasType(node.getType().resolveBinding(), "java.lang.Byte")
                || hasType(node.getType().resolveBinding(), "java.lang.Character")
                || hasType(node.getType().resolveBinding(), "java.lang.Short")
                || hasType(node.getType().resolveBinding(), "java.lang.Integer")
                || hasType(node.getType().resolveBinding(), "java.lang.Long")
                || hasType(node.getType().resolveBinding(), "java.lang.Boolean")
                || hasType(node.getType().resolveBinding(), "java.lang.Float")
                || hasType(node.getType().resolveBinding(), "java.lang.Double")
                || hasType(node.getType().resolveBinding(), "java.lang.String")) {
            Modifier finalModifier = null;
            for (final Modifier modifier : getModifiersOnly(modifiers(node))) {
                if (modifier.isStatic()) {
                    return VISIT_SUBTREE;
                }
                if (modifier.isFinal()) {
                    finalModifier = modifier;
                }
            }

            if (finalModifier != null && node.fragments() != null && node.fragments().size() == 1) {
                final Expression initializer =
                        ((VariableDeclarationFragment) node.fragments().get(0)).getInitializer();

                if (isLiteral(initializer)) {
                    addStaticModifier(finalModifier);
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
        }

        return VISIT_SUBTREE;
    }

    private void addStaticModifier(final Modifier finalModifier) {
        final ASTBuilder b = ctx.getASTBuilder();
        final Refactorings r = ctx.getRefactorings();

        r.insertBefore(b.static0(), finalModifier);
    }

    private boolean isLiteral(final Expression initializer) {
        if (initializer != null) {
            switch (initializer.getNodeType()) {
            case ASTNode.BOOLEAN_LITERAL:
            case ASTNode.CHARACTER_LITERAL:
            case ASTNode.NUMBER_LITERAL:
            case ASTNode.STRING_LITERAL:
            case ASTNode.NULL_LITERAL:
                return true;

            default:
                return false;
            }
        } else {
            return false;
        }
    }

    private List<Modifier> getModifiersOnly(final Collection<IExtendedModifier> modifiers) {
        final List<Modifier> results = new LinkedList<Modifier>();
        for (final IExtendedModifier em : modifiers) {
            if (em.isModifier()) {
                results.add((Modifier) em);
            }
        }
        return results;
    }
}

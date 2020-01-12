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
package org.autorefactor.jdt.internal.ui.fix;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.IExtendedModifier;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

/** See {@link #getDescription()} method. */
public class StaticConstantRatherThanInstanceConstantCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_StaticConstantRatherThanInstanceConstantCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_StaticConstantRatherThanInstanceConstantCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_StaticConstantRatherThanInstanceConstantCleanUp_reason;
    }

    @Override
    public boolean visit(final FieldDeclaration node) {
        if (node.getType().isPrimitiveType() || ASTNodes.hasType(node.getType().resolveBinding(), Byte.class.getCanonicalName(), Character.class.getCanonicalName(), Short.class.getCanonicalName(),
                Integer.class.getCanonicalName(), Long.class.getCanonicalName(), Boolean.class.getCanonicalName(), Float.class.getCanonicalName(), Double.class.getCanonicalName(), String.class.getCanonicalName())) {
            Modifier finalModifier= null;
            for (Modifier modifier : getModifiersOnly(ASTNodes.modifiers(node))) {
                if (modifier.isStatic()) {
                    return true;
                }
                if (modifier.isFinal()) {
                    finalModifier= modifier;
                }
            }

            if (finalModifier != null && node.fragments() != null && node.fragments().size() == 1) {
                final Expression initializer= ((VariableDeclarationFragment) node.fragments().get(0)).getInitializer();

                if (ASTNodes.isHardCoded(initializer)) {
                    addStaticModifier(finalModifier);
                    return false;
                }
            }
        }

        return true;
    }

    private void addStaticModifier(final Modifier finalModifier) {
        final ASTNodeFactory b= ctx.getASTBuilder();
        final Refactorings r= ctx.getRefactorings();

        r.insertBefore(b.static0(), finalModifier);
    }

    private List<Modifier> getModifiersOnly(final Collection<IExtendedModifier> modifiers) {
        final List<Modifier> results= new LinkedList<>();
        for (IExtendedModifier em : modifiers) {
            if (em.isModifier()) {
                results.add((Modifier) em);
            }
        }

        return results;
    }
}

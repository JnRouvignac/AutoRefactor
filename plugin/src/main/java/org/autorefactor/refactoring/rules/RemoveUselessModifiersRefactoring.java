/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2015 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.util.NotImplementedException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.AnnotationTypeDeclaration;
import org.eclipse.jdt.core.dom.AnnotationTypeMemberDeclaration;
import org.eclipse.jdt.core.dom.BodyDeclaration;
import org.eclipse.jdt.core.dom.EnumDeclaration;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.IExtendedModifier;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.Modifier.ModifierKeyword;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;

import static org.autorefactor.refactoring.ASTHelper.*;

/** See {@link #getDescription()} method. */
public class RemoveUselessModifiersRefactoring extends AbstractRefactoringRule {
    @Override
    public String getDescription() {
        return ""
            + "Fixes modifier order.\n"
            + "Also removes modifiers implied by the context:\n"
            + "- \"public\", \"static\" and \"final\" for interface fields,\n"
            + "- \"public\" and \"abstract\" for interface methods,\n"
            + "- \"final\" for parameters in interface method declarations.";
    }

    @Override
    public String getName() {
        return "Remove useless modifiers";
    }

    private static final class ModifierOrderComparator implements Comparator<Modifier> {
        @Override
        public int compare(Modifier o1, Modifier o2) {
            final int i1 = ORDERED_MODIFIERS.indexOf(o1.getKeyword());
            final int i2 = ORDERED_MODIFIERS.indexOf(o2.getKeyword());
            if (i1 == -1) {
                throw new NotImplementedException(o1, "cannot determine order for modifier " + o1);
            }
            if (i2 == -1) {
                throw new NotImplementedException(o2, "cannot compare modifier " + o2);
            }
            return i1 - i2;
        }
    }

    private static final List<ModifierKeyword> ORDERED_MODIFIERS =
            Collections.unmodifiableList(Arrays.asList(
                    ModifierKeyword.PUBLIC_KEYWORD,
                    ModifierKeyword.PROTECTED_KEYWORD,
                    ModifierKeyword.PRIVATE_KEYWORD,
                    ModifierKeyword.STATIC_KEYWORD,
                    ModifierKeyword.ABSTRACT_KEYWORD,
                    ModifierKeyword.FINAL_KEYWORD,
                    ModifierKeyword.TRANSIENT_KEYWORD,
                    ModifierKeyword.VOLATILE_KEYWORD,
                    ModifierKeyword.SYNCHRONIZED_KEYWORD,
                    ModifierKeyword.NATIVE_KEYWORD,
                    ModifierKeyword.STRICTFP_KEYWORD));

    @Override
    public boolean visit(FieldDeclaration node) {
        if (isInterface(node.getParent())) {
            return removePublicStaticFinalModifiers(node);
        }
        return ensureModifiersOrder(node);
    }

    private boolean removePublicStaticFinalModifiers(FieldDeclaration node) {
        // remove modifiers implied by the context
        boolean result = VISIT_SUBTREE;
        for (Modifier m : getModifiersOnly(modifiers(node))) {
            if (m.isPublic() || m.isStatic() || m.isFinal()) {
                ctx.getRefactorings().remove(m);
                result = DO_NOT_VISIT_SUBTREE;
            }
        }
        return result;
    }

    private boolean isInterface(ASTNode node) {
        return node instanceof TypeDeclaration
                && ((TypeDeclaration) node).isInterface();
    }

    @Override
    public boolean visit(MethodDeclaration node) {
        if (isInterface(node.getParent())) {
            // remove modifiers implied by the context
            return removePublicAbstractModifiers(node);
        }
        return ensureModifiersOrder(node);
    }

    private boolean removePublicAbstractModifiers(BodyDeclaration node) {
        boolean result = VISIT_SUBTREE;
        for (Modifier m : getModifiersOnly(modifiers(node))) {
            if (m.isPublic() || m.isAbstract()) {
                ctx.getRefactorings().remove(m);
                result = DO_NOT_VISIT_SUBTREE;
            }
        }
        return result;
    }

    @Override
    public boolean visit(AnnotationTypeDeclaration node) {
        return ensureModifiersOrder(node);
    }

    @Override
    public boolean visit(AnnotationTypeMemberDeclaration node) {
        return removePublicAbstractModifiers(node);
    }

    @Override
    public boolean visit(EnumDeclaration node) {
        return removeStaticModifier(modifiers(node)) | ensureModifiersOrder(node);
    }

    @Override
    public boolean visit(TryStatement node) {
        boolean result = VISIT_SUBTREE;
        for (VariableDeclarationExpression resource : resources(node)) {
            result &= removeFinalModifier(modifiers(resource));
        }
        return result;
    }

    @Override
    public boolean visit(TypeDeclaration node) {
        return (isInterface(node) && removeStaticModifier(modifiers(node)))
            | ensureModifiersOrder(node);
    }

    @SuppressWarnings("unchecked")
    private boolean ensureModifiersOrder(BodyDeclaration node) {
        boolean result = VISIT_SUBTREE;
        final List<Modifier> modifiers = getModifiersOnly(modifiers(node));
        final List<Modifier> reorderedModifiers = new ArrayList<Modifier>(modifiers);
        Collections.sort(reorderedModifiers, new ModifierOrderComparator());
        if (!modifiers.equals(reorderedModifiers)) {
            final int startSize = getStartSize(node.modifiers(), modifiers);
            for (int i = startSize; i < reorderedModifiers.size(); i++) {
                moveToIndex(reorderedModifiers.get(i), i);
                result = DO_NOT_VISIT_SUBTREE;
            }
        }
        return result;
    }

    private boolean removeStaticModifier(List<IExtendedModifier> modifiers) {
        boolean result = VISIT_SUBTREE;
        for (Modifier m : getModifiersOnly(modifiers)) {
            if (m.isStatic()) {
                ctx.getRefactorings().remove(m);
                result = DO_NOT_VISIT_SUBTREE;
            }
        }
        return result;
    }

    private <T> int getStartSize(List<T> initialList, final List<T> filteredList) {
        final List<T> l = new ArrayList<T>(initialList);
        l.removeAll(filteredList);
        return l.size();
    }

    private void moveToIndex(Modifier m, int index) {
        final ASTBuilder b = ctx.getASTBuilder();
        ctx.getRefactorings().moveToIndex(m, index, b.move(m));
    }

    @Override
    public boolean visit(SingleVariableDeclaration node) {
        if (isInterface(node.getParent().getParent())) {
            return removeFinalModifier(modifiers(node));
        }
        return VISIT_SUBTREE;
    }

    private boolean removeFinalModifier(List<IExtendedModifier> modifiers) {
        boolean result = VISIT_SUBTREE;
        for (Modifier m : getModifiersOnly(modifiers)) {
            if (m.isFinal()) {
                ctx.getRefactorings().remove(m);
                result = DO_NOT_VISIT_SUBTREE;
            }
        }
        return result;
    }

    private List<Modifier> getModifiersOnly(Collection<IExtendedModifier> modifiers) {
        final List<Modifier> results = new LinkedList<Modifier>();
        for (IExtendedModifier em : modifiers) {
            if (em.isModifier()) {
                results.add((Modifier) em);
            }
        }
        return results;
    }
}

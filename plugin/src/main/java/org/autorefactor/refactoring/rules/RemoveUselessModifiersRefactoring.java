/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2018 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2018 Andrei Paikin - Remove protected modifier for final class not inherited members.
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
import static org.autorefactor.refactoring.ASTHelper.resources;
import static org.autorefactor.refactoring.ASTHelper.getModifiersOnly;
import static org.eclipse.jdt.core.dom.Modifier.isFinal;
import static org.eclipse.jdt.core.dom.Modifier.isPrivate;
import static org.eclipse.jdt.core.dom.Modifier.isProtected;

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.ASTHelper;
import org.eclipse.jdt.core.dom.Annotation;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.AnnotationTypeDeclaration;
import org.eclipse.jdt.core.dom.AnnotationTypeMemberDeclaration;
import org.eclipse.jdt.core.dom.BodyDeclaration;
import org.eclipse.jdt.core.dom.EnumDeclaration;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.IExtendedModifier;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;

/** See {@link #getDescription()} method. */
public class RemoveUselessModifiersRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Remove useless modifiers";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
                + "Sorts modifiers.\n"
                + "Also removes modifiers implied by the context:\n"
                + "- \"static\" and \"abstract\" for interface,\n"
                + "- \"public\", \"static\" and \"final\" for interface fields,\n"
                + "- \"public\" and \"abstract\" for interface methods,\n"
                + "- \"final\" for private methods,\n"
                + "- \"final\" for parameters in interface method declarations,"
                + "- \"protected\" modifier for final class not inherited members.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces code to focus attention on code that matters.";
    }

    @Override
    public boolean visit(FieldDeclaration node) {
        if (isInterface(node.getParent())) {
            return removePublicStaticFinalModifiers(node);
        }
        if (isProtected(node.getModifiers())
                && isFinalClass(node.getParent())) {
            return removeProtectedModifier(node);
        }
        return ensureModifiersOrder(node);
    }

    private boolean removePublicStaticFinalModifiers(FieldDeclaration node) {
        // Remove modifiers implied by the context
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

    private boolean isFinalClass(ASTNode node) {
        return node instanceof TypeDeclaration
                && isFinal(((TypeDeclaration) node).getModifiers());
    }

    @Override
    public boolean visit(MethodDeclaration node) {
        if (isInterface(node.getParent())) {
            // Remove modifiers implied by the context
            return removePublicAbstractModifiers(node);
        }
        int modifiers = node.getModifiers();
        if (isFinal(modifiers) && (isFinalClass(node.getParent()) || isPrivate(modifiers))) {
            return removeFinalModifier(modifiers(node));
        }
        if (isProtected(node.getModifiers())
                && (node.isConstructor()
                        ? isFinalClass(node.getParent()) : isFinalClassWithoutInheritance(node.getParent()))) {
            return removeProtectedModifier(node);
        }
        return ensureModifiersOrder(node);
    }

    private boolean isFinalClassWithoutInheritance(ASTNode node) {
        if (node instanceof TypeDeclaration) {
            TypeDeclaration clazz = (TypeDeclaration) node;
            return isFinalClass(clazz)
                    && clazz.superInterfaceTypes().isEmpty()
                    && (clazz.getSuperclassType() == null
                            || hasType(clazz.getSuperclassType().resolveBinding(), "java.lang.Object"));
        }

        return false;
    }

    private boolean removeProtectedModifier(BodyDeclaration node) {
        for (Modifier modifier : getModifiersOnly(modifiers(node))) {
            if (modifier.isProtected()) {
                ctx.getRefactorings().remove(modifier);
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
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
        if (removeStaticAbstractModifier(modifiers(node)) == DO_NOT_VISIT_SUBTREE) {
            return DO_NOT_VISIT_SUBTREE;
        }

        return ensureModifiersOrder(node);
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
        if (isInterface(node) && removeStaticAbstractModifier(modifiers(node)) == DO_NOT_VISIT_SUBTREE) {
            return DO_NOT_VISIT_SUBTREE;
        }

        return ensureModifiersOrder(node);
    }

    private boolean ensureModifiersOrder(BodyDeclaration node) {
        final List<IExtendedModifier> extendedModifiers = modifiers(node);
        final List<IExtendedModifier> reorderedModifiers = ASTHelper.reorder(extendedModifiers);

        if (!extendedModifiers.equals(reorderedModifiers)) {
            reorderModifiers(reorderedModifiers);
            return DO_NOT_VISIT_SUBTREE;
        }

        return VISIT_SUBTREE;
    }

    private void reorderModifiers(final List<IExtendedModifier> reorderedModifiers) {
        final ASTBuilder b = ctx.getASTBuilder();

        for (int i = 0; i < reorderedModifiers.size(); i++) {
            IExtendedModifier m = reorderedModifiers.get(i);
            if (m.isModifier()) {
                ctx.getRefactorings().moveToIndex((Modifier) m, i, b.move((Modifier) m));
            } else {
                ctx.getRefactorings().moveToIndex((Annotation) m, i, b.move((Annotation) m));
            }
        }
    }

    private boolean removeStaticAbstractModifier(List<IExtendedModifier> modifiers) {
        boolean result = VISIT_SUBTREE;
        for (Modifier m : getModifiersOnly(modifiers)) {
            if (m.isStatic() || m.isAbstract()) {
                ctx.getRefactorings().remove(m);
                result = DO_NOT_VISIT_SUBTREE;
            }
        }
        return result;
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

}

/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Zsombor Gegesy - initial API and implementation
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
import static org.autorefactor.refactoring.ASTHelper.getModifiersOnly;
import static org.autorefactor.refactoring.ASTHelper.modifiers;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.ASTHelper;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.IExtendedModifier;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.SimpleType;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

/** See {@link #getDescription()} method. */
public class EnforceStaticFinalLoggerRefactoring extends AbstractRefactoringRule {

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Enforce logger fields to be private (or protected) static final";
    }

    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "EnforceStaticFinalLogger";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "Loggers should be final, static and preferably private variables in their class, to not waste memory.";
    }

    @Override
    public boolean visit(FieldDeclaration node) {
        final Type fieldType = node.getType();
        if (!isLogger(fieldType)) {
            return VISIT_SUBTREE;
        }
        for (VariableDeclarationFragment var : ASTHelper.fragments(node)) {
            if (var.getInitializer() == null) {
                // there is no initializer for the field, it can't become a
                // private static final field
                return VISIT_SUBTREE;
            }
        }
        return markFinalStatic(node);
    }

    private boolean markFinalStatic(FieldDeclaration node) {
        boolean result = VISIT_SUBTREE;
        boolean hasFinal = false;
        boolean hasAccess = false;
        boolean hasStatic = false;
        List<IExtendedModifier> modifiers = modifiers(node);
        int size = modifiers.size();
        List<Modifier> modifiersOnly = getModifiersOnly(modifiers);
        for (Modifier modifier : modifiersOnly) {
            if (modifier.isFinal()) {
                hasFinal = true;
            } else if (modifier.isStatic()) {
                hasStatic = true;
            } else if (modifier.isPrivate() || modifier.isProtected()) {
                hasAccess = true;
            } else if (modifier.isPublic()) {
                ctx.getRefactorings().remove(modifier);
                size--;
                result = DO_NOT_VISIT_SUBTREE;
            }

        }
        ASTBuilder builder = ctx.getASTBuilder();
        if (!hasAccess) {
            // insert at first
            ctx.getRefactorings().insertAt(node, FieldDeclaration.MODIFIERS2_PROPERTY, builder.private0(), 0);
            size++;
            result = DO_NOT_VISIT_SUBTREE;
        }
        if (!hasStatic) {
            ctx.getRefactorings().insertAt(node, FieldDeclaration.MODIFIERS2_PROPERTY, builder.static0(), size);
            size++;
            result = DO_NOT_VISIT_SUBTREE;
        }
        if (!hasFinal) {
            ctx.getRefactorings().insertAt(node, FieldDeclaration.MODIFIERS2_PROPERTY, builder.final0(), size);
            size++;
            result = DO_NOT_VISIT_SUBTREE;
        }
        return result;
    }

    private boolean isLogger(Type fieldType) {
        ITypeBinding typeBinding = fieldType.resolveBinding();
        if (typeBinding != null) {
            // if we have type bindings, we can use that...
            return LOGGER_TYPES.contains(typeBinding.getQualifiedName());
        }
        if (fieldType instanceof SimpleType) {
            SimpleType st = (SimpleType) fieldType;
            String qName = st.getName().getFullyQualifiedName();
            return LOGGER_TYPES.contains(qName);
        }
        return false;
    }

    private final static Set<String> LOGGER_TYPES = new HashSet<String>(Arrays.asList(
            "java.util.logging.Logger", // java internal logging.
            "org.apache.log4j.Logger", // Log4j 1.x
            "org.apache.logging.log4j.Logger", // Log4j 2.x
            "org.apache.commons.logging.Log", // apache commons logging
            "org.slf4j.Logger" // slf4j
    ));

}

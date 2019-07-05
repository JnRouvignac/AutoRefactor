/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Andrei Paikin - Initial API and implementation
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
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.arguments;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.instanceOf;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.internal.corext.dom.ASTBuilder;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.TypeLiteral;

/**
 * Replaces HashSet for enum type creation to EnumSet factory static methods.
 */
public final class EnumSetRatherThanHashSetCleanUp extends
        AbstractEnumCollectionReplacementCleanUp {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "EnumSet rather than HashSet for enum types";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Converts creation of HashSet with enum as a type "
                + "to invocation of static methods of EnumSet where possible";
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
    public String getImplType() {
        return "java.util.HashSet";
    }

    @Override
    public String getInterfaceType() {
        return "java.util.Set";
    }

    @Override
    public Set<String> getClassesToImport() {
        return new HashSet<String>(Arrays.asList("java.util.EnumSet"));
    }

    /**
     * Refactoring is not correct if argument for HashSet constructor is a Collection, but other
     * than EnumSet. <br>
     * In case of empty collection <code>EnumSet.copyOf</code> will throw an
     * <code>IllegalArgumentException</code>, <br>
     * and HashSet(Collection) will not. <br>
     * <br>
     * Other constructors can be replaced with <code>EnumSet.noneOf(Class)</code> method. <br>
     * <br>
     * @param cic
     *            - class instance creation node to be replaced
     * @param type
     *            - type argument of the declaration
     * @see java.util.EnumSet#noneOf(Class) <br>
     */
    @Override
    boolean maybeReplace(ClassInstanceCreation cic, Set<String> alreadyImportedClasses,
            Set<String> importsToAdd, Type... types) {
        if (types == null || types.length < 1) {
            return VISIT_SUBTREE;
        }

        Type type = types[0];
        ASTBuilder b = ctx.getASTBuilder();
        List<Expression> arguments = arguments(cic);
        final MethodInvocation invocation;

        if (!arguments.isEmpty()
                && instanceOf(arguments.get(0), "java.util.Collection")) {
            Expression typeArg = arguments.get(0);
            if (!instanceOf(typeArg, "java.util.EnumSet")) {
                return VISIT_SUBTREE;
            }
            invocation = b.invoke(alreadyImportedClasses.contains("java.util.EnumSet") ? b.name("EnumSet")
                    : b.name("java", "util", "EnumSet"),
                    "copyOf", b.copy(typeArg));
        } else {
            TypeLiteral newTypeLiteral = ctx.getAST().newTypeLiteral();
            newTypeLiteral.setType(b.copy(type));
            invocation = b.invoke(alreadyImportedClasses.contains("java.util.EnumSet") ? b.name("EnumSet")
                    : b.name("java", "util", "EnumSet"),
                    "noneOf", newTypeLiteral);
        }

        ctx.getRefactorings().replace(cic, invocation);
        importsToAdd.add("java.util.EnumSet");
        return DO_NOT_VISIT_SUBTREE;
    }
}
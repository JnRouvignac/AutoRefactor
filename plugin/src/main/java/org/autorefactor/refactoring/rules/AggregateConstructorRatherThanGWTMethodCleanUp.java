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
package org.autorefactor.refactoring.rules;

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.hasType;
import static org.autorefactor.refactoring.ASTHelper.isMethod;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.Release;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Type;

/** See {@link #getDescription()} method. */
public class AggregateConstructorRatherThanGWTMethodCleanUp extends NewClassImportCleanUp {
    private final class RefactoringWithArrayListOrHashMapClass extends CleanUpWithNewClassImport {

        @Override
        public boolean visit(final MethodInvocation node) {
            final boolean isSubTreeToVisit =
                    AggregateConstructorRatherThanGWTMethodCleanUp.this.maybeRefactorMethodInvocation(node,
                            getClassesToUseWithImport(), getImportsToAdd());

            return isSubTreeToVisit;
        }
    }

    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Aggregate constructor rather than GWT method";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Use new ArrayList<>() and new HashMap<>() instead of"
                + " using specific GWT Lists.newArrayList() and Maps.newHashMap().";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "With diamond operator in Java 7, those specific GWT methods are useless."
                + " It also reduces the dependency to libraries.";
    }

    @Override
    public boolean isJavaVersionSupported(Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 7;
    }

    @Override
    public Set<String> getClassesToImport() {
        return new HashSet<String>(Arrays.asList(
                "java.util.ArrayList",
                "java.util.LinkedList",
                "java.util.HashMap",
                "java.util.TreeMap",
                "java.util.LinkedHashMap",
                "java.util.IdentityHashMap",
                "java.util.EnumMap"));
    }

    @Override
    public CleanUpWithNewClassImport getRefactoringClassInstance() {
        return new RefactoringWithArrayListOrHashMapClass();
    }

    @Override
    public boolean visit(final MethodInvocation node) {
        return maybeRefactorMethodInvocation(node, getAlreadyImportedClasses(node), new HashSet<String>());
    }

    private boolean maybeRefactorMethodInvocation(final MethodInvocation node,
            final Set<String> classesToUseWithImport,
            final Set<String> importsToAdd) {
        if (node.arguments().isEmpty()) {
            return maybeRefactor(node, classesToUseWithImport, importsToAdd, "Lists", "ArrayList")
                    && maybeRefactor(node, classesToUseWithImport, importsToAdd, "Lists", "LinkedList")
                    && maybeRefactor(node, classesToUseWithImport, importsToAdd, "Maps", "HashMap")
                    && maybeRefactor(node, classesToUseWithImport, importsToAdd, "Maps", "TreeMap")
                    && maybeRefactor(node, classesToUseWithImport, importsToAdd, "Maps", "LinkedHashMap")
                    && maybeRefactor(node, classesToUseWithImport, importsToAdd, "Maps", "IdentityHashMap");
        }

        if (node.arguments().size() == 1) {
            final Expression arg = (Expression) node.arguments().get(0);

            if (!hasType(arg, "java.lang.Class")) {
                return VISIT_SUBTREE;
            }

            final ITypeBinding argType = arg.resolveTypeBinding();
            String generic = "";

            if (argType != null) {
                final ITypeBinding[] typeArgs = argType.getTypeArguments();

                if (typeArgs != null) {
                    if (typeArgs.length != 1) {
                        return VISIT_SUBTREE;
                    }

                    final ITypeBinding typeParam = typeArgs[0];

                    if (!typeParam.isEnum()) {
                        return VISIT_SUBTREE;
                    }
                    generic = "<" + typeParam.getQualifiedName() + ">";
                }
            }

            if (isMethod(node, "com.google.common.collect.Maps", "newEnumMap", "java.lang.Class" + generic)
                    || isMethod(node, "com.google.gwt.thirdparty.guava.common.collect.Maps", "newEnumMap",
                            "java.lang.Class" + generic)) {
                final ASTBuilder b = this.ctx.getASTBuilder();
                final Refactorings r = this.ctx.getRefactorings();

                final Type type = b.getAST().newParameterizedType(b.type(classesToUseWithImport
                        .contains("java.util.EnumMap") ? "EnumMap" : "java.util.EnumMap"));
                r.replace(node, b.new0(type, b.copy(arg)));
                importsToAdd.add("java.util.EnumMap");
                return DO_NOT_VISIT_SUBTREE;
            }
        }

        return VISIT_SUBTREE;
    }

    private boolean maybeRefactor(final MethodInvocation node, final Set<String> classesToUseWithImport,
            final Set<String> importsToAdd, final String aggregateInterface,
            final String implClass) {
        if (isMethod(node, "com.google.common.collect." + aggregateInterface, "new" + implClass)
                || isMethod(node, "com.google.gwt.thirdparty.guava.common.collect." + aggregateInterface,
                        "new" + implClass)) {
            final ASTBuilder b = this.ctx.getASTBuilder();
            final Refactorings r = this.ctx.getRefactorings();

            Type type = b.getAST().newParameterizedType(b.type(classesToUseWithImport
                    .contains("java.util." + implClass) ? implClass : "java.util." + implClass));
            r.replace(node, b.new0(type));
            importsToAdd.add("java.util." + implClass);
            return DO_NOT_VISIT_SUBTREE;
        }

        return VISIT_SUBTREE;
    }
}

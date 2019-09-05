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
import java.util.Arrays;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Set;
import java.util.TreeMap;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Type;

/** See {@link #getDescription()} method. */
public class AggregateConstructorRatherThanGWTMethodCleanUp extends NewClassImportCleanUp {
    private final class RefactoringWithArrayListOrHashMapClass extends CleanUpWithNewClassImport {
        @Override
        public boolean visit(final MethodInvocation node) {
            return AggregateConstructorRatherThanGWTMethodCleanUp.this
                    .maybeRefactorMethodInvocation(node, getClassesToUseWithImport(), getImportsToAdd());
        }
    }

    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_AggregateConstructorRatherThanGWTMethodCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_AggregateConstructorRatherThanGWTMethodCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_AggregateConstructorRatherThanGWTMethodCleanUp_reason;
    }

    @Override
    public boolean isJavaVersionSupported(Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 7;
    }

    @Override
    public Set<String> getClassesToImport() {
        return new HashSet<>(Arrays.asList(ArrayList.class.getCanonicalName(), LinkedList.class.getCanonicalName(), HashMap.class.getCanonicalName(),
                TreeMap.class.getCanonicalName(), LinkedHashMap.class.getCanonicalName(), IdentityHashMap.class.getCanonicalName(), EnumMap.class.getCanonicalName()));
    }

    @Override
    public CleanUpWithNewClassImport getRefactoringClassInstance() {
        return new RefactoringWithArrayListOrHashMapClass();
    }

    @Override
    public boolean visit(final MethodInvocation node) {
        return maybeRefactorMethodInvocation(node, getAlreadyImportedClasses(node), new HashSet<String>());
    }

    private boolean maybeRefactorMethodInvocation(final MethodInvocation node, final Set<String> classesToUseWithImport,
            final Set<String> importsToAdd) {
        if (node.arguments().isEmpty()) {
            return maybeRefactor(node, classesToUseWithImport, importsToAdd, "Lists", "ArrayList") //$NON-NLS-1$ $NON-NLS-2$
                    && maybeRefactor(node, classesToUseWithImport, importsToAdd, "Lists", "LinkedList") //$NON-NLS-1$ $NON-NLS-2$
                    && maybeRefactor(node, classesToUseWithImport, importsToAdd, "Maps", "HashMap") //$NON-NLS-1$ $NON-NLS-2$
                    && maybeRefactor(node, classesToUseWithImport, importsToAdd, "Maps", "TreeMap") //$NON-NLS-1$ $NON-NLS-2$
                    && maybeRefactor(node, classesToUseWithImport, importsToAdd, "Maps", "LinkedHashMap") //$NON-NLS-1$ $NON-NLS-2$
                    && maybeRefactor(node, classesToUseWithImport, importsToAdd, "Maps", "IdentityHashMap"); //$NON-NLS-1$ $NON-NLS-2$
        }

        if (node.arguments().size() == 1) {
            final Expression arg= (Expression) node.arguments().get(0);

            if (!ASTNodes.hasType(arg, Class.class.getCanonicalName())) {
                return true;
            }

            final ITypeBinding argType= arg.resolveTypeBinding();
            String generic= ""; //$NON-NLS-1$

            if (argType != null) {
                final ITypeBinding[] typeArgs= argType.getTypeArguments();

                if (typeArgs != null) {
                    if (typeArgs.length != 1) {
                        return true;
                    }

                    final ITypeBinding typeParam= typeArgs[0];

                    if (!typeParam.isEnum()) {
                        return true;
                    }
                    generic= "<" + typeParam.getQualifiedName() + ">"; //$NON-NLS-1$ $NON-NLS-2$
                }
            }

            if (ASTNodes.usesGivenSignature(node, "com.google.common.collect.Maps", "newEnumMap", Class.class.getCanonicalName() + generic) //$NON-NLS-1$ $NON-NLS-2$
                    || ASTNodes.usesGivenSignature(node, "com.google.gwt.thirdparty.guava.common.collect.Maps", "newEnumMap", //$NON-NLS-1$ $NON-NLS-2$
                            Class.class.getCanonicalName() + generic)) {
                final ASTNodeFactory b= this.ctx.getASTBuilder();
                final Refactorings r= this.ctx.getRefactorings();

                final Type type= b.getAST().newParameterizedType(
                        b.type(classesToUseWithImport.contains(EnumMap.class.getCanonicalName()) ? "EnumMap" : EnumMap.class.getCanonicalName())); //$NON-NLS-1$
                r.replace(node, b.new0(type, b.copy(arg)));
                importsToAdd.add(EnumMap.class.getCanonicalName());
                return false;
            }
        }

        return true;
    }

    private boolean maybeRefactor(final MethodInvocation node, final Set<String> classesToUseWithImport,
            final Set<String> importsToAdd, final String aggregateInterface, final String implClass) {
        if (ASTNodes.usesGivenSignature(node, "com.google.common.collect." + aggregateInterface, "new" + implClass) || ASTNodes.usesGivenSignature(node, //$NON-NLS-1$ $NON-NLS-2$
                "com.google.gwt.thirdparty.guava.common.collect." + aggregateInterface, "new" + implClass)) { //$NON-NLS-1$ $NON-NLS-2$
            final ASTNodeFactory b= this.ctx.getASTBuilder();
            final Refactorings r= this.ctx.getRefactorings();

            Type type= b.getAST().newParameterizedType(b.type(
                    classesToUseWithImport.contains("java.util." + implClass) ? implClass : "java.util." + implClass)); //$NON-NLS-1$ $NON-NLS-2$
            r.replace(node, b.new0(type));
            importsToAdd.add("java.util." + implClass); //$NON-NLS-1$
            return false;
        }

        return true;
    }
}

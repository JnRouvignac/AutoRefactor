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
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.LambdaExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;

/** See {@link #getDescription()} method. */
public class StandardMethodRatherThanLibraryMethodCleanUp extends NewClassImportCleanUp {
    private final class RefactoringWithObjectsClass extends CleanUpWithNewClassImport {
        @Override
        public boolean visit(final MethodInvocation node) {
            return StandardMethodRatherThanLibraryMethodCleanUp.this.maybeRefactorMethodInvocation(node,
                    getClassesToUseWithImport(), getImportsToAdd());
        }
    }

    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_StandardMethodRatherThanLibraryMethodCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_StandardMethodRatherThanLibraryMethodCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_StandardMethodRatherThanLibraryMethodCleanUp_reason;
    }

    @Override
    public boolean isJavaVersionSupported(final Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 7;
    }

    @Override
    public Set<String> getClassesToImport() {
        return new HashSet<>(Arrays.asList(Objects.class.getCanonicalName()));
    }

    @Override
    public CleanUpWithNewClassImport getRefactoringClassInstance() {
        return new RefactoringWithObjectsClass();
    }

    @Override
    public boolean visit(final MethodInvocation node) {
        return maybeRefactorMethodInvocation(node, getAlreadyImportedClasses(node), new HashSet<String>());
    }

    private boolean maybeRefactorMethodInvocation(final MethodInvocation node, final Set<String> classesToUseWithImport,
            final Set<String> importsToAdd) {
        ASTNodeFactory b= this.cuRewrite.getASTBuilder();

        Name javaUtilObjects= b.name(classesToUseWithImport.contains(Objects.class.getCanonicalName()) ? Objects.class.getSimpleName() : Objects.class.getCanonicalName());

        if (ASTNodes.usesGivenSignature(node, "org.apache.commons.lang3.ObjectUtils", "hashCode", Object.class.getCanonicalName()) //$NON-NLS-1$ //$NON-NLS-2$
                || ASTNodes.usesGivenSignature(node, "org.apache.commons.lang3.ObjectUtils", "equals", Object.class.getCanonicalName(), //$NON-NLS-1$ //$NON-NLS-2$
                        Object.class.getCanonicalName())
                || ASTNodes.usesGivenSignature(node, "org.apache.commons.lang3.ObjectUtils", "toString", Object.class.getCanonicalName(), //$NON-NLS-1$ //$NON-NLS-2$
                        String.class.getCanonicalName())) {
            replaceUtilClass(node, importsToAdd, javaUtilObjects);
            return false;
        }

        if (ASTNodes.usesGivenSignature(node, "com.google.common.base.Objects", "equal", Object.class.getCanonicalName(), Object.class.getCanonicalName()) //$NON-NLS-1$ //$NON-NLS-2$
                || ASTNodes.usesGivenSignature(node, "com.google.gwt.thirdparty.guava.common.base.Objects", "equal", Object.class.getCanonicalName(), //$NON-NLS-1$ //$NON-NLS-2$
                        Object.class.getCanonicalName())) {
            Refactorings r= this.cuRewrite.getRefactorings();

            r.replace(node, b.invoke(javaUtilObjects, "equals", b.createMoveTarget((Expression) node.arguments().get(0)), //$NON-NLS-1$
                    b.createMoveTarget((Expression) node.arguments().get(1))));
            importsToAdd.add(Objects.class.getCanonicalName());
            return false;
        }

        if (ASTNodes.usesGivenSignature(node, "org.apache.commons.lang3.ObjectUtils", "toString", Object.class.getCanonicalName())) { //$NON-NLS-1$ //$NON-NLS-2$
            Refactorings r= this.cuRewrite.getRefactorings();

            r.replace(node,
                    b.invoke(javaUtilObjects, "toString", b.createMoveTarget((Expression) node.arguments().get(0)), b.string(""))); //$NON-NLS-1$ //$NON-NLS-2$
            importsToAdd.add(Objects.class.getCanonicalName());
            return false;
        }

        if (ASTNodes.usesGivenSignature(node, "com.google.common.base.Objects", "hashCode", Object[].class.getCanonicalName()) || ASTNodes.usesGivenSignature(node, //$NON-NLS-1$ //$NON-NLS-2$
                "com.google.gwt.thirdparty.guava.common.base.Objects", "hashCode", Object[].class.getCanonicalName())) { //$NON-NLS-1$ //$NON-NLS-2$
            Refactorings r= this.cuRewrite.getRefactorings();

            r.replace(node, b.invoke(javaUtilObjects, "hash", copyArguments(b, node))); //$NON-NLS-1$
            importsToAdd.add(Objects.class.getCanonicalName());
            return false;
        }

        if (ASTNodes.usesGivenSignature(node, "org.apache.commons.lang3.ObjectUtils", "hashCodeMulti", Object[].class.getCanonicalName())) { //$NON-NLS-1$ //$NON-NLS-2$
            Refactorings r= this.cuRewrite.getRefactorings();

            if (node.getExpression() != null) {
                r.replace(node.getExpression(), javaUtilObjects);
                r.replace(node.getName(), b.simpleName("hash")); //$NON-NLS-1$
            } else {
                r.replace(node, b.invoke(javaUtilObjects, "hash", copyArguments(b, node))); //$NON-NLS-1$
            }

            importsToAdd.add(Objects.class.getCanonicalName());
            return false;
        }

        if (ASTNodes.usesGivenSignature(node, "com.google.common.base.Preconditions", "checkNotNull", "T") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                || ASTNodes.usesGivenSignature(node, "com.google.common.base.Preconditions", "checkNotNull", "T", Object.class.getCanonicalName()) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                || ASTNodes.usesGivenSignature(node, "com.google.gwt.thirdparty.guava.common.base.Preconditions", "checkNotNull", "T") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                || ASTNodes.usesGivenSignature(node, "com.google.gwt.thirdparty.guava.common.base.Preconditions", "checkNotNull", "T", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                        Object.class.getCanonicalName())
                || ASTNodes.usesGivenSignature(node, "org.apache.commons.lang3.Validate", "notNull", "T") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                || ASTNodes.usesGivenSignature(node, "org.apache.commons.lang3.Validate", "notNull", "T", String.class.getCanonicalName(), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                        Object[].class.getCanonicalName())) {
            Refactorings r= this.cuRewrite.getRefactorings();

            List<Expression> copyOfArgs= copyArguments(b, node);

            if (copyOfArgs.size() <= 2) {
                r.replace(node, b.invoke(javaUtilObjects, "requireNonNull", copyOfArgs)); //$NON-NLS-1$
            } else if (cuRewrite.getJavaProjectOptions().getJavaSERelease().getMinorVersion() >= 8) {
                LambdaExpression messageSupplier= b.lambda();
                messageSupplier
                        .setBody(b.invoke(b.simpleName(String.class.getSimpleName()), "format", copyOfArgs.subList(1, copyOfArgs.size()))); //$NON-NLS-1$
                r.replace(node, b.invoke(javaUtilObjects, "requireNonNull", copyOfArgs.get(0), messageSupplier)); //$NON-NLS-1$
            } else {
                return true;
            }
            importsToAdd.add(Objects.class.getCanonicalName());
            return false;
        }

        return true;
    }

    private List<Expression> copyArguments(final ASTNodeFactory b, final MethodInvocation node) {
        List<Expression> copyOfArgs= new ArrayList<>(node.arguments().size());

        for (Object expression : node.arguments()) {
            copyOfArgs.add(b.createMoveTarget((Expression) expression));
        }

        return copyOfArgs;
    }

    private void replaceUtilClass(final MethodInvocation node, final Set<String> importsToAdd,
            final Name javaUtilObjects) {
        Refactorings r= this.cuRewrite.getRefactorings();

        r.replace(node.getExpression(), javaUtilObjects);
        importsToAdd.add(Objects.class.getCanonicalName());
    }
}

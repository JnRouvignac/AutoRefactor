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
import static org.autorefactor.refactoring.ASTHelper.isMethod;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.Release;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.MethodInvocation;

/** See {@link #getDescription()} method. */
public class StandardMethodRatherThanLibraryMethodRefactoring extends NewClassImportRefactoring {
    private final class RefactoringWithObjectsClass extends RefactoringWithNewClassImport {
        public RefactoringWithObjectsClass(final RefactoringContext context) {
            ctx = context;
        }

        @Override
        public boolean visit(final MethodInvocation node) {
            return StandardMethodRatherThanLibraryMethodRefactoring.this.maybeRefactorMethodInvocation(node,
                    getClassesToUseWithImport(), getImportsToAdd());
        }
    }

    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Standard method rather than Library method";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Stop using ObjectUtils.equals(), ObjectUtils.hashCode(), ObjectUtils.hashCodeMulti()"
                + " and ObjectUtils.toString() to use java.util.Objects methods instead.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "Those specific Apache and Google methods are deprecated."
                + " It also reduces the dependency to libraries.";
    }

    @Override
    public boolean isJavaVersionSupported(final Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 7;
    }

    @Override
    public Set<String> getClassesToImport() {
        return new HashSet<String>(Arrays.asList(
                "java.util.Objects"));
    }

    @Override
    public RefactoringWithNewClassImport getRefactoringClassInstance() {
        return new RefactoringWithObjectsClass(ctx);
    }

    @Override
    public boolean visit(final MethodInvocation node) {
        return maybeRefactorMethodInvocation(node, getAlreadyImportedClasses(node), new HashSet<String>());
    }

    private boolean maybeRefactorMethodInvocation(final MethodInvocation node,
            final Set<String> classesToUseWithImport,
            final Set<String> importsToAdd) {
        if (isMethod(node, "org.apache.commons.lang3.ObjectUtils", "hashCode", "java.lang.Object")
                || isMethod(node, "org.apache.commons.lang3.ObjectUtils", "equals", "java.lang.Object",
                        "java.lang.Object")
                || isMethod(node, "org.apache.commons.lang3.ObjectUtils", "toString", "java.lang.Object",
                        "java.lang.String")) {
            replaceUtilClass(node, classesToUseWithImport, importsToAdd);
            return DO_NOT_VISIT_SUBTREE;
        }

        if (isMethod(node, "com.google.common.base.Objects", "equal", "java.lang.Object", "java.lang.Object")
                || isMethod(node, "com.google.gwt.thirdparty.guava.common.base.Objects", "equal", "java.lang.Object",
                        "java.lang.Object")) {
            final ASTBuilder b = this.ctx.getASTBuilder();
            final Refactorings r = this.ctx.getRefactorings();

            r.replace(node, b.invoke(classesToUseWithImport
                    .contains("java.util.Objects") ? b.simpleName("Objects") : b.name("java", "util", "Objects"),
                            "equals",
                            b.copy((Expression) node.arguments().get(0)),
                            b.copy((Expression) node.arguments().get(1))));
            importsToAdd.add("java.util.Objects");
            return DO_NOT_VISIT_SUBTREE;
        }

        if (isMethod(node, "org.apache.commons.lang3.ObjectUtils", "toString", "java.lang.Object")) {
            final ASTBuilder b = this.ctx.getASTBuilder();
            final Refactorings r = this.ctx.getRefactorings();

            r.replace(node, b.invoke(classesToUseWithImport
                    .contains("java.util.Objects") ? b.simpleName("Objects") : b.name("java", "util", "Objects"),
                            "toString",
                            b.copy((Expression) node.arguments().get(0)),
                            b.string("")));
            importsToAdd.add("java.util.Objects");
            return DO_NOT_VISIT_SUBTREE;
        }

        if (isMethod(node, "com.google.common.base.Objects", "hashCode", "java.lang.Object[]")
                || isMethod(node, "com.google.gwt.thirdparty.guava.common.base.Objects", "hashCode",
                        "java.lang.Object[]")) {
            final ASTBuilder b = this.ctx.getASTBuilder();
            final Refactorings r = this.ctx.getRefactorings();

            final List<Expression> copyOfArgs = new ArrayList<Expression>(node.arguments().size());

            for (Object expression : node.arguments()) {
                copyOfArgs.add(b.copy((Expression) expression));
            }

            r.replace(node, b.invoke(classesToUseWithImport
                    .contains("java.util.Objects") ? b.simpleName("Objects") : b.name("java", "util", "Objects"),
                            "hash",
                            copyOfArgs.toArray(new Expression[copyOfArgs.size()])));
            importsToAdd.add("java.util.Objects");
            return DO_NOT_VISIT_SUBTREE;
        }

        if (isMethod(node, "org.apache.commons.lang3.ObjectUtils", "hashCodeMulti", "java.lang.Object[]")) {
            final ASTBuilder b = this.ctx.getASTBuilder();
            final Refactorings r = this.ctx.getRefactorings();

            if (node.getExpression() != null) {
                r.replace(node.getExpression(), classesToUseWithImport
                        .contains("java.util.Objects") ? b.simpleName("Objects") : b.name("java", "util", "Objects"));
                r.replace(node.getName(), b.simpleName("hash"));
            } else {
                final Expression[] copyOfArgs = copyArguments(b, node);

                r.replace(node, b.invoke(classesToUseWithImport
                        .contains("java.util.Objects") ? b.simpleName("Objects") : b.name("java", "util", "Objects"),
                                "hash",
                                copyOfArgs));
            }

            importsToAdd.add("java.util.Objects");
            return DO_NOT_VISIT_SUBTREE;
        }

        if (isMethod(node, "com.google.common.base.Preconditions", "checkNotNull", "T")
                || isMethod(node, "com.google.common.base.Preconditions", "checkNotNull", "T",
                        "java.lang.Object")) {
            final ASTBuilder b = this.ctx.getASTBuilder();
            final Refactorings r = this.ctx.getRefactorings();

            final Expression[] copyOfArgs = copyArguments(b, node);

            r.replace(node, b.invoke(classesToUseWithImport
                    .contains("java.util.Objects") ? b.simpleName("Objects") : b.name("java", "util", "Objects"),
                            "requireNonNull",
                            copyOfArgs));
            importsToAdd.add("java.util.Objects");
            return DO_NOT_VISIT_SUBTREE;
        }

        return VISIT_SUBTREE;
    }

    private Expression[] copyArguments(final ASTBuilder b, final MethodInvocation node) {
        final List<Expression> copyOfArgs = new ArrayList<Expression>(node.arguments().size());

        for (Object expression : node.arguments()) {
            copyOfArgs.add(b.copy((Expression) expression));
        }
        return copyOfArgs.toArray(new Expression[copyOfArgs.size()]);
    }

    private void replaceUtilClass(final MethodInvocation node, final Set<String> classesToUseWithImport,
            final Set<String> importsToAdd) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        final Refactorings r = this.ctx.getRefactorings();

        r.replace(node.getExpression(), classesToUseWithImport
                .contains("java.util.Objects") ? b.simpleName("Objects") : b.name("java", "util", "Objects"));
        importsToAdd.add("java.util.Objects");
    }
}

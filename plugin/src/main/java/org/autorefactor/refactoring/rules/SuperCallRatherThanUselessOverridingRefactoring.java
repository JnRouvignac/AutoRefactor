/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Split the code
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
import static org.autorefactor.refactoring.ASTHelper.asExpression;
import static org.autorefactor.refactoring.ASTHelper.hasType;
import static org.autorefactor.refactoring.ASTHelper.statements;
import static org.eclipse.jdt.core.search.IJavaSearchConstants.REFERENCES;
import static org.eclipse.jdt.core.search.SearchPattern.R_EXACT_MATCH;
import static org.eclipse.jdt.core.search.SearchPattern.createPattern;

import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

import org.autorefactor.util.UnhandledException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IAnnotationBinding;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.IPackageBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SuperMethodInvocation;
import org.eclipse.jdt.core.search.SearchEngine;
import org.eclipse.jdt.core.search.SearchMatch;
import org.eclipse.jdt.core.search.SearchParticipant;
import org.eclipse.jdt.core.search.SearchRequestor;

/**
 * Removes overriding of method if the overriding only call the super class.
 *
 * @see #getDescription()
 */
public class SuperCallRatherThanUselessOverridingRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Super call rather than useless overriding";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Removes overriding of method if the overriding only call the super class.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves the readibility of such simple code to spotlight the complexity of other code.";
    }

    @Override
    public boolean visit(final MethodDeclaration node) {
        if (node.getBody() == null) {
            return VISIT_SUBTREE;
        }

        final List<Statement> bodyStmts = statements(node.getBody());

        if (bodyStmts.size() == 1) {
            final SuperMethodInvocation bodyMi = asExpression(bodyStmts.get(0), SuperMethodInvocation.class);

            if (bodyMi != null) {
                final IMethodBinding bodyMethodBinding = bodyMi.resolveMethodBinding();
                final IMethodBinding declMethodBinding = node.resolveBinding();

                if (declMethodBinding != null
                        && bodyMethodBinding != null
                        && declMethodBinding.overrides(bodyMethodBinding)
                        && !hasSignificantAnnotations(declMethodBinding)
                        && haveSameModifiers(bodyMethodBinding, declMethodBinding)
                        && haveSameParameters(node, bodyMi)) {
                    if (Modifier.isProtected(declMethodBinding.getModifiers())
                            && !declaredInSamePackage(bodyMethodBinding, declMethodBinding)) {
                        // protected also means package visibility, so check if it is required
                        if (!isMethodUsedInItsPackage(declMethodBinding, node)) {
                            this.ctx.getRefactorings().remove(node);
                            return DO_NOT_VISIT_SUBTREE;
                        }
                    } else {
                        this.ctx.getRefactorings().remove(node);
                        return DO_NOT_VISIT_SUBTREE;
                    }
                }
            }
        }

        return VISIT_SUBTREE;
    }

    private boolean haveSameParameters(final MethodDeclaration node, final SuperMethodInvocation bodyMi) {
        final List<?> parameters = node.parameters();

        for (int i = 0; i < node.parameters().size(); i++) {
            final SingleVariableDeclaration paramName = (SingleVariableDeclaration) parameters.get(i);
            final Expression paramExpr = (Expression) bodyMi.arguments().get(i);

            if (!(paramExpr instanceof SimpleName)
                    || !paramName.getName().getIdentifier().equals(((SimpleName) paramExpr).getIdentifier())) {
                return false;
            }
        }

        return true;
    }

    /** This method is extremely expensive. */
    private boolean isMethodUsedInItsPackage(final IMethodBinding methodBinding, MethodDeclaration node) {
        final IPackageBinding methodPackage = methodBinding.getDeclaringClass().getPackage();

        final AtomicBoolean methodIsUsedInPackage = new AtomicBoolean(false);
        final SearchRequestor requestor = new SearchRequestor() {
            @Override
            public void acceptSearchMatch(SearchMatch match) {
                methodIsUsedInPackage.set(true);
            }
        };

        try {
            final SearchEngine searchEngine = new SearchEngine();
            searchEngine.search(
                    createPattern(methodBinding.getJavaElement(), REFERENCES, R_EXACT_MATCH),
                    new SearchParticipant[] { SearchEngine.getDefaultSearchParticipant() },
                    SearchEngine.createJavaSearchScope(new IJavaElement[] { methodPackage.getJavaElement() }),
                    requestor,
                    ctx.getProgressMonitor());
            return methodIsUsedInPackage.get();
        } catch (CoreException e) {
            throw new UnhandledException(node, e);
        }
    }

    private boolean declaredInSamePackage(final IMethodBinding methodBinding1, final IMethodBinding methodBinding2) {
        final ITypeBinding declaringClass1 = methodBinding1.getDeclaringClass();
        final ITypeBinding declaringClass2 = methodBinding2.getDeclaringClass();
        return declaringClass1.getPackage().equals(declaringClass2.getPackage());
    }

    private boolean haveSameModifiers(final IMethodBinding overriding, final IMethodBinding overridden) {
        // UCDetector can suggest to reduce visibility where possible
        return overriding.getModifiers() == overridden.getModifiers();
    }

    private boolean hasSignificantAnnotations(final IMethodBinding methodBinding) {
        for (final IAnnotationBinding annotation : methodBinding.getAnnotations()) {
            final ITypeBinding annotationType = annotation.getAnnotationType();

            if (!hasType(annotationType, "java.lang.Override", "java.lang.SuppressWarnings")) {
                return true;
            }
        }
        return false;
    }
}

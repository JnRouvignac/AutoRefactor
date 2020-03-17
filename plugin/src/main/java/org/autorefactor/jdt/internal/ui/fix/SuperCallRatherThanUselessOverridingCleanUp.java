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
package org.autorefactor.jdt.internal.ui.fix;

import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
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
import org.eclipse.jdt.core.search.IJavaSearchConstants;
import org.eclipse.jdt.core.search.SearchEngine;
import org.eclipse.jdt.core.search.SearchMatch;
import org.eclipse.jdt.core.search.SearchParticipant;
import org.eclipse.jdt.core.search.SearchPattern;
import org.eclipse.jdt.core.search.SearchRequestor;

/**
 * Removes overriding of method if the overriding only call the super class.
 *
 * @see #getDescription()
 */
public class SuperCallRatherThanUselessOverridingCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_SuperCallRatherThanUselessOverridingCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_SuperCallRatherThanUselessOverridingCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_SuperCallRatherThanUselessOverridingCleanUp_reason;
    }

    @Override
    public boolean visit(final MethodDeclaration node) {
        if (node.getBody() == null) {
            return true;
        }

        List<Statement> bodyStatements= ASTNodes.statements(node.getBody());

        if (bodyStatements.size() == 1) {
            SuperMethodInvocation bodyMi= ASTNodes.asExpression(bodyStatements.get(0), SuperMethodInvocation.class);

            if (bodyMi != null) {
                IMethodBinding bodyMethodBinding= bodyMi.resolveMethodBinding();
                IMethodBinding declMethodBinding= node.resolveBinding();

                if (declMethodBinding != null && bodyMethodBinding != null
                        && declMethodBinding.overrides(bodyMethodBinding)
                        && !hasSignificantAnnotations(declMethodBinding)
                        && haveSameModifiers(bodyMethodBinding, declMethodBinding)
                        && haveSameParameters(node, bodyMi)) {
                    if (!Modifier.isProtected(declMethodBinding.getModifiers())
                            || declaredInSamePackage(bodyMethodBinding, declMethodBinding)
                            // protected also means package visibility, so check if it is required
                            || !isMethodUsedInItsPackage(declMethodBinding, node)) {
                        cuRewrite.getASTRewrite().remove(node);
                        return false;
                    }
                }
            }
        }

        return true;
    }

    private boolean haveSameParameters(final MethodDeclaration node, final SuperMethodInvocation bodyMi) {
        List<?> parameters= node.parameters();

        for (int i= 0; i < node.parameters().size(); i++) {
            SingleVariableDeclaration paramName= (SingleVariableDeclaration) parameters.get(i);
            SimpleName paramExpression= ASTNodes.as((Expression) bodyMi.arguments().get(i), SimpleName.class);

            if (paramExpression == null
                    || !paramName.getName().getIdentifier().equals(paramExpression.getIdentifier())) {
                return false;
            }
        }

        return true;
    }

    /** This method is extremely expensive. */
    private boolean isMethodUsedInItsPackage(final IMethodBinding methodBinding, final MethodDeclaration node) {
        IPackageBinding methodPackage= methodBinding.getDeclaringClass().getPackage();

        final AtomicBoolean methodIsUsedInPackage= new AtomicBoolean(false);
        SearchRequestor requestor= new SearchRequestor() {
            @Override
            public void acceptSearchMatch(final SearchMatch match) {
                methodIsUsedInPackage.set(true);
            }
        };

        try {
            SearchEngine searchEngine= new SearchEngine();
            searchEngine.search(SearchPattern.createPattern(methodBinding.getJavaElement(), IJavaSearchConstants.REFERENCES, SearchPattern.R_EXACT_MATCH),
                    new SearchParticipant[] { SearchEngine.getDefaultSearchParticipant() },
                    SearchEngine.createJavaSearchScope(new IJavaElement[] { methodPackage.getJavaElement() }),
                    requestor, cuRewrite.getProgressMonitor());
            return methodIsUsedInPackage.get();
        } catch (CoreException e) {
            throw new UnhandledException(node, e);
        }
    }

    private boolean declaredInSamePackage(final IMethodBinding methodBinding1, final IMethodBinding methodBinding2) {
        ITypeBinding declaringClass1= methodBinding1.getDeclaringClass();
        ITypeBinding declaringClass2= methodBinding2.getDeclaringClass();
        return declaringClass1.getPackage().equals(declaringClass2.getPackage());
    }

    private boolean haveSameModifiers(final IMethodBinding overriding, final IMethodBinding overridden) {
        // UCDetector can suggest to reduce visibility where possible
        return overriding.getModifiers() == overridden.getModifiers();
    }

    private boolean hasSignificantAnnotations(final IMethodBinding methodBinding) {
        for (IAnnotationBinding annotation : methodBinding.getAnnotations()) {
            ITypeBinding annotationType= annotation.getAnnotationType();

            if (!ASTNodes.hasType(annotationType, Override.class.getCanonicalName(), SuppressWarnings.class.getCanonicalName())) {
                return true;
            }
        }

        return false;
    }
}

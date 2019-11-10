/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016-2017 Jean-Noël Rouvignac - initial API and implementation
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

import java.io.Closeable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.autorefactor.jdt.internal.corext.dom.VarDefinitionsUsesVisitor;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/** See {@link #getDescription()} method. */
public class TryWithResourceCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_TryWithResourceCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_TryWithResourceCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_TryWithResourceCleanUp_reason;
    }

    @Override
    public boolean isJavaVersionSupported(Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 7;
    }

    @Override
    public boolean visit(TryStatement node) {
        final List<Statement> tryStatements= ASTNodes.asList(node.getBody());
        if (!tryStatements.isEmpty() && tryStatements.get(0).getNodeType() == ASTNode.TRY_STATEMENT) {
            final TryStatement innerTryStatement= ASTNodes.as(tryStatements.get(0), TryStatement.class);
            if (innerTryStatement != null && !innerTryStatement.resources().isEmpty() && innerTryStatement.catchClauses().isEmpty()) {
                return collapseTryStatements(node, innerTryStatement);
            }
        }

        final VariableDeclarationStatement previousDeclStatement= ASTNodes.as(ASTNodes.getPreviousStatement(node),
                VariableDeclarationStatement.class);
        if (previousDeclStatement == null) {
            return true;
        }

        final VariableDeclarationFragment previousDeclFragment= ASTNodes.getUniqueFragment(previousDeclStatement);
        final List<Statement> finallyStatements= ASTNodes.asList(node.getFinally());

        if (previousDeclFragment != null && !finallyStatements.isEmpty()) {
            final List<ASTNode> nodesToRemove= new ArrayList<>();
            nodesToRemove.add(previousDeclStatement);

            final Statement finallyStatement= finallyStatements.get(0);
            nodesToRemove.add(finallyStatements.size() == 1 ? node.getFinally() : finallyStatement);

            final ExpressionStatement finallyEs= ASTNodes.as(finallyStatement, ExpressionStatement.class);
            final IfStatement finallyIs= ASTNodes.as(finallyStatement, IfStatement.class);

            if (finallyEs != null) {
                final MethodInvocation mi= ASTNodes.as(finallyEs.getExpression(), MethodInvocation.class);
                if (methodClosesCloseables(mi) && ASTNodes.areSameVariables(previousDeclFragment, mi.getExpression())) {
                    return maybeRefactorToTryWithResources(node, tryStatements, previousDeclStatement, previousDeclFragment,
                            nodesToRemove);
                }
            } else if (finallyIs != null && ASTNodes.asList(finallyIs.getThenStatement()).size() == 1
                    && ASTNodes.asList(finallyIs.getElseStatement()).isEmpty()) {
                final Expression nullCheckedExpression= ASTNodes.getNullCheckedExpression(finallyIs.getExpression());

                final Statement thenStatement= ASTNodes.asList(finallyIs.getThenStatement()).get(0);
                final MethodInvocation mi= ASTNodes.asExpression(thenStatement, MethodInvocation.class);

                if (methodClosesCloseables(mi)
                        && ASTNodes.areSameVariables(previousDeclFragment, nullCheckedExpression, mi.getExpression())) {
                    return maybeRefactorToTryWithResources(node, tryStatements, previousDeclStatement, previousDeclFragment,
                            nodesToRemove);
                }
            }
        }

        return true;
    }

    private boolean maybeRefactorToTryWithResources(TryStatement node, final List<Statement> tryStatements,
            final VariableDeclarationStatement previousDeclStatement,
            final VariableDeclarationFragment previousDeclFragment, final List<ASTNode> nodesToRemove) {
        final VariableDeclarationExpression newResource= newResource(tryStatements, previousDeclStatement,
                previousDeclFragment, nodesToRemove);

        if (newResource == null) {
            return true;
        }

        final Refactorings r= ctx.getRefactorings();
        r.insertFirst(node, TryStatement.RESOURCES_PROPERTY, newResource);
        r.remove(nodesToRemove);
        return false;
    }

    private boolean methodClosesCloseables(final MethodInvocation mi) {
        if (ASTNodes.usesGivenSignature(mi, Closeable.class.getCanonicalName(), "close")) { //$NON-NLS-1$
            return true;
        }
//        // Try to handle Guava's Closeables.closeQuietly(), Apache Commons IO'a IOUtils.closeQuietly()
//        // and/or all various homegrown static utilities closing Closeables
//        IMethodBinding methodBinding = mi.resolveMethodBinding();
//        return methodBinding != null
//                && methodBinding.getName().startsWith("close")
//                // In theory we should also verify the code of the method that is being called.
//                // In practice, the only thing you can do with an instance of Closeable is to close it,
//                // so let's assume this is exactly what the method does
//                && (isArrayOfCloseables(methodBinding.getParameterTypes())
//                        || isVarargsOfCloseables(methodBinding.getParameterTypes())
//                        // Beware of generic types (wildcards like ? extends Closeable)
//                        || isCollectionOfCloseables(methodBinding.getParameterTypes())
//                        || isCloseable(methodBinding.getParameterTypes()));
        return false;
    }

    private VariableDeclarationExpression newResource(List<Statement> tryStatements,
            VariableDeclarationStatement previousDeclStatement, VariableDeclarationFragment previousDeclFragment,
            List<ASTNode> nodesToRemove) {
        final ASTNodeFactory b= ctx.getASTBuilder();
        final VariableDeclarationFragment fragment= newFragment(tryStatements, previousDeclFragment, nodesToRemove);
        return fragment != null ? b.declareExpression(b.move(previousDeclStatement.getType()), fragment) : null;
    }

    private VariableDeclarationFragment newFragment(List<Statement> tryStatements,
            VariableDeclarationFragment existingFragment, List<ASTNode> nodesToRemove) {
        final VarDefinitionsUsesVisitor visitor= new VarDefinitionsUsesVisitor(existingFragment).find();
        final List<SimpleName> definitions= visitor.getWrites();

        final ASTNodeFactory b= ctx.getASTBuilder();
        if (!tryStatements.isEmpty()) {
            final Statement tryStatement= tryStatements.get(0);
            final Assignment assignResource= ASTNodes.asExpression(tryStatement, Assignment.class);
            if (assignResource != null && ASTNodes.isSameVariable(existingFragment, assignResource.getLeftHandSide())) {
                nodesToRemove.add(tryStatement);
                if (containsOnly(definitions, assignResource.getLeftHandSide(), existingFragment.getName())) {
                    return b.declareFragment(b.move(existingFragment.getName()),
                            b.move(assignResource.getRightHandSide()));
                }

                return null;
            }
        }

        return containsOnly(definitions, existingFragment.getName()) ? b.move(existingFragment) : null;
    }

    private boolean containsOnly(Collection<SimpleName> definitions, Expression... simpleNames) {
        if (definitions.size() != simpleNames.length) {
            return false;
        }
        for (Expression simpleName : simpleNames) {
            if (!definitions.contains(simpleName)) {
                return false;
            }
        }

        return true;
    }

    private boolean collapseTryStatements(TryStatement outerTryStatement, TryStatement innerTryStatement) {
        final Refactorings r= ctx.getRefactorings();
        final ASTNodeFactory b= ctx.getASTBuilder();
        r.insertLast(outerTryStatement, TryStatement.RESOURCES_PROPERTY, b.copyRange(ASTNodes.resources(innerTryStatement)));
        r.replace(innerTryStatement, b.move(innerTryStatement.getBody()));
        return false;
    }
}

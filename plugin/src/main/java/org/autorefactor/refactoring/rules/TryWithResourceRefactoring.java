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
package org.autorefactor.refactoring.rules;

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.areSameVariables;
import static org.autorefactor.refactoring.ASTHelper.as;
import static org.autorefactor.refactoring.ASTHelper.asExpression;
import static org.autorefactor.refactoring.ASTHelper.asList;
import static org.autorefactor.refactoring.ASTHelper.getNullCheckedExpression;
import static org.autorefactor.refactoring.ASTHelper.getPreviousStatement;
import static org.autorefactor.refactoring.ASTHelper.getUniqueFragment;
import static org.autorefactor.refactoring.ASTHelper.isMethod;
import static org.autorefactor.refactoring.ASTHelper.isSameVariable;
import static org.autorefactor.refactoring.ASTHelper.resources;
import static org.eclipse.jdt.core.dom.ASTNode.TRY_STATEMENT;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.Release;
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
public class TryWithResourceRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Use try-with-resource";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Changes code to make use of Java 7 try-with-resources feature. "
            + "In particular, it removes now useless finally clauses.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves the readibility. It also upgrades legacy code.";
    }

    @Override
    public boolean isJavaVersionSupported(Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 7;
    }

    @Override
    public boolean visit(TryStatement node) {
        final List<Statement> tryStmts = asList(node.getBody());
        if (tryStmts.size() >= 1 && tryStmts.get(0).getNodeType() == TRY_STATEMENT) {
            final TryStatement innerTryStmt = as(tryStmts.get(0), TryStatement.class);
            if (innerTryStmt != null
                    && !innerTryStmt.resources().isEmpty()
                    && innerTryStmt.catchClauses().isEmpty()) {
                return collapseTryStatements(node, innerTryStmt);
            }
        }

        final VariableDeclarationStatement previousDeclStmt =
            as(getPreviousStatement(node), VariableDeclarationStatement.class);
        if (previousDeclStmt == null) {
            return VISIT_SUBTREE;
        }

        final VariableDeclarationFragment previousDeclFragment = getUniqueFragment(previousDeclStmt);
        final List<Statement> finallyStmts = asList(node.getFinally());
        if (previousDeclFragment != null && finallyStmts.size() >= 1) {
            final List<ASTNode> nodesToRemove = new ArrayList<ASTNode>();
            nodesToRemove.add(previousDeclStmt);

            final Statement finallyStmt = finallyStmts.get(0);
            nodesToRemove.add(finallyStmts.size() == 1 ? node.getFinally() : finallyStmt);

            final ExpressionStatement finallyEs = as(finallyStmt, ExpressionStatement.class);
            final IfStatement finallyIs = as(finallyStmt, IfStatement.class);
            if (finallyEs != null) {
                final MethodInvocation mi = as(finallyEs.getExpression(), MethodInvocation.class);
                if (methodClosesCloseables(mi)
                        && areSameVariables(previousDeclFragment, mi.getExpression())) {
                    final VariableDeclarationExpression newResource =
                        newResource(tryStmts, previousDeclStmt, previousDeclFragment, nodesToRemove);
                    return refactorToTryWithResources(node, newResource, nodesToRemove);
                }
            } else if (finallyIs != null
                    && asList(finallyIs.getThenStatement()).size() == 1
                    && asList(finallyIs.getElseStatement()).isEmpty()) {
                final Expression nullCheckedExpr = getNullCheckedExpression(finallyIs.getExpression());

                final Statement thenStmt = asList(finallyIs.getThenStatement()).get(0);
                final MethodInvocation mi = asExpression(thenStmt, MethodInvocation.class);
                if (methodClosesCloseables(mi)
                        && areSameVariables(previousDeclFragment, nullCheckedExpr, mi.getExpression())) {
                    final VariableDeclarationExpression newResource =
                        newResource(tryStmts, previousDeclStmt, previousDeclFragment, nodesToRemove);
                    return refactorToTryWithResources(node, newResource, nodesToRemove);
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean methodClosesCloseables(final MethodInvocation mi) {
        if (isMethod(mi, "java.io.Closeable", "close")) {
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

    private boolean refactorToTryWithResources(
            TryStatement node, VariableDeclarationExpression newResource, List<ASTNode> nodesToRemove) {
        if (newResource == null) {
            return VISIT_SUBTREE;
        }
        final Refactorings r = ctx.getRefactorings();
        r.insertFirst(node, TryStatement.RESOURCES_PROPERTY, newResource);
        r.remove(nodesToRemove);
        return DO_NOT_VISIT_SUBTREE;
    }

    private VariableDeclarationExpression newResource(
            List<Statement> tryStmts,
            VariableDeclarationStatement previousDeclStmt,
            VariableDeclarationFragment previousDeclFragment,
            List<ASTNode> nodesToRemove) {
        final ASTBuilder b = ctx.getASTBuilder();
        final VariableDeclarationFragment fragment = newFragment(tryStmts, previousDeclFragment, nodesToRemove);
        return fragment != null
            ? b.declareExpr(b.move(previousDeclStmt.getType()), fragment)
            : null;
    }

    private VariableDeclarationFragment newFragment(
            List<Statement> tryStmts,
            VariableDeclarationFragment existingFragment,
            List<ASTNode> nodesToRemove) {
        final VariableDefinitionsUsesVisitor visitor = new VariableDefinitionsUsesVisitor(existingFragment).find();
        final List<SimpleName> definitions = visitor.getDefinitions();

        final ASTBuilder b = ctx.getASTBuilder();
        if (!tryStmts.isEmpty()) {
            final Statement tryStmt = tryStmts.get(0);
            final Assignment assignResource = asExpression(tryStmt, Assignment.class);
            if (assignResource != null
                    && isSameVariable(existingFragment, assignResource.getLeftHandSide())) {
                nodesToRemove.add(tryStmt);
                if (containsOnly(definitions, assignResource.getLeftHandSide(), existingFragment.getName())) {
                    return b.declareFragment(
                        b.move(existingFragment.getName()),
                        b.move(assignResource.getRightHandSide()));
                }
                return null;
            }
        }
        return containsOnly(definitions, existingFragment.getName())
            ? b.move(existingFragment)
            : null;
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

    private boolean collapseTryStatements(TryStatement outerTryStmt, TryStatement innerTryStmt) {
        final Refactorings r = ctx.getRefactorings();
        final ASTBuilder b = ctx.getASTBuilder();
        r.insertLast(outerTryStmt, TryStatement.RESOURCES_PROPERTY, b.copyRange(resources(innerTryStmt)));
        r.replace(innerTryStmt, b.move(innerTryStmt.getBody()));
        return DO_NOT_VISIT_SUBTREE;
    }
}

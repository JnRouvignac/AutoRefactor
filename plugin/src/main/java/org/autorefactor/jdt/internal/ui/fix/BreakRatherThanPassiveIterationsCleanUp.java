/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice Tiercelin - initial API and implementation
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

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.InterruptibleVisitor;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SuperMethodInvocation;
import org.eclipse.jdt.core.dom.ThrowStatement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/** See {@link #getDescription()} method. */
public class BreakRatherThanPassiveIterationsCleanUp extends AbstractCleanUpRule {
    private static final class SideEffectVisitor extends InterruptibleVisitor {
        private final Set<String> localVariableNames;
        private boolean hasSideEffect;

        private SideEffectVisitor(final Set<String> localVariableNames) {
            this.localVariableNames= localVariableNames;
        }

        private boolean hasSideEffect() {
            return hasSideEffect;
        }

        @Override
        public boolean visit(final Assignment node) {
            if (!ASTNodes.hasOperator(node, Assignment.Operator.ASSIGN)) {
                hasSideEffect= true;
                return interruptVisit();
            }

            return visitVar(node.getLeftHandSide());
        }

        private boolean visitVar(final Expression modifiedVar) {
            if (!(modifiedVar instanceof SimpleName)
                    || !localVariableNames.contains(((SimpleName) modifiedVar).getIdentifier())) {
                hasSideEffect= true;
                return interruptVisit();
            }

            return true;
        }

        @Override
        public boolean visit(final PrefixExpression node) {
            return !ASTNodes.hasOperator(node, PrefixExpression.Operator.INCREMENT, PrefixExpression.Operator.DECREMENT) || visitVar(node.getOperand());
        }

        @Override
        public boolean visit(final PostfixExpression node) {
            return visitVar(node.getOperand());
        }

        @SuppressWarnings("unchecked")
        @Override
        public boolean visit(final InfixExpression node) {
            if (ASTNodes.hasOperator(node, InfixExpression.Operator.PLUS) && ASTNodes.hasType(node, String.class.getCanonicalName())
                    && (mayCallImplicitToString(node.getLeftOperand())
                            || mayCallImplicitToString(node.getRightOperand())
                            || mayCallImplicitToString(node.extendedOperands()))) {
                hasSideEffect= true;
                return interruptVisit();
            }
            return true;
        }

        private boolean mayCallImplicitToString(final List<Expression> extendedOperands) {
            if (extendedOperands != null) {
                for (Expression expression : extendedOperands) {
                    if (mayCallImplicitToString(expression)) {
                        return true;
                    }
                }
            }
            return false;
        }

        private boolean mayCallImplicitToString(final Expression expression) {
            return !ASTNodes.hasType(expression, String.class.getCanonicalName(), boolean.class.getSimpleName(), short.class.getSimpleName(), int.class.getSimpleName(), long.class.getSimpleName(), float.class.getSimpleName(), double.class.getSimpleName(),
                    Short.class.getCanonicalName(), Boolean.class.getCanonicalName(), Integer.class.getCanonicalName(), Long.class.getCanonicalName(), Float.class.getCanonicalName(),
                    Double.class.getCanonicalName()) && !(expression instanceof PrefixExpression) && !(expression instanceof InfixExpression)
                    && !(expression instanceof PostfixExpression);
        }

        @Override
        public boolean visit(final SuperMethodInvocation node) {
            hasSideEffect= true;
            return interruptVisit();
        }

        @Override
        public boolean visit(final MethodInvocation node) {
            hasSideEffect= true;
            return interruptVisit();
        }

        @Override
        public boolean visit(final ClassInstanceCreation node) {
            hasSideEffect= true;
            return interruptVisit();
        }

        @Override
        public boolean visit(final ThrowStatement node) {
            hasSideEffect= true;
            return interruptVisit();
        }
    }

    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_BreakRatherThanPassiveIterationsCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_BreakRatherThanPassiveIterationsCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_BreakRatherThanPassiveIterationsCleanUp_reason;
    }

    @Override
    public boolean visit(final ForStatement node) {
        final Set<String> vars= new HashSet<>();

        for (Expression initializer : ASTNodes.initializers(node)) {
            vars.addAll(ASTNodes.getLocalVariableIdentifiers(initializer, true));
        }

        if (hasSideEffect(node.getExpression(), vars)) {
            return true;
        }

        for (Expression updater : ASTNodes.updaters(node)) {
            if (hasSideEffect(updater, vars)) {
                return true;
            }
        }

        return visitLoopBody(node.getBody(), vars);
    }

    private boolean hasSideEffect(final ASTNode node, final Set<String> allowedVars) {
        final SideEffectVisitor variableUseVisitor= new SideEffectVisitor(allowedVars);
        variableUseVisitor.visitNode(node);
        return variableUseVisitor.hasSideEffect();
    }

    @Override
    public boolean visit(final EnhancedForStatement node) {
        return !ASTNodes.isArray(node.getExpression()) || visitLoopBody(node.getBody(), new HashSet<String>());
    }

    private boolean visitLoopBody(final Statement body, final Set<String> allowedVars) {
        final List<Statement> statements= ASTNodes.asList(body);

        if (statements == null || statements.isEmpty()) {
            return true;
        }

        for (int i= 0; i < statements.size() - 1; i++) {
            final Statement statement= statements.get(i);
            allowedVars.addAll(ASTNodes.getLocalVariableIdentifiers(statement, true));

            if (hasSideEffect(statement, allowedVars)) {
                return true;
            }
        }

        final IfStatement ifStatement= ASTNodes.as(statements.get(statements.size() - 1), IfStatement.class);

        if (ifStatement != null && ifStatement.getElseStatement() == null && !hasSideEffect(ifStatement.getExpression(), allowedVars)) {
            final List<Statement> assignments= ASTNodes.asList(ifStatement.getThenStatement());

            for (Statement statement : assignments) {
                if (statement instanceof VariableDeclarationStatement) {
                    final VariableDeclarationStatement decl= (VariableDeclarationStatement) statement;

                    for (Object obj : decl.fragments()) {
                        final VariableDeclarationFragment fragment= (VariableDeclarationFragment) obj;

                        if (!ASTNodes.isHardCoded(fragment.getInitializer())) {
                            return true;
                        }
                    }
                } else if (statement instanceof ExpressionStatement) {
                    final Assignment assignment= ASTNodes.as(((ExpressionStatement) statement).getExpression(), Assignment.class);

                    if (assignment == null || !ASTNodes.hasOperator(assignment, Assignment.Operator.ASSIGN) || !ASTNodes.isHardCoded(assignment.getRightHandSide())) {
                        return true;
                    }
                } else {
                    return true;
                }
            }

            addBreak(ifStatement, assignments);
            return false;
        }

        return true;
    }

    private void addBreak(final IfStatement ifStatement, final List<Statement> assignments) {
        final ASTNodeFactory b= ctx.getASTBuilder();
        final Refactorings r= ctx.getRefactorings();

        if (ifStatement.getThenStatement() instanceof Block) {
            r.insertAfter(b.break0(), assignments.get(assignments.size() - 1));
        } else {
            r.replace(ifStatement.getThenStatement(), b.block(b.copy(ifStatement.getThenStatement()), b.break0()));
        }
    }
}

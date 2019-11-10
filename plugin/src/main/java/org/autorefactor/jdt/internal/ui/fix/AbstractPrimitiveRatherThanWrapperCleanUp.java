/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice TIERCELIN - initial API and implementation
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

import static org.eclipse.jdt.core.dom.ASTNode.ASSIGNMENT;
import static org.eclipse.jdt.core.dom.ASTNode.CAST_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.CONDITIONAL_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.INFIX_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.PARENTHESIZED_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.POSTFIX_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.PREFIX_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.RETURN_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.VARIABLE_DECLARATION_FRAGMENT;

import java.util.ArrayList;
import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.InterruptibleVisitor;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/** See {@link #getDescription()} method. */
public abstract class AbstractPrimitiveRatherThanWrapperCleanUp extends AbstractCleanUpRule {
    /**
     * Get the wrapper fully qualified name.
     *
     * @return the wrapper fully qualified name.
     */
    public abstract String getWrapperFullyQualifiedName();

    /**
     * Get the primitive type name.
     *
     * @return the primitive type name.
     */
    public abstract String getPrimitiveTypeName();

    /**
     * Get the literal class.
     *
     * @return the literal class.
     */
    public abstract Class<? extends Expression> getLiteralClass();

    /**
     * Get the prefix in safe operators.
     *
     * @return the prefix in safe operators.
     */
    public List<PrefixExpression.Operator> getPrefixInSafeOperators() {
        return new ArrayList<>(0);
    }

    /**
     * Get the Infix In Safe Operators.
     *
     * @return the Infix In Safe Operators.
     */
    public List<InfixExpression.Operator> getInfixInSafeOperators() {
        return new ArrayList<>(0);
    }

    /**
     * Get the postfix in safe operators.
     *
     * @return the postfix in safe operators.
     */
    public List<PostfixExpression.Operator> getPostfixInSafeOperators() {
        return new ArrayList<>(0);
    }

    /**
     * Get the prefix out safe operators.
     *
     * @return the prefix out safe operators.
     */
    public List<PrefixExpression.Operator> getPrefixOutSafeOperators() {
        return new ArrayList<>(0);
    }

    /**
     * Get the infix out safe operators.
     *
     * @return the infix out safe operators.
     */
    public List<InfixExpression.Operator> getInfixOutSafeOperators() {
        return new ArrayList<>(0);
    }

    /**
     * Get the postfix out safe operators.
     *
     * @return the postfix out safe operators.
     */
    public List<PostfixExpression.Operator> getPostfixOutSafeOperators() {
        return new ArrayList<>(0);
    }

    /**
     * Get the assignment out safe operators.
     *
     * @return the assignment out safe operators.
     */
    public List<Assignment.Operator> getAssignmentOutSafeOperators() {
        return new ArrayList<>(0);
    }

    /**
     * Get the safe in constants.
     *
     * @return the safe in constants.
     */
    public String[] getSafeInConstants() {
        return new String[0];
    }

    /**
     * True if the specific primitive is allowed.
     *
     * @param node The node
     *
     * @return True if the specific primitive is allowed.
     */
    public boolean isSpecificPrimitiveAllowed(final ASTNode node) {
        return false;
    }

    @Override
    public boolean visit(VariableDeclarationStatement node) {
        if (node.fragments().size() == 1) {
            final VariableDeclarationFragment fragment= (VariableDeclarationFragment) node.fragments().get(0);

            if (fragment.resolveBinding() != null
                    && ASTNodes.hasType(fragment.resolveBinding().getType(), getWrapperFullyQualifiedName())
                    && fragment.getInitializer() != null && isNotNull(fragment.getInitializer())) {
                final VarOccurrenceVisitor varOccurrenceVisitor= new VarOccurrenceVisitor(fragment);
                final Block parentBlock= ASTNodes.getAncestorOrNull(fragment, Block.class);

                if (parentBlock != null) {
                    varOccurrenceVisitor.visitNode(parentBlock);

                    if (varOccurrenceVisitor.isPrimitiveAllowed() && varOccurrenceVisitor.getAutoBoxingCount() < 2) {
                        refactorWrapper(node);
                        return false;
                    }
                }
            }
        }

        return true;
    }

    private void refactorWrapper(final VariableDeclarationStatement node) {
        final ASTNodeFactory b= this.ctx.getASTBuilder();
        final Type primitiveType= b.type(getPrimitiveTypeName());
        ctx.getRefactorings().replace(node.getType(), primitiveType);
    }

    private boolean isNotNull(final Expression expression) {
        if (expression instanceof ParenthesizedExpression) {
            final ParenthesizedExpression parenthesizedExpression= (ParenthesizedExpression) expression;
            return isNotNull(parenthesizedExpression.getExpression());
        }
        if (expression instanceof ConditionalExpression) {
            final ConditionalExpression prefixExpression= (ConditionalExpression) expression;
            return isNotNull(prefixExpression.getThenExpression()) && isNotNull(prefixExpression.getElseExpression());
        }
        if (getLiteralClass().equals(expression.getClass())) {
            return true;
        }
        if (expression instanceof QualifiedName) {
            final QualifiedName qualifiedName= (QualifiedName) expression;
            return ASTNodes.hasType(qualifiedName.getQualifier(), getWrapperFullyQualifiedName())
                    && (ASTNodes.isField(qualifiedName, getWrapperFullyQualifiedName(), getSafeInConstants())
                            || ASTNodes.isField(qualifiedName, getPrimitiveTypeName(), getSafeInConstants()));
        }
        if (expression instanceof InfixExpression) {
            final InfixExpression infixExpression= (InfixExpression) expression;
            return getInfixInSafeOperators().contains(infixExpression.getOperator());
        }
        if (expression instanceof PrefixExpression) {
            final PrefixExpression prefixExpression= (PrefixExpression) expression;
            return getPrefixInSafeOperators().contains(prefixExpression.getOperator());
        }
        if (expression instanceof PostfixExpression) {
            final PostfixExpression postfixExpression= (PostfixExpression) expression;
            return getPostfixInSafeOperators().contains(postfixExpression.getOperator());
        }
        if (expression instanceof CastExpression) {
            final CastExpression castExpression= (CastExpression) expression;
            return ASTNodes.hasType(castExpression.getType().resolveBinding(), getPrimitiveTypeName())
                    || ASTNodes.hasType(castExpression.getType().resolveBinding(), getWrapperFullyQualifiedName())
                            && isNotNull(castExpression.getExpression());
        }
        if (expression instanceof MethodInvocation) {
            final MethodInvocation mi= (MethodInvocation) expression;
            return ASTNodes.usesGivenSignature(mi, getWrapperFullyQualifiedName(), "valueOf", getPrimitiveTypeName()); //$NON-NLS-1$
        }

        return false;
    }

    private class VarOccurrenceVisitor extends InterruptibleVisitor {
        private final VariableDeclarationFragment varDecl;

        private boolean isPrimitiveAllowed= true;

        private boolean isVarReturned;

        private int autoBoxingCount;

        public VarOccurrenceVisitor(final VariableDeclarationFragment var) {
            varDecl= var;
        }

        public boolean isPrimitiveAllowed() {
            return isPrimitiveAllowed;
        }

        public int getAutoBoxingCount() {
            return autoBoxingCount;
        }

        @Override
        public boolean visit(final SimpleName aVar) {
            if (isPrimitiveAllowed && aVar.getIdentifier().equals(varDecl.getName().getIdentifier())
                    && !aVar.getParent().equals(varDecl)) {
                isPrimitiveAllowed= isPrimitiveAllowed(aVar);

                if (!isPrimitiveAllowed) {
                    return interruptVisit();
                }
            }

            return true;
        }

        private boolean isPrimitiveAllowed(final ASTNode node) {
            final ASTNode parentNode= node.getParent();

            switch (parentNode.getNodeType()) {
            case PARENTHESIZED_EXPRESSION:
                return isPrimitiveAllowed(parentNode);

            case CAST_EXPRESSION:
                final CastExpression castExpression= (CastExpression) parentNode;
                return ASTNodes.hasType(castExpression.getType().resolveBinding(), getPrimitiveTypeName());

            case ASSIGNMENT:
                final Assignment assignment= (Assignment) parentNode;

                if (getAssignmentOutSafeOperators().contains(assignment.getOperator())) {
                    return true;
                }

                if (assignment.getLeftHandSide().equals(node)) {
                    return isNotNull(assignment.getRightHandSide());
                }

                if (assignment.getRightHandSide().equals(node)) {
                    if (assignment.getLeftHandSide() instanceof Name) {
                        return isOfType(((Name) assignment.getLeftHandSide()).resolveTypeBinding());
                    }
                    if (assignment.getLeftHandSide() instanceof FieldAccess) {
                        return isOfType(((FieldAccess) assignment.getLeftHandSide()).resolveTypeBinding());
                    }
                }

                return false;

            case VARIABLE_DECLARATION_FRAGMENT:
                final VariableDeclarationFragment fragment= (VariableDeclarationFragment) parentNode;
                return fragment.getInitializer().equals(node) && isOfType(fragment.getName().resolveTypeBinding());

            case RETURN_STATEMENT:
                final ReturnStatement returnStatement= (ReturnStatement) parentNode;
                if (returnStatement.getExpression().equals(node)) {
                    final MethodDeclaration method= ASTNodes.getAncestorOrNull(returnStatement, MethodDeclaration.class);

                    if (method != null && method.getReturnType2() != null) {
                        if (ASTNodes.hasType(method.getReturnType2().resolveBinding(), getPrimitiveTypeName())) {
                            return true;
                        }
                        if (ASTNodes.hasType(method.getReturnType2().resolveBinding(), getWrapperFullyQualifiedName())) {
                            if (!isVarReturned) {
                                isVarReturned= true;
                                autoBoxingCount++;
                            }

                            return true;
                        }
                    }
                }

                return false;

            case CONDITIONAL_EXPRESSION:
                final ConditionalExpression conditionalExpression= (ConditionalExpression) parentNode;
                return conditionalExpression.getExpression().equals(node);

            case PREFIX_EXPRESSION:
                return getPrefixOutSafeOperators().contains(((PrefixExpression) parentNode).getOperator());

            case INFIX_EXPRESSION:
                return getInfixOutSafeOperators().contains(((InfixExpression) parentNode).getOperator());

            case POSTFIX_EXPRESSION:
                return getPostfixOutSafeOperators().contains(((PostfixExpression) parentNode).getOperator());

            default:
                return isSpecificPrimitiveAllowed(node);
            }
        }

        private boolean isOfType(final ITypeBinding resolveTypeBinding) {
            if (ASTNodes.hasType(resolveTypeBinding, getPrimitiveTypeName())) {
                return true;
            }
            if (ASTNodes.hasType(resolveTypeBinding, getWrapperFullyQualifiedName())) {
                autoBoxingCount++;
                return true;
            }

            return false;
        }
    }
}

/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2015 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016-2017 Fabrice Tiercelin - Make sure we do not visit again modified nodes
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

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory.Copy;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.ASTSemanticMatcher;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.autorefactor.jdt.internal.corext.dom.VarDefinitionsUsesVisitor;
import org.autorefactor.jdt.internal.corext.refactoring.structure.CompilationUnitRewrite;
import org.autorefactor.util.IllegalArgumentException;
import org.autorefactor.util.IllegalStateException;
import org.autorefactor.util.NotImplementedException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BooleanLiteral;
import org.eclipse.jdt.core.dom.ChildListPropertyDescriptor;
import org.eclipse.jdt.core.dom.ChildPropertyDescriptor;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.PrimitiveType;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.StructuralPropertyDescriptor;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/** See {@link #getDescription()} method. */
public class BooleanCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_BooleanCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_BooleanCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_BooleanCleanUp_reason;
    }

    private static class BooleanASTMatcher extends ASTSemanticMatcher {
        /** else node to then node. */
        final Map<ASTNode, ASTNode> matches= new HashMap<>();
        final Map<ASTNode, ASTNode> previousMatches;

        public BooleanASTMatcher() {
            this(null);
        }

        public BooleanASTMatcher(final Map<ASTNode, ASTNode> previousMatches) {
            if (previousMatches != null) {
                this.previousMatches= previousMatches;
            } else {
                this.previousMatches= Collections.emptyMap();
            }
        }

        @Override
        public boolean match(final BooleanLiteral node, final Object other) {
            if (other instanceof Expression) {
                Expression expression= (Expression) other;

                if (areOppositeBooleanValues(node, expression)) {
                    matches.put(expression, node);
                    return true;
                }
            }

            return super.match(node, other);
        }

        @Override
        public boolean match(final QualifiedName node, final Object other) {
            if (other instanceof Expression) {
                Expression expression= (Expression) other;

                if (this.previousMatches.containsKey(other) || areOppositeBooleanValues(node, expression)) {
                    matches.put(expression, node);
                    return true;
                }
            }

            return super.match(node, other);
        }
    }

    private final class AssignmentIfAndReturnVisitor extends BlockSubVisitor {
        public AssignmentIfAndReturnVisitor(final CompilationUnitRewrite cuRewrite, final Block startNode) {
            super(cuRewrite, startNode);
        }

        @Override
        public boolean visit(final IfStatement node) {
            if (getResult()) {
                boolean result= visitIfStatement(node);

                if (!result) {
                    setResult(false);
                }
            }

            return getResult();
        }
    }

    @Override
    public boolean visit(final Block node) {
        AssignmentIfAndReturnVisitor assignmentIfAndReturnVisitor= new AssignmentIfAndReturnVisitor(cuRewrite, node);
        node.accept(assignmentIfAndReturnVisitor);
        return assignmentIfAndReturnVisitor.getResult();
    }

    /**
     * Compares mixed boolean literal and Boolean object values against each other.
     */
    private static boolean areOppositeBooleanValues(final Expression expr1, final Expression expr2) {
        Boolean b1= ASTNodes.getBooleanLiteral(expr1);
        Boolean b2= ASTNodes.getBooleanLiteral(expr2);
        return b1 != null && b2 != null && !b1.equals(b2);
    }

    private class BooleanReplaceVisitor extends ASTVisitor {
        private final Expression ifCondition;
        private final Collection<ASTNode> nodesToReplace;
        private final Name booleanName;

        public BooleanReplaceVisitor(final Expression ifCondition, final Collection<ASTNode> nodesToReplace, final Name booleanName) {
            this.ifCondition= ifCondition;
            this.nodesToReplace= nodesToReplace;
            this.booleanName= booleanName;
        }

        @Override
        public boolean visit(final BooleanLiteral node) {
            if (this.nodesToReplace.contains(node)) {
                Boolean booleanValue= ASTNodes.getBooleanLiteral(node);

                Expression orientedCondition;
                if (booleanValue) {
                    orientedCondition= b.createCopyTarget(ifCondition);
                } else {
                    orientedCondition= b.negate(ifCondition, Copy.COPY);
                }

                Expression expression= getExpression(orientedCondition, boolean.class.getSimpleName(), null);
                replaceInParent(node, expression);
            }

            return false;
        }

        @Override
        public boolean visit(final QualifiedName node) {
            if (this.nodesToReplace.contains(node)) {
                QualifiedName qn= ASTNodes.as(node, QualifiedName.class);
                Boolean booleanValue= ASTNodes.getBooleanObject(qn);

                if (booleanValue != null) {
                    Expression orientedCondition;
                    if (booleanValue) {
                        orientedCondition= b.createCopyTarget(ifCondition);
                    } else {
                        orientedCondition= b.negate(ifCondition, Copy.COPY);
                    }

                    Expression expression= getExpression(orientedCondition, Boolean.class.getCanonicalName(), booleanName);
                    replaceInParent(node, expression);
                }
            }

            return false;
        }

        private void replaceInParent(final ASTNode nodeToReplace, final ASTNode replacementNode) {
            if (nodeToReplace.getParent() == null) {
                throw new IllegalArgumentException(nodeToReplace, "The node to replace does not have a parent"); //$NON-NLS-1$
            }

            StructuralPropertyDescriptor locationInParent= nodeToReplace.getLocationInParent();

            if (locationInParent instanceof ChildPropertyDescriptor) {
                ChildPropertyDescriptor cpd= (ChildPropertyDescriptor) locationInParent;
                nodeToReplace.getParent().setStructuralProperty(cpd, replacementNode);
            } else if (locationInParent instanceof ChildListPropertyDescriptor) {
                ChildListPropertyDescriptor clpd= (ChildListPropertyDescriptor) locationInParent;
                @SuppressWarnings("unchecked") List<ASTNode> property= (List<ASTNode>) nodeToReplace.getParent().getStructuralProperty(clpd);
                property.set(property.indexOf(nodeToReplace), replacementNode);
            } else {
                throw new NotImplementedException(nodeToReplace, locationInParent);
            }
        }
    }

    private ASTNodeFactory b;

    @Override
    public void setRefactoringContext(final CompilationUnitRewrite cuRewrite) {
        super.setRefactoringContext(cuRewrite);
        b= cuRewrite.getASTBuilder();
    }

    @Override
    public boolean visit(final ConditionalExpression node) {
        ITypeBinding typeBinding= node.resolveTypeBinding();

        if (typeBinding != null) {
            Expression newE= newExpressionOrNull(typeBinding.getQualifiedName(), node.getExpression(),
                    node.getThenExpression(), node.getElseExpression());

            if (newE != null) {
                cuRewrite.getRefactorings().replace(node, newE);
                return false;
            }
        }

        return true;
    }

    private boolean withThenReturnStatement(final IfStatement node, final ReturnStatement thenRs, final ReturnStatement elseRs) {
        ReturnStatement newRs= getReturnStatement(node, thenRs.getExpression(), elseRs.getExpression());

        if (newRs != null) {
            cuRewrite.getRefactorings().replace(node, newRs);
            cuRewrite.getRefactorings().remove(elseRs);
            return false;
        }

        MethodDeclaration md= ASTNodes.getAncestor(node, MethodDeclaration.class);
        Type returnType= md.getReturnType2();

        if (returnType != null && returnType.isPrimitiveType()) {
            PrimitiveType pt= (PrimitiveType) returnType;

            if (PrimitiveType.BOOLEAN.equals(pt.getPrimitiveTypeCode())) {
                Boolean thenBool= ASTNodes.getBooleanLiteral(thenRs.getExpression());
                Boolean elseBool= ASTNodes.getBooleanLiteral(elseRs.getExpression());
                newRs= getReturnStatement(node, thenBool, elseBool, thenRs.getExpression(), elseRs.getExpression());

                if (newRs != null) {
                    cuRewrite.getRefactorings().replace(node, newRs);
                    cuRewrite.getRefactorings().remove(elseRs);
                    return false;
                }
            }
        }

        return true;
    }

    private boolean noThenReturnStatement(final IfStatement node) {
        Assignment thenA= ASTNodes.asExpression(node.getThenStatement(), Assignment.class);

        if (ASTNodes.hasOperator(thenA, Assignment.Operator.ASSIGN) && ASTNodes.asList(node.getElseStatement()).isEmpty()
                && (thenA.getLeftHandSide() instanceof Name || thenA.getLeftHandSide() instanceof FieldAccess)) {
            Statement previousSibling= ASTNodes.getPreviousSibling(node);

            if (previousSibling instanceof VariableDeclarationStatement) {
                VariableDeclarationStatement vds= (VariableDeclarationStatement) previousSibling;
                VariableDeclarationFragment vdf= getVariableDeclarationFragment(vds, thenA.getLeftHandSide());

                if (vdf != null) {
                    VarDefinitionsUsesVisitor variableUseVisitor= new VarDefinitionsUsesVisitor(
                            vdf.resolveBinding(), node.getExpression(), true).find();

                    if (variableUseVisitor.getReads().isEmpty()) {
                        ITypeBinding typeBinding= vds.getType().resolveBinding();
                        return maybeReplace(node, thenA, typeBinding, vdf.getInitializer());
                    }
                }
            } else if (previousSibling instanceof ExpressionStatement) {
                Assignment elseA= ASTNodes.asExpression(previousSibling, Assignment.class);

                if (ASTNodes.hasOperator(elseA, Assignment.Operator.ASSIGN) && ASTNodes.isSameVariable(thenA.getLeftHandSide(), elseA.getLeftHandSide())) {
                    ITypeBinding typeBinding= elseA.resolveTypeBinding();
                    return maybeReplace(node, thenA, typeBinding, elseA.getRightHandSide());
                }
            }
        }

        return true;
    }

    private boolean maybeReplace(final IfStatement node, final Assignment assignment, final ITypeBinding typeBinding, final Expression rightHandSide) {
        if (typeBinding != null) {
            String expressionTypeName= typeBinding.getQualifiedName();
            Expression newE= newExpressionOrNull(expressionTypeName, node.getExpression(), assignment.getRightHandSide(),
                    rightHandSide);

            if (newE != null) {
                cuRewrite.getRefactorings().replace(rightHandSide, newE);
                cuRewrite.getRefactorings().remove(node);
                return false;
            }
        }

        return true;
    }

    private ReturnStatement getReturnStatement(final IfStatement node, final Boolean thenBool, final Boolean elseBool,
            final Expression thenExpression, final Expression elseExpression) {
        if (thenBool == null && elseBool != null) {
            Expression leftOp= signExpression(b.parenthesizeIfNeeded(b.createCopyTarget(node.getExpression())), !elseBool);
            return b.return0(b.infixExpression(leftOp, getConditionalOperator(elseBool),
                    b.parenthesizeIfNeeded(b.createCopyTarget(thenExpression))));
        }

        if (thenBool != null && elseBool == null) {
            Expression leftOp= signExpression(b.parenthesizeIfNeeded(b.createCopyTarget(node.getExpression())),
                    thenBool);
            return b.return0(b.infixExpression(leftOp, getConditionalOperator(thenBool),
                    b.parenthesizeIfNeeded(b.createCopyTarget(elseExpression))));
        }

        return null;
    }

    private InfixExpression.Operator getConditionalOperator(final boolean isOrOperator) {
        return isOrOperator ? InfixExpression.Operator.CONDITIONAL_OR : InfixExpression.Operator.CONDITIONAL_AND;
    }

    private Expression signExpression(final Expression ie, final boolean isPositive) {
        if (isPositive) {
            return ie;
        }

        return b.negate(ie, Copy.NONE);
    }

    private VariableDeclarationFragment getVariableDeclarationFragment(final VariableDeclarationStatement vds,
            final Expression expression) {
        if (vds == null) {
            return null;
        }

        for (VariableDeclarationFragment vdf : ASTNodes.fragments(vds)) {
            if (ASTNodes.isSameVariable(expression, vdf)) {
                return vdf;
            }
        }

        return null;
    }

    private ReturnStatement getReturnStatement(final IfStatement node, final Expression thenExpression,
            final Expression elseExpression) {
        if (areOppositeBooleanValues(thenExpression, elseExpression)) {
            Expression exprToReturn= b.createCopyTarget(node.getExpression());

            if (ASTNodes.getBooleanLiteral(elseExpression)) {
                exprToReturn= b.negate(exprToReturn, Copy.NONE);
            }

            MethodDeclaration md= ASTNodes.getAncestor(node, MethodDeclaration.class);
            Expression returnExpression= getReturnExpression(md, exprToReturn);

            if (returnExpression != null) {
                return b.return0(returnExpression);
            }
        }

        return null;
    }

    private Expression getReturnExpression(final MethodDeclaration md, final Expression ifCondition) {
        IMethodBinding methodBinding= md.resolveBinding();

        if (methodBinding == null) {
            return null;
        }

        String qualifiedName= methodBinding.getReturnType().getQualifiedName();
        Expression newE= getExpression(ifCondition, qualifiedName, getBooleanName(md));

        if (newE != null) {
            return newE;
        }

        // TODO JNR rejuggle exception messages like this:
        // compilationUnit.java:line number: error message
        throw new IllegalStateException(md,
                "Did not expect any other return type than boolean or java.lang.Boolean for method " //$NON-NLS-1$
                        + md.getName().getIdentifier() + ", but found " + qualifiedName); //$NON-NLS-1$
    }

    private Expression newExpressionOrNull(final String expressionTypeName, final Expression condition, final Expression thenExpression,
            final Expression elseExpression) {
        Boolean thenLiteral= ASTNodes.getBooleanLiteral(thenExpression);
        Boolean elseLiteral= ASTNodes.getBooleanLiteral(elseExpression);

        if (areOppositeBooleanValues(thenExpression, elseExpression)) {
            Name booleanName= getBooleanName(condition);
            Expression orientedCondition;
            if (thenLiteral) {
                orientedCondition= b.createCopyTarget(condition);
            } else {
                orientedCondition= b.negate(condition, Copy.COPY);
            }

            return getExpression(orientedCondition, expressionTypeName, booleanName);
        }

        if ((ASTNodes.isPrimitive(thenExpression) || ASTNodes.isPrimitive(elseExpression))
                && ("boolean".equals(expressionTypeName) //$NON-NLS-1$
                        || Boolean.class.getCanonicalName().equals(expressionTypeName))) {
            // If both expressions are primitive, there cannot be any NPE
            // If only one expression is primitive, a NPE is already possible so we do not
            // care
            if (thenLiteral != null && elseLiteral == null) {
                if (thenLiteral) {
                    return b.infixExpression(b.createCopyTarget(condition), InfixExpression.Operator.CONDITIONAL_OR, b.createCopyTarget(elseExpression));
                }

                return b.infixExpression(b.negate(condition, Copy.COPY), InfixExpression.Operator.CONDITIONAL_AND, b.createCopyTarget(elseExpression));
            }

            if (thenLiteral == null && elseLiteral != null) {
                if (elseLiteral) {
                    return b.infixExpression(b.negate(condition, Copy.COPY), InfixExpression.Operator.CONDITIONAL_OR, b.createCopyTarget(thenExpression));
                }

                return b.infixExpression(b.createCopyTarget(condition), InfixExpression.Operator.CONDITIONAL_AND, b.createCopyTarget(thenExpression));
            }
        }

        return null;
    }

    private Expression getExpression(final Expression condition, final String expressionTypeName, final Name booleanName) {
        if (boolean.class.getSimpleName().equals(expressionTypeName)) {
            return condition;
        }

        if (getJavaMinorVersion() >= 4 && Boolean.class.getCanonicalName().equals(expressionTypeName)) {
            return b.invoke(booleanName, "valueOf", condition); //$NON-NLS-1$
        }

        return null;
    }

    private Name getBooleanName(final ASTNode node) {
        if (!isSimpleNameAlreadyUsed(Boolean.class.getSimpleName(), ASTNodes.getAncestor(node, CompilationUnit.class))) {
            return b.simpleName(Boolean.class.getSimpleName());
        }

        return b.name(Boolean.class.getCanonicalName());
    }

    private boolean isSimpleNameAlreadyUsed(final String simpleName, final CompilationUnit cu) {
        for (ImportDeclaration id : ASTNodes.imports(cu)) {
            if (!(id.getName() instanceof QualifiedName)) {
                throw new NotImplementedException(id.getName());
            }

            QualifiedName f= (QualifiedName) id.getName();

            if (simpleName.equals(f.getName().getIdentifier())) {
                return true;
            }
        }

        return false;
    }

    private boolean visitIfStatement(final IfStatement node) {
        Expression ifCondition= node.getExpression();

        if (ASTNodes.isPassive(ifCondition)) {
            BooleanASTMatcher matcher= new BooleanASTMatcher();

            if (ASTNodes.match(matcher, node.getThenStatement(), node.getElseStatement())
                    && (matcher.matches.size() <= 1 || ifCondition instanceof Name || ifCondition instanceof FieldAccess)) {
                // Then and else statements are matching, bar the boolean values
                // which are opposite
                Statement copyStatement= b.copySubtree(node.getThenStatement());
                // Identify the node that needs to be replaced after the copy
                BooleanASTMatcher matcher2= new BooleanASTMatcher(matcher.matches);

                if (ASTNodes.match(matcher2, copyStatement, node.getElseStatement())) {
                    Refactorings r = cuRewrite.getRefactorings();

                    copyStatement.accept(
                            new BooleanReplaceVisitor(ifCondition, matcher2.matches.values(), getBooleanName(node)));

                    if (!ASTNodes.canHaveSiblings(node)) {
                        // Make sure to keep curly braces if the node is an else statement
                        r.replace(node, copyStatement);
                        return false;
                    }

                    if (!ASTNodes.hasVariableConflict(node, node.getThenStatement())) {
                        List<Statement> statementsToMove= ASTNodes.asList(copyStatement);

                        for (int i= statementsToMove.size() - 1; i > 0; i--) {
                            r.insertAfter(statementsToMove.get(i), node);
                        }

                        r.replace(node, statementsToMove.get(0));
                        return false;
                    }
                }
            }
        }

        ReturnStatement thenRs= ASTNodes.as(node.getThenStatement(), ReturnStatement.class);

        if (thenRs != null) {
            ReturnStatement elseRs= ASTNodes.as(
                    node.getElseStatement() != null ? node.getElseStatement() : ASTNodes.getNextSibling(node),
                    ReturnStatement.class);

            return elseRs == null || withThenReturnStatement(node, thenRs, elseRs);
        }

        return noThenReturnStatement(node);
    }
}

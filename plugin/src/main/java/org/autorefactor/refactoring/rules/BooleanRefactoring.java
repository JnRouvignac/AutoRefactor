/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2014 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.util.IllegalArgumentException;
import org.autorefactor.util.IllegalStateException;
import org.autorefactor.util.NotImplementedException;
import org.eclipse.jdt.core.dom.ASTMatcher;
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
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.PrimitiveType;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.StructuralPropertyDescriptor;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.ASTNode.*;

/**
 * Boolean related refactorings:
 * <ul>
 * <li>Use boolean constants when possible</li>
 * <li>Remove if statements when each clause does similar things with opposite
 * boolean values</li>
 * <li>Remove ternary operators when each clause does similar things with
 * opposite boolean values</li>
 * </ul>
 */
public class BooleanRefactoring extends AbstractRefactoring {

    private static class BooleanASTMatcher extends ASTMatcher {

        /** else node to then node. */
        final Map<ASTNode, ASTNode> matches = new HashMap<ASTNode, ASTNode>();
        final Map<ASTNode, ASTNode> previousMatches;

        public BooleanASTMatcher() {
            this(null);
        }

        public BooleanASTMatcher(Map<ASTNode, ASTNode> previousMatches) {
            if (previousMatches != null) {
                this.previousMatches = previousMatches;
            } else {
                this.previousMatches = Collections.emptyMap();
            }
        }

        @Override
        public boolean match(BooleanLiteral node, Object other) {
            if (other instanceof Expression) {
                final Expression expr = (Expression) other;
                if (areOppositeBooleanValues(node, expr)) {
                    matches.put(expr, node);
                    return true;
                }
            }
            return false;
        }

        /** Compares mixed boolean literal and Boolean object values against each other. */
        private boolean areOppositeBooleanValues(Expression expr1, Expression expr2) {
            final Boolean b1 = getBooleanLiteral(expr1);
            final Boolean b2 = getBooleanLiteral(expr2);
            return b1 != null && b2 != null && !b1.equals(b2);
        }

        @Override
        public boolean match(QualifiedName node, Object other) {
            if (other instanceof Expression) {
                final Expression expr = (Expression) other;
                if (this.previousMatches.containsKey(other)
                        || areOppositeBooleanValues(node, expr)) {
                    matches.put(expr, node);
                    return true;
                }
            }
            return false;
        }
    }

    private class BooleanReplaceVisitor extends ASTVisitor {

        private final Expression ifCondition;
        private final Collection<ASTNode> nodesToReplace;
        private final Name booleanName;

        public BooleanReplaceVisitor(Expression ifCondition,
                Collection<ASTNode> nodesToReplace, Name booleanName) {
            this.ifCondition = ifCondition;
            this.nodesToReplace = nodesToReplace;
            this.booleanName = booleanName;
        }

        @Override
        public boolean visit(BooleanLiteral node) {
            if (this.nodesToReplace.contains(node)) {
                final Expression expr = getExpression(getBooleanLiteral(node),
                        this.ifCondition, "boolean", null);
                replaceInParent(node, expr);
            }
            return DO_NOT_VISIT_SUBTREE;
        }

        @Override
        public boolean visit(QualifiedName node) {
            if (this.nodesToReplace.contains(node)) {
                final QualifiedName qn = as(node, QualifiedName.class);
                final Expression expr = getExpression(getBooleanObjectAsLiteral(qn),
                        this.ifCondition, "java.lang.Boolean", this.booleanName);
                replaceInParent(node, expr);
            }
            return DO_NOT_VISIT_SUBTREE;
        }

        private void replaceInParent(ASTNode nodeToReplace, ASTNode replacementNode) {
            if (nodeToReplace.getParent() == null) {
                throw new IllegalArgumentException(nodeToReplace, "The node to replace does not have a parent");
            }
            final StructuralPropertyDescriptor locationInParent = nodeToReplace.getLocationInParent();
            if (locationInParent instanceof ChildPropertyDescriptor) {
                final ChildPropertyDescriptor cpd = (ChildPropertyDescriptor) locationInParent;
                nodeToReplace.getParent().setStructuralProperty(cpd, replacementNode);
            } else if (locationInParent instanceof ChildListPropertyDescriptor) {
                final ChildListPropertyDescriptor clpd = (ChildListPropertyDescriptor) locationInParent;
                @SuppressWarnings("unchecked")
                final List<ASTNode> property = (List<ASTNode>) nodeToReplace.getParent().getStructuralProperty(clpd);
                property.set(property.indexOf(nodeToReplace), replacementNode);
            } else {
                throw new NotImplementedException(nodeToReplace, locationInParent);
            }
        }
    }

    private ASTBuilder b;

    /** {@inheritDoc} */
    @Override
    public void setRefactoringContext(RefactoringContext ctx) {
        super.setRefactoringContext(ctx);
        b = ctx.getASTBuilder();
    }

    private int getJavaMinorVersion() {
        return ctx.getJavaProjectOptions().getJavaSERelease().getMinorVersion();
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ConditionalExpression node) {
        final ITypeBinding typeBinding = node.resolveTypeBinding();
        if (typeBinding != null) {
            final Expression newE = maybeGetExpression(
                    typeBinding.getQualifiedName(), node.getExpression(),
                    getBooleanLiteral(node.getThenExpression()),
                    getBooleanLiteral(node.getElseExpression()));
            if (newE != null) {
                ctx.getRefactorings().replace(node, newE);
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(IfStatement node) {
        final BooleanASTMatcher matcher = new BooleanASTMatcher();
        if (match(matcher, node.getThenStatement(), node.getElseStatement())) {
            // Then and else statement are matching, bar the boolean values
            // which are opposite
            final Statement copyStmt = b.copySubtree(node.getThenStatement());
            // identify the node that need to be replaced after the copy
            final BooleanASTMatcher matcher2 = new BooleanASTMatcher(matcher.matches);
            if (match(matcher2, copyStmt, node.getElseStatement())) {
                final Expression ifCondition = node.getExpression();
                copyStmt.accept(new BooleanReplaceVisitor(ifCondition,
                        matcher2.matches.values(), getBooleanName(node)));
                // make sure to keep curly braces if the node is an else statement
                ctx.getRefactorings().replace(node,
                    isElseStatementOfParent(node) ? copyStmt : toSingleStmt(copyStmt));
                return DO_NOT_VISIT_SUBTREE;
            }
        }

        final ReturnStatement thenRs = as(node.getThenStatement(), ReturnStatement.class);
        if (thenRs != null) {
            if (node.getElseStatement() == null) {
                // The != null case is handled with the matcher above
                final ReturnStatement rs = as(getNextSibling(node), ReturnStatement.class);
                if (rs != null) {
                    final Boolean thenBool = getBooleanLiteral(thenRs.getExpression());
                    final Boolean elseBool = getBooleanLiteral(rs.getExpression());
                    ReturnStatement newRs = getReturnStatement(node, thenBool, elseBool);
                    if (newRs != null) {
                        ctx.getRefactorings().replace(node, newRs);
                        ctx.getRefactorings().remove(rs);
                        return DO_NOT_VISIT_SUBTREE;
                    }
                    final MethodDeclaration md = getAncestor(node, MethodDeclaration.class);
                    final Type returnType = md.getReturnType2();
                    if (returnType != null && returnType.isPrimitiveType()) {
                        final PrimitiveType pt = (PrimitiveType) returnType;
                        if (PrimitiveType.BOOLEAN.equals(pt.getPrimitiveTypeCode())) {
                            newRs = getReturnStatement(node, thenBool, elseBool,
                                    thenRs.getExpression(), rs.getExpression());
                            if (newRs != null) {
                                ctx.getRefactorings().replace(node, newRs);
                                ctx.getRefactorings().remove(rs);
                                return DO_NOT_VISIT_SUBTREE;
                            }
                        }
                    }
                }
            }
        } else {
            final Assignment thenA = asExpression(node.getThenStatement(), Assignment.class);
            if (thenA != null
                    && asList(node.getElseStatement()).isEmpty()
                    && Assignment.Operator.ASSIGN.equals(thenA.getOperator())
                    && (thenA.getLeftHandSide() instanceof Name
                        || thenA.getLeftHandSide() instanceof FieldAccess)) {
                final Statement previousSibling = getPreviousSibling(node);
                if (previousSibling instanceof VariableDeclarationStatement) {
                    final VariableDeclarationStatement vds = (VariableDeclarationStatement) previousSibling;
                    VariableDeclarationFragment vdf = getVariableDeclarationFragment(vds, thenA.getLeftHandSide());
                    if (vdf != null) {
                        final ITypeBinding typeBinding = vds.getType().resolveBinding();
                        return replace(node, thenA, typeBinding, vdf.getInitializer());
                    }
                } else if (previousSibling instanceof ExpressionStatement) {
                    final Assignment elseA = asExpression(previousSibling, Assignment.class);
                    if (elseA != null
                            && Assignment.Operator.ASSIGN.equals(elseA.getOperator())
                            && isSameVariable(
                                    thenA.getLeftHandSide(),
                                    elseA.getLeftHandSide())) {
                        final ITypeBinding typeBinding = elseA.resolveTypeBinding();
                        return replace(node, thenA, typeBinding, elseA.getRightHandSide());
                    }
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean isElseStatementOfParent(IfStatement node) {
        final ASTNode parent = node.getParent();
        if (parent instanceof IfStatement) {
            final IfStatement is = (IfStatement) parent;
            return is.getElseStatement().equals(node);
        }
        return false;
    }

    private boolean replace(IfStatement node, Assignment a, ITypeBinding typeBinding, Expression rightHandSide) {
        if (typeBinding == null) {
            return VISIT_SUBTREE;
        }
        final String expressionTypeName = typeBinding.getQualifiedName();
        final Expression newE = maybeGetExpression(
                expressionTypeName,
                node.getExpression(),
                getBooleanLiteral(a.getRightHandSide()),
                getBooleanLiteral(rightHandSide));
        if (newE != null) {
            ctx.getRefactorings().replace(rightHandSide, newE);
            ctx.getRefactorings().remove(node);
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private Statement toSingleStmt(final Statement stmt) {
        if (stmt instanceof Block) {
            final List<Statement> stmts = asList(stmt);
            if (stmts.size() == 1) {
                return stmts.get(0);
            }
        }
        return stmt;
    }

    private ReturnStatement getReturnStatement(IfStatement node,
            Boolean thenBool, Boolean elseBool, Expression thenExpr, Expression elseExpr) {
        if (thenBool == null && elseBool != null) {
            final Expression leftOp = negateIfNeeded(
                    b.parenthesize(b.copy(node.getExpression())), elseBool.booleanValue());
            return b.return0(b.infixExpr(
                    leftOp,
                    getConditionalOperator(elseBool.booleanValue()),
                    b.parenthesize(b.copy(thenExpr))));
        } else if (thenBool != null && elseBool == null) {
            final Expression leftOp = negateIfNeeded(
                    b.parenthesize(b.copy(node.getExpression())), !thenBool.booleanValue());
            return b.return0(b.infixExpr(
                    leftOp,
                    getConditionalOperator(thenBool.booleanValue()),
                    b.parenthesize(b.copy(elseExpr))));
        }
        return null;
    }

    private Operator getConditionalOperator(boolean isOrOperator) {
        return isOrOperator ? InfixExpression.Operator.CONDITIONAL_OR
                : InfixExpression.Operator.CONDITIONAL_AND;
    }

    private Expression negateIfNeeded(Expression ie, boolean negate) {
        if (negate) {
            return negate(ie, false);
        }
        return ie;
    }

    private VariableDeclarationFragment getVariableDeclarationFragment(
            final VariableDeclarationStatement vds, final Expression expr) {
        if (vds == null) {
            return null;
        }
        for (VariableDeclarationFragment vdf : fragments(vds)) {
            if (isSameVariable(expr, vdf.getName())) {
                return vdf;
            }
        }
        return null;
    }

    private ReturnStatement getReturnStatement(IfStatement node,
            final Boolean returnThenLiteral, final Boolean returnElseLiteral) {
        if (areOppositeValues(returnThenLiteral, returnElseLiteral)) {
            Expression exprToReturn = b.copy(node.getExpression());
            if (returnElseLiteral) {
                exprToReturn = negate(exprToReturn, false);
            }

            final MethodDeclaration md = getAncestor(node, MethodDeclaration.class);
            final Expression returnExpr = getReturnExpression(md, exprToReturn);
            if (returnExpr != null) {
                return b.return0(returnExpr);
            }
        }
        return null;
    }

    private boolean areOppositeValues(final Boolean b1, final Boolean b2) {
        return b1 != null && b2 != null && !b1.equals(b2);
    }

    private Expression getReturnExpression(MethodDeclaration md, Expression ifCondition) {
        final IMethodBinding methodBinding = md.resolveBinding();
        if (methodBinding == null) {
            return null;
        }
        final String qualifiedName = methodBinding.getReturnType().getQualifiedName();
        final Expression newE = getExpression(ifCondition, qualifiedName, getBooleanName(md));
        if (newE != null) {
            return newE;
        }
        // TODO JNR rejuggle exception messages like this:
        // compilationUnit.java:line number: error message
        throw new IllegalStateException(md,
                "Did not expect any other return type than boolean or java.lang.Boolean for method "
                        + md.getName().getIdentifier() + ", but found " + qualifiedName);
    }

    private Expression maybeGetExpression(String expressionName, Expression ifCondition, Boolean thenBoolLiteral,
            Boolean elseBoolLiteral) {
        if (areOppositeValues(thenBoolLiteral, elseBoolLiteral)) {
            final Name booleanName = getBooleanName(ifCondition);
            return getExpression(thenBoolLiteral, ifCondition, expressionName, booleanName);
        }
        return null;
    }

    private Expression getExpression(final boolean doNotNegate, final Expression ifCondition, String expressionName,
            final Name booleanName) {
        if (doNotNegate) {
            return getExpression(b.copy(ifCondition), expressionName, booleanName);
        }
        final Expression negatedIfCondition = negate(ifCondition, true);
        return getExpression(negatedIfCondition, expressionName, booleanName);
    }

    private Expression negate(Expression expr, boolean doCopy) {
        if (expr.getNodeType() == PREFIX_EXPRESSION) {
            final PrefixExpression pe = (PrefixExpression) expr;
            if (PrefixExpression.Operator.NOT.equals(pe.getOperator())) {
                final Expression expr2 = removeParentheses(pe.getOperand());
                return doCopy ? b.copy(expr2) : expr2;
            }
        }

        return b.not(b.parenthesizeIfNeeded(doCopy ? b.copy(expr) : expr));
    }

    private Expression getExpression(Expression ifCondition, String expressionTypeName, Name booleanName) {
        if ("boolean".equals(expressionTypeName)) {
            return ifCondition;
        } else if (getJavaMinorVersion() >= 4
                && "java.lang.Boolean".equals(expressionTypeName)) {
            return b.invoke(booleanName, "valueOf", ifCondition);
        }
        return null;
    }

    private Name getBooleanName(ASTNode node) {
        if (!isSimpleNameAlreadyUsed("Boolean", getAncestor(node, CompilationUnit.class))) {
            return b.name("Boolean");
        }
        return b.name("java.lang.Boolean");
    }

    private boolean isSimpleNameAlreadyUsed(String simpleName, CompilationUnit cu) {
        for (ImportDeclaration id : imports(cu)) {
            if (id.getName() instanceof QualifiedName) {
                QualifiedName f = (QualifiedName) id.getName();
                if (simpleName.equals(f.getName().getIdentifier())) {
                    return true;
                }
            } else {
                throw new NotImplementedException(id.getName());
            }
        }
        return false;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(MethodInvocation node) {
        if (isMethod(node, "java.lang.Boolean", "valueOf", "java.lang.String")
                || isMethod(node, "java.lang.Boolean", "valueOf", "boolean")) {
            final BooleanLiteral l = as(arguments(node), BooleanLiteral.class);
            if (l != null) {
                ctx.getRefactorings().replace(node,
                        getRefactoring(node, l.booleanValue()));
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    private FieldAccess getRefactoring(MethodInvocation node, boolean booleanLiteral) {
        final FieldAccess fa = b.getAST().newFieldAccess();
        if (node.getExpression() instanceof Name) {
            fa.setExpression(b.copy(node.getExpression()));
        }
        fa.setName(b.simpleName(booleanLiteral ? "TRUE" : "FALSE"));
        return fa;
    }
}

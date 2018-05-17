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
package org.autorefactor.refactoring.rules;

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.as;
import static org.autorefactor.refactoring.ASTHelper.asExpression;
import static org.autorefactor.refactoring.ASTHelper.asList;
import static org.autorefactor.refactoring.ASTHelper.fragments;
import static org.autorefactor.refactoring.ASTHelper.getAncestor;
import static org.autorefactor.refactoring.ASTHelper.getBooleanLiteral;
import static org.autorefactor.refactoring.ASTHelper.getBooleanObject;
import static org.autorefactor.refactoring.ASTHelper.getNextSibling;
import static org.autorefactor.refactoring.ASTHelper.getPreviousSibling;
import static org.autorefactor.refactoring.ASTHelper.hasOperator;
import static org.autorefactor.refactoring.ASTHelper.imports;
import static org.autorefactor.refactoring.ASTHelper.isPassive;
import static org.autorefactor.refactoring.ASTHelper.isPrimitive;
import static org.autorefactor.refactoring.ASTHelper.isSameVariable;
import static org.autorefactor.refactoring.ASTHelper.match;
import static org.eclipse.jdt.core.dom.Assignment.Operator.ASSIGN;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.CONDITIONAL_AND;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.CONDITIONAL_OR;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.ASTBuilder.Copy;
import org.autorefactor.refactoring.ASTSemanticMatcher;
import org.autorefactor.refactoring.BlockSubVisitor;
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
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
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
public class BooleanRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Boolean";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
                + "Boolean related refactorings:\n"
                + "- remove if statements when then and else clauses do similar things with opposite boolean values,\n"
                + "- remove ternary operators when then and else clauses do similar things with opposite boolean values"
                + ",\n"
                + "- simplify boolean expressions.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces code to focus attention on code that matters.";
    }

    private class BooleanASTMatcher extends ASTSemanticMatcher {

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
            return super.match(node, other);
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
            return super.match(node, other);
        }
    }

    private final class AssignmentIfAndReturnVisitor extends BlockSubVisitor {

        public AssignmentIfAndReturnVisitor(final RefactoringContext ctx, final Block startNode) {
            super(ctx, startNode);
        }

        @Override
        public boolean visit(IfStatement node) {
            final boolean result = visitIfStatement(node);
            if (result == DO_NOT_VISIT_SUBTREE) {
                setResult(DO_NOT_VISIT_SUBTREE);
            }
            return result;
        }
    }

    @Override
    public boolean visit(Block node) {
        final AssignmentIfAndReturnVisitor assignmentIfAndReturnVisitor = new AssignmentIfAndReturnVisitor(ctx, node);
        node.accept(assignmentIfAndReturnVisitor);
        return assignmentIfAndReturnVisitor.getResult();
    }

    /** Compares mixed boolean literal and Boolean object values against each other. */
    private static boolean areOppositeBooleanValues(Expression expr1, Expression expr2) {
        final Boolean b1 = getBooleanLiteral(expr1);
        final Boolean b2 = getBooleanLiteral(expr2);
        return b1 != null && b2 != null && !b1.equals(b2);
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
                Boolean booleanValue = getBooleanLiteral(node);
                Expression orientedCondition;
                if (booleanValue) {
                    orientedCondition = b.copy(ifCondition);
                } else {
                    orientedCondition = b.negate(ifCondition, Copy.COPY);
                }
                final Expression expr = getExpression(orientedCondition, "boolean", null);
                replaceInParent(node, expr);
            }
            return DO_NOT_VISIT_SUBTREE;
        }

        @Override
        public boolean visit(QualifiedName node) {
            if (this.nodesToReplace.contains(node)) {
                final QualifiedName qn = as(node, QualifiedName.class);
                Boolean booleanValue = getBooleanObject(qn);
                if (booleanValue != null) {
                    Expression orientedCondition;
                    if (booleanValue) {
                        orientedCondition = b.copy(ifCondition);
                    } else {
                        orientedCondition = b.negate(ifCondition, Copy.COPY);
                    }
                    final Expression expr = getExpression(orientedCondition, "java.lang.Boolean", booleanName);
                    replaceInParent(node, expr);
                }
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

    @Override
    public void setRefactoringContext(RefactoringContext ctx) {
        super.setRefactoringContext(ctx);
        b = ctx.getASTBuilder();
    }

    private int getJavaMinorVersion() {
        return ctx.getJavaProjectOptions().getJavaSERelease().getMinorVersion();
    }

    @Override
    public boolean visit(ConditionalExpression node) {
        final ITypeBinding typeBinding = node.resolveTypeBinding();
        if (typeBinding != null) {
            final Expression newE = newExpressionOrNull(typeBinding.getQualifiedName(), node.getExpression(),
                    node.getThenExpression(), node.getElseExpression());
            if (newE != null) {
                ctx.getRefactorings().replace(node, newE);
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean withThenReturnStmt(IfStatement node, ReturnStatement thenRs, ReturnStatement elseRs) {
        ReturnStatement newRs = getReturnStatement(node, thenRs.getExpression(), elseRs.getExpression());
        if (newRs != null) {
            ctx.getRefactorings().replace(node, newRs);
            ctx.getRefactorings().remove(elseRs);
            return DO_NOT_VISIT_SUBTREE;
        }
        final MethodDeclaration md = getAncestor(node, MethodDeclaration.class);
        final Type returnType = md.getReturnType2();
        if (returnType != null && returnType.isPrimitiveType()) {
            final PrimitiveType pt = (PrimitiveType) returnType;
            if (PrimitiveType.BOOLEAN.equals(pt.getPrimitiveTypeCode())) {
                final Boolean thenBool = getBooleanLiteral(thenRs.getExpression());
                final Boolean elseBool = getBooleanLiteral(elseRs.getExpression());
                newRs = getReturnStatement(node, thenBool, elseBool, thenRs.getExpression(), elseRs.getExpression());
                if (newRs != null) {
                    ctx.getRefactorings().replace(node, newRs);
                    ctx.getRefactorings().remove(elseRs);
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean noThenReturnStmt(final IfStatement node) {
        final Assignment thenA = asExpression(node.getThenStatement(), Assignment.class);
        if (hasOperator(thenA, ASSIGN)
                && asList(node.getElseStatement()).isEmpty()
                && (thenA.getLeftHandSide() instanceof Name
                        || thenA.getLeftHandSide() instanceof FieldAccess)) {
            final Statement previousSibling = getPreviousSibling(node);
            if (previousSibling instanceof VariableDeclarationStatement) {
                final VariableDeclarationStatement vds = (VariableDeclarationStatement) previousSibling;
                VariableDeclarationFragment vdf = getVariableDeclarationFragment(vds, thenA.getLeftHandSide());
                if (vdf != null) {
                    final VariableDefinitionsUsesVisitor variableUseVisitor =
                        new VariableDefinitionsUsesVisitor(vdf.resolveBinding(), node.getExpression()).find();
                    if (variableUseVisitor.getUses().isEmpty()) {
                        final ITypeBinding typeBinding = vds.getType().resolveBinding();
                        return maybeReplace(node, thenA, typeBinding, vdf.getInitializer());
                    }
                }
            } else if (previousSibling instanceof ExpressionStatement) {
                final Assignment elseA = asExpression(previousSibling, Assignment.class);
                if (hasOperator(elseA, ASSIGN)
                        && isSameVariable(
                                thenA.getLeftHandSide(),
                                elseA.getLeftHandSide())) {
                    final ITypeBinding typeBinding = elseA.resolveTypeBinding();
                    return maybeReplace(node, thenA, typeBinding, elseA.getRightHandSide());
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

    private boolean maybeReplace(IfStatement node, Assignment a, ITypeBinding typeBinding, Expression rightHandSide) {
        if (typeBinding != null) {
            final String expressionTypeName = typeBinding.getQualifiedName();
            final Expression newE =
                    newExpressionOrNull(expressionTypeName, node.getExpression(), a.getRightHandSide(), rightHandSide);
            if (newE != null) {
                ctx.getRefactorings().replace(rightHandSide, newE);
                ctx.getRefactorings().remove(node);
                return DO_NOT_VISIT_SUBTREE;
            }
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
            final Expression leftOp = signExpr(
                    b.parenthesizeIfNeeded(b.copy(node.getExpression())), !elseBool.booleanValue());
            return b.return0(b.infixExpr(
                    leftOp,
                    getConditionalOperator(elseBool.booleanValue()),
                    b.parenthesizeIfNeeded(b.copy(thenExpr))));
        } else if (thenBool != null && elseBool == null) {
            final Expression leftOp = signExpr(
                    b.parenthesizeIfNeeded(b.copy(node.getExpression())), thenBool.booleanValue());
            return b.return0(b.infixExpr(
                    leftOp,
                    getConditionalOperator(thenBool.booleanValue()),
                    b.parenthesizeIfNeeded(b.copy(elseExpr))));
        }
        return null;
    }

    private Operator getConditionalOperator(boolean isOrOperator) {
        return isOrOperator ? CONDITIONAL_OR : CONDITIONAL_AND;
    }

    private Expression signExpr(Expression ie, boolean isPositive) {
        if (isPositive) {
            return ie;
        }
        return b.negate(ie, Copy.NONE);
    }

    private VariableDeclarationFragment getVariableDeclarationFragment(
            final VariableDeclarationStatement vds, final Expression expr) {
        if (vds == null) {
            return null;
        }
        for (VariableDeclarationFragment vdf : fragments(vds)) {
            if (isSameVariable(expr, vdf)) {
                return vdf;
            }
        }
        return null;
    }

    private ReturnStatement getReturnStatement(
            final IfStatement node, final Expression thenExpression, final Expression elseExpression) {
        if (areOppositeBooleanValues(thenExpression, elseExpression)) {
            Expression exprToReturn = b.copy(node.getExpression());
            if (getBooleanLiteral(elseExpression)) {
                exprToReturn = b.negate(exprToReturn, Copy.NONE);
            }

            final MethodDeclaration md = getAncestor(node, MethodDeclaration.class);
            final Expression returnExpr = getReturnExpression(md, exprToReturn);
            if (returnExpr != null) {
                return b.return0(returnExpr);
            }
        }
        return null;
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

    private Expression newExpressionOrNull(
            String expressionTypeName, Expression condition, Expression thenExpression, Expression elseExpression) {
        Boolean thenLiteral = getBooleanLiteral(thenExpression);
        Boolean elseLiteral = getBooleanLiteral(elseExpression);

        if (areOppositeBooleanValues(thenExpression, elseExpression)) {
            final Name booleanName = getBooleanName(condition);
            Expression orientedCondition;
            if (thenLiteral) {
                orientedCondition = b.copy(condition);
            } else {
                orientedCondition = b.negate(condition, Copy.COPY);
            }
            return getExpression(orientedCondition, expressionTypeName, booleanName);
        } else if ((isPrimitive(thenExpression) || isPrimitive(elseExpression))
                && ("boolean".equals(expressionTypeName)
                        || "java.lang.Boolean".equals(expressionTypeName))) {
            // If both expressions are primitive, there cannot be any NPE
            // If only one expression is primitive, a NPE is already possible so we do not care
            if (thenLiteral != null && elseLiteral == null) {
                if (thenLiteral) {
                    return b.infixExpr(
                            b.copy(condition), CONDITIONAL_OR, b.copy(elseExpression));
                } else {
                    return b.infixExpr(
                            b.negate(condition, Copy.COPY), CONDITIONAL_AND, b.copy(elseExpression));
                }
            } else if (thenLiteral == null && elseLiteral != null) {
                if (elseLiteral) {
                    return b.infixExpr(
                            b.negate(condition, Copy.COPY), CONDITIONAL_OR, b.copy(thenExpression));
                } else {
                    return b.infixExpr(
                            b.copy(condition), CONDITIONAL_AND, b.copy(thenExpression));
                }
            }
        }
        return null;
    }

    private Expression getExpression(Expression condition, String expressionTypeName, Name booleanName) {
        if ("boolean".equals(expressionTypeName)) {
            return condition;
        } else if (getJavaMinorVersion() >= 4
                && "java.lang.Boolean".equals(expressionTypeName)) {
            return b.invoke(booleanName, "valueOf", condition);
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

    private boolean visitIfStatement(final IfStatement node) {
        if (isPassive(node.getExpression())) {
            final BooleanASTMatcher matcher = new BooleanASTMatcher();

            if (match(matcher, node.getThenStatement(), node.getElseStatement())) {
                if (matcher.matches.size() <= 1
                        || node.getExpression() instanceof Name) {
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
            }
        }

        final ReturnStatement thenRs = as(node.getThenStatement(), ReturnStatement.class);

        if (thenRs != null) {
            final ReturnStatement elseRs = as(node.getElseStatement() != null
                    ? node.getElseStatement()
                            : getNextSibling(node), ReturnStatement.class);

            if (elseRs != null) {
                return withThenReturnStmt(node, thenRs, elseRs);
            }
            return VISIT_SUBTREE;
        } else {
            return noThenReturnStmt(node);
        }
    }
}

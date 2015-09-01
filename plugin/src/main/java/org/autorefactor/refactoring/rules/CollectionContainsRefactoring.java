/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.ForLoopHelper.ForLoopContent;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.ASTMatcher;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Assignment.Operator;
import org.eclipse.jdt.core.dom.BooleanLiteral;
import org.eclipse.jdt.core.dom.BreakStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.autorefactor.refactoring.ForLoopHelper.*;
import static org.autorefactor.refactoring.ForLoopHelper.ContainerType.*;
import static org.autorefactor.refactoring.ForLoopHelper.IterationType.*;
import static org.eclipse.jdt.core.dom.Assignment.Operator.*;

/** See {@link #getDescription()} method. */
public class CollectionContainsRefactoring extends AbstractRefactoringRule {

    @Override
    public String getDescription() {
        return "Replace loop with Collection.contains(Object obj).";
    }

    @Override
    public String getName() {
        return "Use Collection.contains()";
    }

    @Override
    public boolean visit(EnhancedForStatement node) {
        final Expression iterable = node.getExpression();
        final SingleVariableDeclaration loopVariable = node.getParameter();
        final IfStatement is = uniqueStmtAs(node.getBody(), IfStatement.class);
        return maybeReplaceWithCollectionContains(node, iterable, loopVariable.getName(), is);
    }

    private boolean maybeReplaceWithCollectionContains(
            Statement forNode, Expression iterable, Expression loopElement, IfStatement is) {
        if (is != null
                && is.getElseStatement() == null
                && instanceOf(iterable, "java.util.Collection")) {
            MethodInvocation cond = as(is.getExpression(), MethodInvocation.class);
            List<Statement> thenStmts = asList(is.getThenStatement());
            if (!thenStmts.isEmpty()
                    && isMethod(cond, "java.lang.Object", "equals", "java.lang.Object")) {
                Expression toFind = getExpressionToFind(cond, loopElement);
                if (toFind != null) {
                    if (thenStmts.size() == 1) {
                        Statement forNextStmt = getNextStatement(forNode);
                        Statement thenStmt = thenStmts.get(0);
                        Boolean negate = negateCollectionContains(thenStmt, forNextStmt);
                        if (negate != null) {
                            ASTBuilder b = ctx.getASTBuilder();
                            ctx.getRefactorings().replace(forNode,
                                b.return0(
                                    collectionContains(iterable, toFind, negate, b)));
                            if (forNextStmt.equals(getNextSibling(forNode))) {
                                ctx.getRefactorings().remove(forNextStmt);
                            }
                            return DO_NOT_VISIT_SUBTREE;
                        }
                        return maybeReplaceWithCollectionContains0(forNode, iterable, thenStmt, toFind);
                    } else if (thenStmts.size() == 2) {
                        BreakStatement bs = as(thenStmts.get(1), BreakStatement.class);
                        if (bs != null && bs.getLabel() == null) {
                            return maybeReplaceWithCollectionContains0(forNode, iterable, thenStmts.get(0), toFind);
                        }
                    }
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean maybeReplaceWithCollectionContains0(
            Statement forNode, Expression iterable, Statement uniqueThenStmt, Expression toFind) {
        Statement previousStmt = getPreviousStatement(forNode);
        boolean previousStmtIsPreviousSibling = previousStmt.equals(getPreviousSibling(forNode));
        Assignment as = asExpression(uniqueThenStmt, Assignment.class);
        Pair<Name, Expression> innerInit = decomposeInitializer(as);
        Name initName = innerInit.getFirst();
        Expression init2 = innerInit.getSecond();
        Pair<Name, Expression> outerInit = getInitializer(previousStmt);
        if (isSameVariable(outerInit.getFirst(), initName)) {
            Boolean negate2 = negateCollectionContains((BooleanLiteral) init2, (BooleanLiteral) outerInit.getSecond());
            if (negate2 != null) {
                ASTBuilder b = ctx.getASTBuilder();
                Statement replacement;
                if (previousStmtIsPreviousSibling
                        && previousStmt instanceof VariableDeclarationStatement) {
                    replacement = b.declare("boolean", b.copy((SimpleName) initName),
                        collectionContains(iterable, toFind, negate2, b));
                } else if (!previousStmtIsPreviousSibling
                        || previousStmt instanceof ExpressionStatement) {
                    replacement = b.toStmt(b.assign(b.copy(initName), ASSIGN,
                        collectionContains(iterable, toFind, negate2, b)));
                } else {
                    throw new NotImplementedException(forNode);
                }
                ctx.getRefactorings().replace(forNode, replacement);
                if (previousStmtIsPreviousSibling) {
                    ctx.getRefactorings().remove(previousStmt);
                }
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    private Expression collectionContains(Expression iterable, Expression toFind, Boolean negate, ASTBuilder b) {
        return maybeNegate(
            b.invoke(b.move(iterable), "contains", b.move(toFind)),
            negate);
    }

    private Boolean negateCollectionContains(Statement uniqueThenStmt, Statement foreachNextStmt) {
        BooleanLiteral innerBl = getReturnedBooleanLiteral(uniqueThenStmt);
        BooleanLiteral outerBl = getReturnedBooleanLiteral(foreachNextStmt);
        return negateCollectionContains(innerBl, outerBl);
    }

    private Boolean negateCollectionContains(BooleanLiteral innerBl, BooleanLiteral outerBl) {
        if (innerBl != null
                && outerBl != null
                && !innerBl.booleanValue() == outerBl.booleanValue()) {
            return outerBl.booleanValue();
        }
        return null;
    }

    private Pair<Name, Expression> getInitializer(Statement stmt) {
        if (stmt instanceof VariableDeclarationStatement) {
            VariableDeclarationStatement vds = (VariableDeclarationStatement) stmt;
            List<VariableDeclarationFragment> fragments = fragments(vds);
            if (fragments.size() == 1) {
                VariableDeclarationFragment fragment = fragments.get(0);
                return Pair.of((Name) fragment.getName(), fragment.getInitializer());
            }
        } else if (stmt instanceof ExpressionStatement) {
            Assignment as = asExpression(stmt, Assignment.class);
            if (hasOperator(as, Operator.ASSIGN)
                    && as.getLeftHandSide() instanceof Name) {
                return Pair.of((Name) as.getLeftHandSide(), as.getRightHandSide());
            }
        }
        return Pair.empty();
    }

    private IfStatement uniqueStmtAs(Statement stmt, Class<IfStatement> stmtClazz) {
        return as(uniqueStmt(asList(stmt)), stmtClazz);
    }

    private Statement uniqueStmt(List<Statement> stmts) {
        if (stmts.size() == 1) {
            return stmts.get(0);
        }
        return null;
    }

    private BooleanLiteral getReturnedBooleanLiteral(Statement stmt) {
        ReturnStatement rs = as(stmt, ReturnStatement.class);
        if (rs != null) {
            return as(rs.getExpression(), BooleanLiteral.class);
        }
        return null;
    }

    private Expression maybeNegate(Expression invoke, boolean negate) {
        if (negate) {
            return ctx.getASTBuilder().not(invoke);
        }
        return invoke;
    }

    private Expression getExpressionToFind(MethodInvocation cond, Expression forVar) {
        Expression expr = removeParentheses(cond.getExpression());
        Expression arg0 = removeParentheses(arg0(cond));
        if (isSameVariable(forVar, expr)) {
            return arg0;
        } else if (isSameVariable(forVar, arg0)) {
            return expr;
        } else if (matches(forVar, expr)) {
            return arg0;
        } else if (matches(forVar, arg0)) {
            return expr;
        } else {
            return null;
        }
    }

    private boolean matches(Expression e1, Expression e2) {
        return match(new ASTMatcher(), e1, e2);
    }

    @Override
    public boolean visit(ForStatement node) {
        final ForLoopContent loopContent = iterateOverContainer(node);
        final List<Statement> stmts = asList(node.getBody());
        if (loopContent != null
                && COLLECTION.equals(loopContent.getContainerType())) {
            if (INDEX.equals(loopContent.getIterationType())) {
                Expression loopElement = null;
                IfStatement is;
                if (stmts.size() == 2) {
                    Pair<Name, Expression> loopVarPair = uniqueVariableDeclarationFragmentName(stmts.get(0));
                    loopElement = loopVarPair.getFirst();
                    MethodInvocation mi = as(loopVarPair.getSecond(), MethodInvocation.class);
                    if (!matches(mi, collectionGet(loopContent))
                            || !isSameVariable(mi.getExpression(), loopContent.getContainerVariable())) {
                        return VISIT_SUBTREE;
                    }

                    is = as(stmts.get(1), IfStatement.class);
                } else if (stmts.size() == 1) {
                    is = as(stmts.get(0), IfStatement.class);
                    loopElement = collectionGet(loopContent);
                } else {
                    return VISIT_SUBTREE;
                }

                return maybeReplaceWithCollectionContains(node, loopContent.getContainerVariable(), loopElement, is);
            } else if (ITERATOR.equals(loopContent.getIterationType())) {
                Expression loopElement = null;
                IfStatement is;
                if (stmts.size() == 2) {
                    Pair<Name, Expression> loopVarPair = uniqueVariableDeclarationFragmentName(stmts.get(0));
                    loopElement = loopVarPair.getFirst();
                    MethodInvocation mi = as(loopVarPair.getSecond(), MethodInvocation.class);
                    if (!matches(mi, iteratorNext(loopContent))
                            || !isSameVariable(mi.getExpression(), loopContent.getIteratorVariable())) {
                        return VISIT_SUBTREE;
                    }

                    is = as(stmts.get(1), IfStatement.class);
                } else if (stmts.size() == 1) {
                    is = as(stmts.get(0), IfStatement.class);
                    loopElement = iteratorNext(loopContent);
                } else {
                    return VISIT_SUBTREE;
                }

                return maybeReplaceWithCollectionContains(node, loopContent.getContainerVariable(), loopElement, is);
            }
        }
        return VISIT_SUBTREE;
    }

    private MethodInvocation iteratorNext(final ForLoopContent loopContent) {
        ASTBuilder b = ctx.getASTBuilder();
        return b.invoke(
            b.copySubtree(loopContent.getIteratorVariable()),
            "next");
    }

    private MethodInvocation collectionGet(final ForLoopContent loopContent) {
        ASTBuilder b = ctx.getASTBuilder();
        return b.invoke(
            b.copySubtree(loopContent.getContainerVariable()),
            "get",
            b.copySubtree(loopContent.getLoopVariable()));
    }

    private Pair<Name, Expression> uniqueVariableDeclarationFragmentName(Statement stmt) {
        VariableDeclarationStatement vds = as(stmt, VariableDeclarationStatement.class);
        if (vds != null) {
            List<VariableDeclarationFragment> fragments = fragments(vds);
            if (fragments.size() == 1) {
                VariableDeclarationFragment vdf = fragments.get(0);
                return Pair.of((Name) vdf.getName(), vdf.getInitializer());
            }
        }
        return Pair.empty();
    }
}

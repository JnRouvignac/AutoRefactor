/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015-2018 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2019 Fabrice TIERCELIN - Reuse for Collection.containsAll()
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
import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.dom.FinderVisitor;
import org.autorefactor.jdt.internal.corext.dom.ForLoopHelper;
import org.autorefactor.jdt.internal.corext.dom.ForLoopHelper.ContainerType;
import org.autorefactor.jdt.internal.corext.dom.ForLoopHelper.ForLoopContent;
import org.autorefactor.jdt.internal.corext.dom.ForLoopHelper.IterationType;
import org.autorefactor.jdt.internal.corext.refactoring.structure.CompilationUnitRewrite;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BooleanLiteral;
import org.eclipse.jdt.core.dom.BreakStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.FieldAccess;
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

/** See {@link #getDescription()} method. */
public abstract class AbstractCollectionMethodRatherThanLoopCleanUp extends AbstractCleanUpRule {
    /**
     * Get the expression to find.
     *
     * @param condition The condition
     * @param forVar    The variable
     * @return The expression
     */
    protected abstract Expression getExpressionToFind(MethodInvocation condition, Expression forVar);

    /**
     * Returns the method to replace or null otherwise.
     *
     * @param condition The condition
     * @return true if it is method to replace.
     */
    protected abstract MethodInvocation getMethodToReplace(Expression condition);

    /**
     * Generate the future method.
     *
     * @param iterable   The iterable
     * @param toFind     The expression to find
     * @param isPositive true if the expression is positive
     * @param b          The builder
     * @return the future method.
     */
    protected abstract Expression newMethod(Expression iterable, Expression toFind, boolean isPositive, ASTNodeFactory b);

    @Override
    public boolean visit(final Block node) {
        AssignmentForAndReturnVisitor assignmentForAndReturnVisitor= new AssignmentForAndReturnVisitor(cuRewrite, node);
        node.accept(assignmentForAndReturnVisitor);
        return assignmentForAndReturnVisitor.getResult();
    }

    private final class AssignmentForAndReturnVisitor extends BlockSubVisitor {
        public AssignmentForAndReturnVisitor(final CompilationUnitRewrite cuRewrite, final Block startNode) {
            super(cuRewrite, startNode);
        }

        @Override
        public boolean visit(final EnhancedForStatement node) {
            SingleVariableDeclaration loopVariable= node.getParameter();
            IfStatement is= uniqueStmtAs(node.getBody(), IfStatement.class);
            return maybeReplaceWithCollectionContains(node, node.getExpression(), loopVariable.getName(), is);
        }

        private boolean maybeReplaceWithCollectionContains(final Statement forNode, final Expression iterable,
                final Expression loopElement, final IfStatement is) {
            if (getResult() && is != null && is.getElseStatement() == null && ASTNodes.instanceOf(iterable, Collection.class.getCanonicalName())) {
                MethodInvocation condition= getMethodToReplace(is.getExpression());
                List<Statement> thenStatements= ASTNodes.asList(is.getThenStatement());

                if (!thenStatements.isEmpty() && condition != null) {
                    Expression toFind= getExpressionToFind(condition, loopElement);

                    if (toFind != null) {
                        if (thenStatements.size() == 1) {
                            Statement thenStatement= thenStatements.get(0);
                            BooleanLiteral innerBl= getReturnedBooleanLiteral(thenStatement);

                            Statement forNextStatement= ASTNodes.getNextStatement(forNode);
                            BooleanLiteral outerBl= getReturnedBooleanLiteral(forNextStatement);

                            Boolean isPositive= signCollectionContains(innerBl, outerBl);

                            if (isPositive != null) {
                                replaceLoopAndReturn(forNode, iterable, toFind, forNextStatement, isPositive);
                                setResult(false);
                                return false;
                            }

                            return maybeReplaceLoopAndVariable(forNode, iterable, thenStatement, toFind);
                        }

                        BreakStatement bs= ASTNodes.as(thenStatements.get(thenStatements.size() - 1), BreakStatement.class);

                        if (bs != null && bs.getLabel() == null) {
                            if (thenStatements.size() == 2 && !maybeReplaceLoopAndVariable(forNode, iterable,
                                    thenStatements.get(0), toFind)) {
                                return false;
                            }

                            if (loopElementIsUsed(loopElement, thenStatements)) {
                                // Cannot remove the loop and its loop element
                                return true;
                            }

                            replaceLoopByIf(forNode, iterable, thenStatements, toFind, bs);
                            setResult(false);
                            return false;
                        }
                    }
                }
            }

            return true;
        }

        private boolean loopElementIsUsed(final Expression loopElement, final List<Statement> thenStatements) {
            if (loopElement instanceof SimpleName) {
                VarUseFinderVisitor visitor= new VarUseFinderVisitor((SimpleName) loopElement);

                for (Statement aThenStatement : thenStatements) {
                    if (visitor.findOrDefault(aThenStatement, false)) {
                        return true;
                    }
                }
            }

            return false;
        }

        private class VarUseFinderVisitor extends FinderVisitor<Boolean> {
            private final SimpleName varName;

            public VarUseFinderVisitor(final SimpleName varName) {
                this.varName= varName;
            }

            @Override
            public boolean visit(final SimpleName variable) {
                if (ASTNodes.isSameLocalVariable(varName, variable)) {
                    setResult(true);
                    return false;
                }

                return true;
            }
        }

        private void replaceLoopByIf(final Statement forNode, final Expression iterable, final List<Statement> thenStatements,
                final Expression toFind, final BreakStatement bs) {
            thenStatements.remove(thenStatements.size() - 1);

            ASTNodeFactory b= cuRewrite.getASTBuilder();
            Statement replacement= b.if0(newMethod(iterable, toFind, true, b), b.block(b.copyRange(thenStatements)));
            cuRewrite.getRefactorings().replace(forNode, replacement);

            thenStatements.add(bs);
        }

        private void replaceLoopAndReturn(final Statement forNode, final Expression iterable, final Expression toFind,
                final Statement forNextStatement, final boolean negate) {
            ASTNodeFactory b= cuRewrite.getASTBuilder();
            cuRewrite.getRefactorings().replace(forNode, b.return0(newMethod(iterable, toFind, negate, b)));

            if (forNextStatement.equals(ASTNodes.getNextSibling(forNode))) {
                cuRewrite.getRefactorings().remove(forNextStatement);
            }
        }

        private boolean maybeReplaceLoopAndVariable(final Statement forNode, final Expression iterable, final Statement uniqueThenStatement,
                final Expression toFind) {
            Statement previousStatement= ASTNodes.getPreviousStatement(forNode);

            if (previousStatement != null) {
                boolean previousStmtIsPreviousSibling= previousStatement.equals(ASTNodes.getPreviousSibling(forNode));
                Assignment as= ASTNodes.asExpression(uniqueThenStatement, Assignment.class);
                Pair<Expression, Expression> innerInit= ASTNodes.decomposeInitializer(as);
                Expression initName= innerInit.getFirst();
                Expression init2= innerInit.getSecond();
                Pair<Expression, Expression> outerInit= getInitializer(previousStatement);

                if (ASTNodes.isSameVariable(outerInit.getFirst(), initName)) {
                    Boolean isPositive= signCollectionContains((BooleanLiteral) init2,
                            (BooleanLiteral) outerInit.getSecond());

                    if (isPositive != null) {
                        replaceLoopAndVariable(forNode, iterable, toFind, previousStatement, previousStmtIsPreviousSibling,
                                initName, isPositive);
                        setResult(false);
                        return false;
                    }
                }
            }

            return true;
        }

        private void replaceLoopAndVariable(final Statement forNode, final Expression iterable, final Expression toFind,
                final Statement previousStatement, final boolean previousStmtIsPreviousSibling, final Expression initName, final boolean isPositive) {
            ASTNodeFactory b= cuRewrite.getASTBuilder();

            Statement replacement;
            if (previousStmtIsPreviousSibling && previousStatement instanceof VariableDeclarationStatement) {
                replacement= b.declareStatement(b.type(boolean.class.getSimpleName()), b.createMoveTarget((SimpleName) initName),
                        newMethod(iterable, toFind, isPositive, b));
            } else if (!previousStmtIsPreviousSibling || previousStatement instanceof ExpressionStatement) {
                replacement= b.toStatement(b.assign(b.createMoveTarget(initName), Assignment.Operator.ASSIGN, newMethod(iterable, toFind, isPositive, b)));
            } else {
                throw new NotImplementedException(forNode);
            }

            cuRewrite.getRefactorings().replace(forNode, replacement);

            if (previousStmtIsPreviousSibling) {
                cuRewrite.getRefactorings().removeButKeepComment(previousStatement);
            }
        }

        private Boolean signCollectionContains(final BooleanLiteral innerBl, final BooleanLiteral outerBl) {
            if (innerBl != null && outerBl != null && innerBl.booleanValue() != outerBl.booleanValue()) {
                return innerBl.booleanValue();
            }

            return null;
        }

        private Pair<Expression, Expression> getInitializer(final Statement statement) {
            if (statement instanceof VariableDeclarationStatement) {
                return uniqueVariableDeclarationFragmentName(statement);
            }

            if (statement instanceof ExpressionStatement) {
                Assignment as= ASTNodes.asExpression(statement, Assignment.class);

                if (ASTNodes.hasOperator(as, Assignment.Operator.ASSIGN) && (as.getLeftHandSide() instanceof Name || as.getLeftHandSide() instanceof FieldAccess)) {
                    return Pair.of(as.getLeftHandSide(), as.getRightHandSide());
                }
            }

            return Pair.empty();
        }

        private IfStatement uniqueStmtAs(final Statement statement, final Class<IfStatement> stmtClazz) {
            return ASTNodes.as(uniqueStatement(ASTNodes.asList(statement)), stmtClazz);
        }

        private Statement uniqueStatement(final List<Statement> statements) {
            return statements.size() == 1 ? statements.get(0) : null;
        }

        private BooleanLiteral getReturnedBooleanLiteral(final Statement statement) {
            ReturnStatement rs= ASTNodes.as(statement, ReturnStatement.class);

            if (rs != null) {
                return ASTNodes.as(rs.getExpression(), BooleanLiteral.class);
            }

            return null;
        }

        @Override
        public boolean visit(final ForStatement node) {
            ForLoopContent loopContent= ForLoopHelper.iterateOverContainer(node);
            List<Statement> statements= ASTNodes.asList(node.getBody());

            if (getResult() && loopContent != null && ContainerType.COLLECTION.equals(loopContent.getContainerType())) {
                if (IterationType.INDEX.equals(loopContent.getIterationType())) {
                    Expression loopElement= null;
                    IfStatement is;
                    if (statements.size() == 2) {
                        Pair<Expression, Expression> loopVarPair= uniqueVariableDeclarationFragmentName(statements.get(0));
                        loopElement= loopVarPair.getFirst();
                        MethodInvocation mi= ASTNodes.as(loopVarPair.getSecond(), MethodInvocation.class);

                        if (!ASTNodes.match(mi, collectionGet(loopContent))
                                || !ASTNodes.isSameVariable(mi.getExpression(), loopContent.getContainerVariable())) {
                            return true;
                        }

                        is= ASTNodes.as(statements.get(1), IfStatement.class);
                    } else if (statements.size() == 1) {
                        is= ASTNodes.as(statements.get(0), IfStatement.class);
                        loopElement= collectionGet(loopContent);
                    } else {
                        return true;
                    }

                    return maybeReplaceWithCollectionContains(node, loopContent.getContainerVariable(), loopElement,
                            is);
                }

                if (IterationType.ITERATOR.equals(loopContent.getIterationType())) {
                    Expression loopElement= null;
                    IfStatement is;
                    if (statements.size() == 2) {
                        Pair<Expression, Expression> loopVarPair= uniqueVariableDeclarationFragmentName(statements.get(0));
                        loopElement= loopVarPair.getFirst();
                        MethodInvocation mi= ASTNodes.as(loopVarPair.getSecond(), MethodInvocation.class);

                        if (!ASTNodes.match(mi, iteratorNext(loopContent))
                                || !ASTNodes.isSameVariable(mi.getExpression(), loopContent.getIteratorVariable())) {
                            return true;
                        }

                        is= ASTNodes.as(statements.get(1), IfStatement.class);
                    } else if (statements.size() == 1) {
                        is= ASTNodes.as(statements.get(0), IfStatement.class);
                        loopElement= iteratorNext(loopContent);
                    } else {
                        return true;
                    }

                    return maybeReplaceWithCollectionContains(node, loopContent.getContainerVariable(), loopElement,
                            is);
                }
            }

            return true;
        }

        private MethodInvocation iteratorNext(final ForLoopContent loopContent) {
            ASTNodeFactory b= cuRewrite.getASTBuilder();
            return b.invoke(b.copySubtree(loopContent.getIteratorVariable()), "next"); //$NON-NLS-1$
        }

        private MethodInvocation collectionGet(final ForLoopContent loopContent) {
            ASTNodeFactory b= cuRewrite.getASTBuilder();
            return b.invoke(b.copySubtree(loopContent.getContainerVariable()), "get", //$NON-NLS-1$
                    b.copySubtree(loopContent.getLoopVariable()));
        }

        private Pair<Expression, Expression> uniqueVariableDeclarationFragmentName(final Statement statement) {
            VariableDeclarationFragment vdf= ASTNodes.getUniqueFragment(ASTNodes.as(statement, VariableDeclarationStatement.class));

            if (vdf != null) {
                return Pair.of(vdf.getName(), vdf.getInitializer());
            }

            return Pair.empty();
        }
    }
}

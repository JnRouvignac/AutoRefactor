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
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.dom.FinderVisitor;
import org.autorefactor.jdt.internal.corext.dom.ForLoops;
import org.autorefactor.jdt.internal.corext.dom.ForLoops.ContainerType;
import org.autorefactor.jdt.internal.corext.dom.ForLoops.ForLoopContent;
import org.autorefactor.jdt.internal.corext.dom.ForLoops.IterationType;
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
import org.eclipse.jdt.core.dom.SuperFieldAccess;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public abstract class AbstractCollectionMethodRatherThanLoopCleanUp extends NewClassImportCleanUp {
    private final class RefactoringWithObjectsClass extends CleanUpWithNewClassImport {
        @Override
        public boolean visit(final Block node) {
            return maybeRefactorBlock(node, getClassesToUseWithImport(), getImportsToAdd());
        }
    }

    /**
     * Get the expression to find.
     *
     * @param condition The condition
     * @param forVar    The variable
     * @param iterable  The iterable
     * @return The expression
     */
    protected abstract Expression getExpressionToFind(MethodInvocation condition, Expression forVar, Expression iterable);

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
     * @param classesToUseWithImport The classes to use with import
     * @param importsToAdd The imports to add
     * @return the future method.
     */
    protected abstract Expression newMethod(Expression iterable, Expression toFind, boolean isPositive, Set<String> classesToUseWithImport, Set<String> importsToAdd);

    @Override
    public RefactoringWithObjectsClass getRefactoringClassInstance() {
        return new RefactoringWithObjectsClass();
    }

    @Override
    public Set<String> getClassesToImport() {
        return Collections.emptySet();
    }

    @Override
    public boolean visit(final Block node) {
        return maybeRefactorBlock(node, getAlreadyImportedClasses(node), new HashSet<String>());
    }

    private boolean maybeRefactorBlock(final Block node,
            final Set<String> classesToUseWithImport, final Set<String> importsToAdd) {
        AssignmentForAndReturnVisitor assignmentForAndReturnVisitor= new AssignmentForAndReturnVisitor(classesToUseWithImport, importsToAdd);
        assignmentForAndReturnVisitor.visitNode(node);
        return assignmentForAndReturnVisitor.result;
    }

    private final class AssignmentForAndReturnVisitor extends BlockSubVisitor {
        private final Set<String> classesToUseWithImport;
        private final Set<String> importsToAdd;

        public AssignmentForAndReturnVisitor(final Set<String> classesToUseWithImport, final Set<String> importsToAdd) {
            this.classesToUseWithImport= classesToUseWithImport;
            this.importsToAdd= importsToAdd;
        }

        @Override
        public boolean visit(final EnhancedForStatement node) {
            SingleVariableDeclaration loopVariable= node.getParameter();
            IfStatement ifStatement= uniqueStmtAs(node.getBody(), IfStatement.class);
            return maybeReplaceWithCollectionContains(node, node.getExpression(), loopVariable.getName(), ifStatement);
        }

        private boolean maybeReplaceWithCollectionContains(final Statement forNode, final Expression iterable,
                final Expression loopElement, final IfStatement ifStatement) {
            if (result
            		&& ifStatement != null
            		&& ifStatement.getElseStatement() == null
            		&& ASTNodes.instanceOf(iterable, Collection.class.getCanonicalName())) {
                MethodInvocation condition= getMethodToReplace(ifStatement.getExpression());
                List<Statement> thenStatements= ASTNodes.asList(ifStatement.getThenStatement());

                if (!thenStatements.isEmpty()
                        && condition != null) {
                    Expression toFind= getExpressionToFind(condition, loopElement, iterable);

                    if (toFind != null) {
                        if (thenStatements.size() == 1) {
                            Statement thenStatement= thenStatements.get(0);
                            BooleanLiteral innerBooleanLiteral= getReturnedBooleanLiteral(thenStatement);

                            Statement forNextStatement= ASTNodes.getNextStatement(forNode);
                            BooleanLiteral outerBooleanLiteral= getReturnedBooleanLiteral(forNextStatement);

                            Boolean isPositive= signCollectionContains(innerBooleanLiteral, outerBooleanLiteral);

                            if (isPositive != null) {
                                replaceLoopAndReturn(forNode, iterable, toFind, forNextStatement, isPositive);
                                result= false;
                                return false;
                            }

                            return maybeReplaceLoopAndVariable(forNode, iterable, thenStatement, toFind);
                        }

                        BreakStatement breakStatement= ASTNodes.as(thenStatements.get(thenStatements.size() - 1), BreakStatement.class);

                        if (breakStatement != null && breakStatement.getLabel() == null) {
                            if (thenStatements.size() == 2 && !maybeReplaceLoopAndVariable(forNode, iterable,
                                    thenStatements.get(0), toFind)) {
                                return false;
                            }

                            if (loopElementIsUsed(loopElement, thenStatements)) {
                                // Cannot remove the loop and its loop element
                                return true;
                            }

                            replaceLoopByIf(forNode, iterable, thenStatements, toFind, breakStatement);
                            result= false;
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
                final Expression toFind, final BreakStatement breakStatement) {
            ASTRewrite rewrite= cuRewrite.getASTRewrite();
            ASTNodeFactory ast= cuRewrite.getASTBuilder();
            TextEditGroup group= new TextEditGroup(""); //$NON-NLS-1$

            thenStatements.remove(thenStatements.size() - 1);

			IfStatement replacement= ast.newIfStatement();
			replacement.setExpression(newMethod(iterable, toFind, true, classesToUseWithImport, importsToAdd));
			Block newBlock= ast.newBlock();
			newBlock.statements().add(ast.copyRange(thenStatements));
			replacement.setThenStatement(newBlock);

            ASTNodes.replaceButKeepComment(rewrite, forNode, replacement, group);

            thenStatements.add(breakStatement);
        }

        private void replaceLoopAndReturn(final Statement forNode, final Expression iterable, final Expression toFind,
                final Statement forNextStatement, final boolean negate) {
            ASTRewrite rewrite= cuRewrite.getASTRewrite();
            ASTNodeFactory ast= cuRewrite.getASTBuilder();

            TextEditGroup group= new TextEditGroup(""); //$NON-NLS-1$

            ASTNodes.replaceButKeepComment(rewrite, forNode, ast.newReturnStatement(newMethod(iterable, toFind, negate, classesToUseWithImport, importsToAdd)), group);

            if (forNextStatement.equals(ASTNodes.getNextSibling(forNode))) {
                rewrite.remove(forNextStatement, group);
            }
        }

        private boolean maybeReplaceLoopAndVariable(final Statement forNode, final Expression iterable, final Statement uniqueThenStatement,
                final Expression toFind) {
            Statement previousStatement= ASTNodes.getPreviousStatement(forNode);

            if (previousStatement != null) {
                boolean previousStmtIsPreviousSibling= previousStatement.equals(ASTNodes.getPreviousSibling(forNode));
                Assignment assignment= ASTNodes.asExpression(uniqueThenStatement, Assignment.class);
                Pair<Expression, Expression> innerInit= ASTNodes.decomposeInitializer(assignment);
                Expression initName= innerInit.getFirst();
                Expression init2= innerInit.getSecond();
                Pair<Expression, Expression> outerInit= getInitializer(previousStatement);

                if (ASTNodes.isSameVariable(outerInit.getFirst(), initName)) {
                    Boolean isPositive= signCollectionContains((BooleanLiteral) init2,
                            (BooleanLiteral) outerInit.getSecond());

                    if (isPositive != null) {
                        replaceLoopAndVariable(forNode, iterable, toFind, previousStatement, previousStmtIsPreviousSibling,
                                initName, isPositive);
                        result= false;
                        return false;
                    }
                }
            }

            return true;
        }

        private void replaceLoopAndVariable(final Statement forNode, final Expression iterable, final Expression toFind,
                final Statement previousStatement, final boolean previousStmtIsPreviousSibling, final Expression initName, final boolean isPositive) {
            ASTRewrite rewrite= cuRewrite.getASTRewrite();
            ASTNodeFactory ast= cuRewrite.getASTBuilder();
            TextEditGroup group= new TextEditGroup(""); //$NON-NLS-1$

            Statement replacement;
            if (previousStmtIsPreviousSibling && previousStatement instanceof VariableDeclarationStatement) {
                replacement= ast.declareStatement(ast.type(boolean.class.getSimpleName()), ASTNodes.createMoveTarget(rewrite, (SimpleName) initName),
                        newMethod(iterable, toFind, isPositive, classesToUseWithImport, importsToAdd));
            } else if (!previousStmtIsPreviousSibling || previousStatement instanceof ExpressionStatement) {
                replacement= ast.newExpressionStatement(ast.newAssignment(ASTNodes.createMoveTarget(rewrite, initName), Assignment.Operator.ASSIGN, newMethod(iterable, toFind, isPositive, classesToUseWithImport, importsToAdd)));
            } else {
                throw new NotImplementedException(forNode);
            }

            ASTNodes.replaceButKeepComment(rewrite, forNode, replacement, group);

            if (previousStmtIsPreviousSibling) {
                ASTNodes.removeButKeepComment(rewrite, previousStatement, group);
            }
        }

        private Boolean signCollectionContains(final BooleanLiteral innerBl, final BooleanLiteral outerBl) {
            if (innerBl != null
            		&& outerBl != null
            		&& innerBl.booleanValue() != outerBl.booleanValue()) {
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

                if (ASTNodes.hasOperator(as, Assignment.Operator.ASSIGN)
                		&& (as.getLeftHandSide() instanceof Name || as.getLeftHandSide() instanceof FieldAccess || as.getLeftHandSide() instanceof SuperFieldAccess)) {
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
            ForLoopContent loopContent= ForLoops.iterateOverContainer(node);

            if (result
            		&& loopContent != null
            		&& ContainerType.COLLECTION.equals(loopContent.getContainerType())) {
                if (IterationType.INDEX.equals(loopContent.getIterationType())) {
                    return maybeReplace(node, loopContent, collectionGet(loopContent), loopContent.getContainerVariable());
                }

                if (IterationType.ITERATOR.equals(loopContent.getIterationType())) {
                    return maybeReplace(node, loopContent, iteratorNext(loopContent), loopContent.getIteratorVariable());
                }
            }

            return true;
        }

        private boolean maybeReplace(final ForStatement node, final ForLoopContent loopContent,
                final MethodInvocation item, final Expression variable) {
            List<Statement> statements= ASTNodes.asList(node.getBody());

            Expression loopElement;
            IfStatement is;
            if (statements.size() == 2) {
                Pair<Expression, Expression> loopVarPair= uniqueVariableDeclarationFragmentName(statements.get(0));
                MethodInvocation methodInvocation= ASTNodes.as(loopVarPair.getSecond(), MethodInvocation.class);

                if (!ASTNodes.match(methodInvocation, item)
                        || !ASTNodes.isSameVariable(methodInvocation.getExpression(), variable)) {
                    return true;
                }

                loopElement= loopVarPair.getFirst();
                is= ASTNodes.as(statements.get(1), IfStatement.class);
            } else if (statements.size() == 1) {
                loopElement= item;
                is= ASTNodes.as(statements.get(0), IfStatement.class);
            } else {
                return true;
            }

            return maybeReplaceWithCollectionContains(node, loopContent.getContainerVariable(), loopElement,
                    is);
        }

        private MethodInvocation iteratorNext(final ForLoopContent loopContent) {
            ASTNodeFactory ast= cuRewrite.getASTBuilder();

            MethodInvocation nextMethod= ast.newMethodInvocation();
			nextMethod.setExpression(ast.copySubtree(loopContent.getIteratorVariable()));
			nextMethod.setName(ast.newSimpleName("next")); //$NON-NLS-1$
			return nextMethod;
        }

        private MethodInvocation collectionGet(final ForLoopContent loopContent) {
            ASTNodeFactory ast= cuRewrite.getASTBuilder();

            MethodInvocation getMethod= ast.newMethodInvocation();
			getMethod.setExpression(ast.copySubtree(loopContent.getContainerVariable()));
			getMethod.setName(ast.newSimpleName("get")); //$NON-NLS-1$
			getMethod.arguments().add(ast.copySubtree(ASTNodes.getUnparenthesedExpression(loopContent.getLoopVariable())));
			return getMethod;
        }

        private Pair<Expression, Expression> uniqueVariableDeclarationFragmentName(final Statement statement) {
            VariableDeclarationFragment fragment= ASTNodes.getUniqueFragment(statement);

            if (fragment != null) {
                return Pair.of(fragment.getName(), fragment.getInitializer());
            }

            return Pair.empty();
        }
    }
}

/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2017 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.jdt.internal.corext.dom;

import static org.eclipse.jdt.core.dom.ASTNode.BREAK_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.CONTINUE_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.FIELD_ACCESS;
import static org.eclipse.jdt.core.dom.ASTNode.IF_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.QUALIFIED_NAME;
import static org.eclipse.jdt.core.dom.ASTNode.RETURN_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.SIMPLE_NAME;
import static org.eclipse.jdt.core.dom.ASTNode.SINGLE_VARIABLE_DECLARATION;
import static org.eclipse.jdt.core.dom.ASTNode.THIS_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.THROW_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.VARIABLE_DECLARATION_FRAGMENT;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.autorefactor.util.IllegalStateException;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.Pair;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.AbstractTypeDeclaration;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.ArrayAccess;
import org.eclipse.jdt.core.dom.ArrayInitializer;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BodyDeclaration;
import org.eclipse.jdt.core.dom.BooleanLiteral;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.CatchClause;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Comment;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.ConstructorInvocation;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IExtendedModifier;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.Javadoc;
import org.eclipse.jdt.core.dom.LabeledStatement;
import org.eclipse.jdt.core.dom.MemberValuePair;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.NormalAnnotation;
import org.eclipse.jdt.core.dom.NullLiteral;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.ParameterizedType;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SuperConstructorInvocation;
import org.eclipse.jdt.core.dom.SuperMethodInvocation;
import org.eclipse.jdt.core.dom.SwitchStatement;
import org.eclipse.jdt.core.dom.TagElement;
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.jdt.core.dom.ThrowStatement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.UnionType;
import org.eclipse.jdt.core.dom.VariableDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.jdt.core.dom.WhileStatement;

/**
 * Helper class for manipulating, converting, navigating and checking
 * {@link ASTNode}s.
 */
public final class ASTNodes {
    /** Enum representing the possible side effect of an expression. */
    public enum ExprActivity {
        /** Does nothing. */
        PASSIVE,
        /** May modify something. */
        CAN_BE_ACTIVE,
        /** Modify something. */
        ACTIVE;
    }

    /** Compares {@link ASTNode}s according to their start position. */
    public static final class NodeStartPositionComparator implements Comparator<ASTNode> {
        /**
         * Compare objects.
         *
         * @param o1 First item
         * @param o2 Second item
         *
         * @return -1, 0 or 1
         */
        public int compare(ASTNode o1, ASTNode o2) {
            return o1.getStartPosition() - o2.getStartPosition();
        }
    }

    private static final class VariableDeclarationIdentifierVisitor extends ASTVisitor {
        private final Set<String> variableNames= new HashSet<>();
        private final ASTNode startNode;
        private final boolean includeInnerScopes;

        private VariableDeclarationIdentifierVisitor(ASTNode startNode, boolean includeInnerScopes) {
            this.startNode= startNode;
            this.includeInnerScopes= includeInnerScopes;
        }

        private Set<String> getVariableNames() {
            return variableNames;
        }

        @Override
        public boolean visit(SingleVariableDeclaration node) {
            variableNames.add(node.getName().getIdentifier());
            return true;
        }

        @Override
        public boolean visit(VariableDeclarationFragment node) {
            variableNames.add(node.getName().getIdentifier());
            return true;
        }

        @Override
        public boolean visit(Block node) {
            return startNode == node || includeInnerScopes;
        }
    }

    private static final class ExprActivityVisitor extends InterruptibleVisitor {
        private ExprActivity activityLevel= ExprActivity.PASSIVE;

        public ExprActivity getActivityLevel() {
            return activityLevel;
        }

        @Override
        public boolean visit(CastExpression node) {
            activityLevel= ExprActivity.CAN_BE_ACTIVE;
            return true;
        }

        @Override
        public boolean visit(ArrayAccess node) {
            activityLevel= ExprActivity.CAN_BE_ACTIVE;
            return true;
        }

        @Override
        public boolean visit(Assignment node) {
            activityLevel= ExprActivity.ACTIVE;
            return interruptVisit();
        }

        @Override
        public boolean visit(PrefixExpression node) {
            if (hasOperator(node, PrefixExpression.Operator.INCREMENT, PrefixExpression.Operator.DECREMENT)) {
                activityLevel= ExprActivity.ACTIVE;
                return interruptVisit();
            }
            return true;
        }

        @Override
        public boolean visit(PostfixExpression node) {
            activityLevel= ExprActivity.ACTIVE;
            return interruptVisit();
        }

        @SuppressWarnings("unchecked")
        @Override
        public boolean visit(InfixExpression node) {
            if (hasOperator(node, InfixExpression.Operator.DIVIDE)
                    || (hasOperator(node, InfixExpression.Operator.PLUS) && hasType(node, String.class.getCanonicalName())
                            && (mayCallImplicitToString(node.getLeftOperand())
                                    || mayCallImplicitToString(node.getRightOperand())
                                    || mayCallImplicitToString(node.extendedOperands())))) {
                activityLevel= ExprActivity.CAN_BE_ACTIVE;
            }
            return true;
        }

        private boolean mayCallImplicitToString(List<Expression> extendedOperands) {
            if (extendedOperands != null) {
                for (Expression expression : extendedOperands) {
                    if (mayCallImplicitToString(expression)) {
                        return true;
                    }
                }
            }
            return false;
        }

        private boolean mayCallImplicitToString(Expression expression) {
            return !hasType(expression, String.class.getCanonicalName(), boolean.class.getSimpleName(), short.class.getSimpleName(), int.class.getSimpleName(), long.class.getSimpleName(), float.class.getSimpleName(), double.class.getSimpleName(),
                    Short.class.getCanonicalName(), Boolean.class.getCanonicalName(), Integer.class.getCanonicalName(), Long.class.getCanonicalName(), Float.class.getCanonicalName(),
                    Double.class.getCanonicalName()) && !(expression instanceof PrefixExpression) && !(expression instanceof InfixExpression)
                    && !(expression instanceof PostfixExpression);
        }

        @Override
        public boolean visit(SuperMethodInvocation node) {
            activityLevel= ExprActivity.CAN_BE_ACTIVE;
            return true;
        }

        @Override
        public boolean visit(MethodInvocation node) {
            activityLevel= ExprActivity.CAN_BE_ACTIVE;
            return true;
        }

        @Override
        public boolean visit(ClassInstanceCreation node) {
            activityLevel= ExprActivity.CAN_BE_ACTIVE;
            return true;
        }

        @Override
        public boolean visit(ThrowStatement node) {
            activityLevel= ExprActivity.CAN_BE_ACTIVE;
            return true;
        }
    }

    private ASTNodes() {
    }

    // AST nodes manipulation

    /**
     * Returns the same node after removing any parentheses around it.
     *
     * @param node the node around which parentheses must be removed
     * @return the same node after removing any parentheses around it. If there are
     *         no parentheses around it then the exact same node is returned
     */
    public static ASTNode getUnparenthesedExpression(ASTNode node) {
        if (node instanceof Expression) {
            return getUnparenthesedExpression((Expression) node);
        }
        return node;
    }

    /**
     * Returns the same expression after removing any parentheses around it.
     *
     * @param expression the expression around which parentheses must be removed
     * @return the same expression after removing any parentheses around it If there
     *         are no parentheses around it then the exact same expression is
     *         returned
     */
    public static Expression getUnparenthesedExpression(Expression expression) {
        if (expression.getNodeType() == ASTNode.PARENTHESIZED_EXPRESSION) {
            return getUnparenthesedExpression(((ParenthesizedExpression) expression).getExpression());
        }
        return expression;
    }

    // AST nodes conversions

    /**
     * Returns the provided statement as a non null list of statements:
     * <ul>
     * <li>if the statement is null, then an empty list is returned</li>
     * <li>if the statement is a {@link Block}, then its children are returned</li>
     * <li>otherwise, the current node is returned wrapped in a list</li>
     * </ul>
     *
     * @param statement the statement to analyze
     * @return the provided statement as a non null list of statements
     */
    public static List<Statement> asList(Statement statement) {
        if (statement == null) {
            return Collections.emptyList();
        }

        if (statement instanceof Block) {
            return statements((Block) statement);
        }

        return Arrays.asList(statement);
    }

    /**
     * Casts the provided statement to an object of the provided type if type
     * matches.
     *
     * @param <T>       the required statement type
     * @param statement the statement to cast
     * @param stmtClass the class representing the required statement type
     * @return the provided statement as an object of the provided type if type
     *         matches, null otherwise
     */
    @SuppressWarnings("unchecked")
    public static <T extends Statement> T as(Statement statement, Class<T> stmtClass) {
        if (statement == null) {
            return null;
        }

        final List<Statement> statements= asList(statement);
        if (statements.size() == 1) {
            final Statement oneStatement= statements.get(0);

            if (stmtClass.isAssignableFrom(oneStatement.getClass())) {
                return (T) oneStatement;
            }
        }

        return null;
    }

    /**
     * Returns whether the provided statement has the provided type.
     *
     * @param statement the statement to test
     * @param stmtClass the type to test the statement against
     * @return {@code true} if the provided statement has the provided type,
     *         {@code false} otherwise
     */
    public static boolean is(Statement statement, Class<? extends Statement> stmtClass) {
        return as(statement, stmtClass) != null;
    }

    /**
     * Casts the provided expression to an object of the provided type if type
     * matches.
     *
     * @param <T>        the required expression type
     * @param expression the expression to cast
     * @param exprClass  the class representing the required expression type
     * @return the provided expression as an object of the provided type if type
     *         matches, null otherwise
     */
    @SuppressWarnings("unchecked")
    public static <T extends Expression> T as(Expression expression, Class<T> exprClass) {
        if (expression != null) {
            if (exprClass.isAssignableFrom(expression.getClass())) {
                return (T) expression;
            }
            if (expression instanceof ParenthesizedExpression) {
                return as(((ParenthesizedExpression) expression).getExpression(), exprClass);
            }
        }
        return null;
    }

    /**
     * Returns whether the provided expression has the provided type.
     *
     * @param expression the expression to test
     * @param exprClass  the type to test the expression against
     * @return {@code true} if the provided expression has the provided type,
     *         {@code false} otherwise
     */
    public static boolean is(Expression expression, Class<? extends Expression> exprClass) {
        return as(expression, exprClass) != null;
    }

    /**
     * If the provided expression collection only has one element, then that unique
     * expression is cast to an object of the provided type if type matches.
     *
     * @param <T>       the required expression type
     * @param exprs     the singleton expression to cast
     * @param exprClass the class representing the required expression type
     * @return the provided singleton expression as an object of the provided type
     *         if type matches, null otherwise
     */
    public static <T extends Expression> T as(Collection<? extends Expression> exprs, Class<T> exprClass) {
        if (exprs != null && exprs.size() == 1) {
            return as(exprs.iterator().next(), exprClass);
        }
        return null;
    }

    /**
     * Returns the {@link Expression} of a specified type out of the provided
     * {@link Statement}. Note the provided statement is first converted to an
     * {@link ExpressionStatement} if possible.
     *
     * @param <T>       the required expression type
     * @param statement the statement
     * @param exprClass the class representing the required expression type
     * @return the {@link Expression} of a specified type out of an
     *         {@link ExpressionStatement}
     */
    public static <T extends Expression> T asExpression(Statement statement, Class<T> exprClass) {
        final ExpressionStatement es= as(statement, ExpressionStatement.class);
        if (es != null) {
            return as(es.getExpression(), exprClass);
        }
        return null;
    }

    /**
     * Returns whether the two provided expressions are cast compatible.
     *
     * @param expr1 the first expression
     * @param expr2 the second expression
     * @return {@code true} if the two provided expressions are cast compatible,
     *         {@code false} otherwise
     * @see ITypeBinding#isCastCompatible(ITypeBinding)
     */
    public static boolean isCastCompatible(Expression expr1, Expression expr2) {
        final ITypeBinding tb1= expr1.resolveTypeBinding();
        final ITypeBinding tb2= expr2.resolveTypeBinding();
        return tb1 != null && tb2 != null && tb1.isCastCompatible(tb2);
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see ClassInstanceCreation#arguments()
     */
    @SuppressWarnings("unchecked")
    public static List<Expression> arguments(ClassInstanceCreation node) {
        return node.arguments();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see ConstructorInvocation#arguments()
     */
    @SuppressWarnings("unchecked")
    public static List<Expression> arguments(ConstructorInvocation node) {
        return node.arguments();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see MethodInvocation#arguments()
     */
    @SuppressWarnings("unchecked")
    public static List<Expression> arguments(MethodInvocation node) {
        return node.arguments();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see SuperConstructorInvocation#arguments()
     */
    @SuppressWarnings("unchecked")
    public static List<Expression> arguments(SuperConstructorInvocation node) {
        return node.arguments();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see SuperMethodInvocation#arguments()
     */
    @SuppressWarnings("unchecked")
    public static List<Expression> arguments(SuperMethodInvocation node) {
        return node.arguments();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of body declarations
     * @see AnonymousClassDeclaration#bodyDeclarations()
     */
    @SuppressWarnings("unchecked")
    public static List<BodyDeclaration> bodyDeclarations(AnonymousClassDeclaration node) {
        return node.bodyDeclarations();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of body declarations
     * @see TypeDeclaration#bodyDeclarations()
     */
    @SuppressWarnings("unchecked")
    public static List<BodyDeclaration> bodyDeclarations(AbstractTypeDeclaration node) {
        return node.bodyDeclarations();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see TryStatement#catchClauses()
     */
    @SuppressWarnings("unchecked")
    public static List<CatchClause> catchClauses(TryStatement node) {
        return node.catchClauses();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see ArrayInitializer#expressions()
     */
    @SuppressWarnings("unchecked")
    public static List<Expression> expressions(ArrayInitializer node) {
        return node.expressions();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see InfixExpression#extendedOperands()
     */
    @SuppressWarnings("unchecked")
    public static List<Expression> extendedOperands(InfixExpression node) {
        return node.extendedOperands();
    }

    /**
     * Returns all the operands from the provided infix expressions.
     *
     * @param node the infix expression
     * @return a List of expressions
     */
    public static List<Expression> allOperands(InfixExpression node) {
        final List<Expression> extOps= extendedOperands(node);
        final List<Expression> results= new ArrayList<Expression>(2 + extOps.size());
        results.add(node.getLeftOperand());
        results.add(node.getRightOperand());
        results.addAll(extOps);
        return results;
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see ForStatement#initializers()
     */
    @SuppressWarnings("unchecked")
    public static List<Expression> initializers(ForStatement node) {
        return node.initializers();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see ForStatement#updaters()
     */
    @SuppressWarnings("unchecked")
    public static List<Expression> updaters(ForStatement node) {
        return node.updaters();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see FieldDeclaration#fragments()
     */
    @SuppressWarnings("unchecked")
    public static List<VariableDeclarationFragment> fragments(FieldDeclaration node) {
        return node.fragments();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see VariableDeclarationExpression#fragments()
     */
    @SuppressWarnings("unchecked")
    public static List<VariableDeclarationFragment> fragments(VariableDeclarationExpression node) {
        return node.fragments();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see VariableDeclarationStatement#fragments()
     */
    @SuppressWarnings("unchecked")
    public static List<VariableDeclarationFragment> fragments(VariableDeclarationStatement node) {
        return node.fragments();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see CompilationUnit#getCommentList()
     */
    @SuppressWarnings("unchecked")
    public static List<Comment> getCommentList(CompilationUnit node) {
        return node.getCommentList();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see CompilationUnit#imports()
     */
    @SuppressWarnings("unchecked")
    public static List<ImportDeclaration> imports(CompilationUnit node) {
        return node.imports();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see BodyDeclaration#modifiers()
     */
    @SuppressWarnings("unchecked")
    public static List<IExtendedModifier> modifiers(BodyDeclaration node) {
        return node.modifiers();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see BodyDeclaration#modifiers()
     */
    @SuppressWarnings("unchecked")
    public static List<IExtendedModifier> modifiers(SingleVariableDeclaration node) {
        return node.modifiers();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see VariableDeclarationExpression#modifiers()
     */
    @SuppressWarnings("unchecked")
    public static List<IExtendedModifier> modifiers(VariableDeclarationExpression node) {
        return node.modifiers();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see VariableDeclarationStatement#modifiers()
     */
    @SuppressWarnings("unchecked")
    public static List<IExtendedModifier> modifiers(VariableDeclarationStatement node) {
        return node.modifiers();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see MethodDeclaration#parameters()
     */
    @SuppressWarnings("unchecked")
    public static List<SingleVariableDeclaration> parameters(MethodDeclaration node) {
        return node.parameters();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of variable declaration expressions
     * @see TryStatement#resources()
     */
    @SuppressWarnings("unchecked")
    public static List<VariableDeclarationExpression> resources(TryStatement node) {
        return node.resources();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see Block#statements()
     */
    @SuppressWarnings("unchecked")
    public static List<Statement> statements(Block node) {
        return node.statements();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see SwitchStatement#statements()
     */
    @SuppressWarnings("unchecked")
    public static List<Statement> statements(SwitchStatement node) {
        return node.statements();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see Javadoc#tags()
     */
    @SuppressWarnings("unchecked")
    public static List<TagElement> tags(Javadoc node) {
        return node.tags();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of types
     * @see MethodDeclaration#thrownExceptionTypes()
     */
    @SuppressWarnings("unchecked")
    public static List<Type> thrownExceptionTypes(MethodDeclaration node) {
        return node.thrownExceptionTypes();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see ParameterizedType#typeArguments()
     */
    @SuppressWarnings("unchecked")
    public static List<Type> typeArguments(MethodInvocation node) {
        return node.typeArguments();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see ParameterizedType#typeArguments()
     */
    @SuppressWarnings("unchecked")
    public static List<Type> typeArguments(ParameterizedType node) {
        return node.typeArguments();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of abstract type declarations
     * @see CompilationUnit#types()
     */
    @SuppressWarnings("unchecked")
    public static List<AbstractTypeDeclaration> types(CompilationUnit node) {
        return node.types();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of abstract type declarations
     * @see UnionType#types()
     */
    @SuppressWarnings("unchecked")
    public static List<Type> types(UnionType node) {
        return node.types();
    }

    /**
     * Generecized version of the equivalent JDT method.
     *
     * @param node the node on which to call the equivalent JDT method
     * @return a List of expressions
     * @see NormalAnnotation#values()
     */
    @SuppressWarnings("unchecked")
    public static List<MemberValuePair> values(NormalAnnotation node) {
        return node.values();
    }

    /**
     * Returns the type of the object on which the method is called.
     *
     * @param mi the node on which to call the equivalent JDT method
     * @return the type of the object on which the method is called.
     */
    public static ITypeBinding getCalledType(final MethodInvocation mi) {
        IMethodBinding methodBinding= mi.resolveMethodBinding();
        if (methodBinding != null) {
            return methodBinding.getDeclaringClass();
        }
        return null;
    }

    /**
     * Returns the {@link Boolean} object value represented by the provided
     * expression.
     *
     * @param expression the expression to analyze
     * @return the {@link Boolean} object value if the provided expression
     *         represents one, null otherwise
     */
    public static Boolean getBooleanLiteral(Expression expression) {
        final BooleanLiteral bl= as(expression, BooleanLiteral.class);
        if (bl != null) {
            return bl.booleanValue();
        }
        final QualifiedName qn= as(expression, QualifiedName.class);
        if (hasType(qn, Boolean.class.getCanonicalName())) {
            return getBooleanObject(qn);
        }
        return null;
    }

    /**
     * Returns the {@link Boolean} object constant value represented by the provided
     * qualified name.
     *
     * @param qualifiedName the qualified name that must represent a Boolean object
     *                      constant
     * @return the {@link Boolean} object constant value represented by the provided
     *         qualified name, or null if the qualified name does not represent a
     *         {@link Boolean} object constant value.
     */
    public static Boolean getBooleanObject(final QualifiedName qualifiedName) {
        final String fqn= qualifiedName.getFullyQualifiedName();
        if ("Boolean.TRUE".equals(fqn)) { //$NON-NLS-1$
            return true;
        }
        if ("Boolean.FALSE".equals(fqn)) { //$NON-NLS-1$
            return false;
        }
        return null;
    }

    // AST navigation

    /**
     * Returns the first ancestor of the provided node which has the required type.
     *
     * @param <T>           the required ancestor's type
     * @param node          the start node
     * @param ancestorClass the required ancestor's type
     * @return the first ancestor of the provided node which has the required type
     * @see #getAncestorOrNull(ASTNode, Class)
     * @see #getFirstAncestorOrNull(ASTNode, Class...)
     * @throws IllegalStateException if ancestor not found.
     */
    public static <T extends ASTNode> T getAncestor(ASTNode node, Class<T> ancestorClass) {
        final T ancestor= getAncestorOrNull(node, ancestorClass);
        if (ancestor != null) {
            return ancestor;
        }
        throw new IllegalStateException(node, "Could not find any ancestor for " + ancestorClass + "and node " + node); //$NON-NLS-1$ //$NON-NLS-2$
    }

    /**
     * Returns the first ancestor of the provided node which has the required type.
     *
     * @param <T>           the required ancestor's type
     * @param node          the start node
     * @param ancestorClass the required ancestor's type
     * @return the first ancestor of the provided node which has the required type,
     *         {@code null} if no suitable ancestor can be found
     * @see #getAncestor(ASTNode, Class)
     * @see #getFirstAncestorOrNull(ASTNode, Class...)
     */
    @SuppressWarnings("unchecked")
    public static <T extends ASTNode> T getAncestorOrNull(ASTNode node, Class<T> ancestorClass) {
        if (node == null || node.getParent() == null) {
            return null;
        }
        final ASTNode parent= node.getParent();
        if (ancestorClass.isAssignableFrom(parent.getClass())) {
            return (T) parent;
        }
        return getAncestorOrNull(parent, ancestorClass);
    }

    /**
     * Returns the enclosing type of the provided node.
     * <p>
     * i.e. this returns the most immediate type declaration surrounding the
     * provided node.
     *
     * @param node the start node
     * @return the enclosing type of the provided node, or {@code null}
     */
    public static ASTNode getEnclosingType(ASTNode node) {
        final Class<?>[] ancestorClasses= { AbstractTypeDeclaration.class, AnonymousClassDeclaration.class };
        final ASTNode ancestor= getFirstAncestorOrNull(node, ancestorClasses);
        if (ancestor == null) {
            throw new IllegalStateException(node,
                    "Could not find any ancestor for " + Arrays.toString(ancestorClasses) + " and node type " //$NON-NLS-1$ //$NON-NLS-2$
                            + (node != null ? node.getClass().getSimpleName() : null) + " node.toString() " + node); //$NON-NLS-1$
        }
        return ancestor;
    }

    /**
     * Returns the first ancestor of the provided node which has any of the required
     * types.
     *
     * @param node            the start node
     * @param ancestorClasses the required ancestor's types
     * @return the first ancestor of the provided node which has any of the required
     *         type, or {@code null}
     * @see #getAncestor(ASTNode, Class)
     * @see #getAncestorOrNull(ASTNode, Class)
     */
    public static ASTNode getFirstAncestorOrNull(ASTNode node, Class<?>... ancestorClasses) {
        if (ancestorClasses.length == 1) {
            throw new java.lang.IllegalArgumentException("Please use ASTHelper.getAncestor(ASTNode, Class<?>) instead"); //$NON-NLS-1$
        }
        if (node == null || node.getParent() == null || ancestorClasses.length == 0) {
            return null;
        }
        final ASTNode parent= node.getParent();
        for (Class<?> ancestorClass : ancestorClasses) {
            if (ancestorClass.isAssignableFrom(parent.getClass())) {
                return parent;
            }
        }
        return getFirstAncestorOrNull(parent, ancestorClasses);
    }

    /**
     * Returns the type of either a method return or an assigned variable that is
     * the destination of the given node. Returns null otherwise.
     *
     * @param node the start node
     *
     * @return the type of either a method return or an assigned variable
     */
    public static ITypeBinding getTargetType(final ASTNode node) {
        if (node != null) {
            final ASTNode parent= node.getParent();
            if (parent instanceof ParenthesizedExpression) {
                return getTargetType(parent);
            }
            if (parent instanceof ReturnStatement) {
                final ReturnStatement returnStatement= (ReturnStatement) parent;
                if (returnStatement.getExpression().equals(node)) {
                    final MethodDeclaration method= getAncestorOrNull(returnStatement, MethodDeclaration.class);
                    if (method != null && method.getReturnType2() != null) {
                        return method.getReturnType2().resolveBinding();
                    }
                }
            } else if (parent instanceof CastExpression) {
                return ((CastExpression) parent).resolveTypeBinding();
            } else if (parent instanceof VariableDeclarationFragment) {
                return resolveTypeBinding((VariableDeclarationFragment) parent);
            } else if (parent instanceof Assignment) {
                return ((Assignment) parent).getLeftHandSide().resolveTypeBinding();
            } else if (parent instanceof ArrayAccess) {
                final ArrayAccess arrayAccess= (ArrayAccess) parent;
                if (arrayAccess.getIndex().equals(node)) {
                    return node.getAST().resolveWellKnownType(int.class.getSimpleName());
                }
            } else if (parent instanceof ConditionalExpression) {
                final ConditionalExpression conditionalExpression= (ConditionalExpression) parent;
                if (conditionalExpression.getExpression().equals(node)) {
                    return node.getAST().resolveWellKnownType(boolean.class.getSimpleName());
                }
            } else if (parent instanceof PrefixExpression) {
                final PrefixExpression prefixExpression= (PrefixExpression) parent;
                if (hasOperator(prefixExpression, PrefixExpression.Operator.NOT)) {
                    return node.getAST().resolveWellKnownType(boolean.class.getSimpleName());
                }
            } else if (parent instanceof InfixExpression) {
                final InfixExpression prefixExpression= (InfixExpression) parent;
                if (hasOperator(prefixExpression, InfixExpression.Operator.CONDITIONAL_AND, InfixExpression.Operator.CONDITIONAL_OR)) {
                    return node.getAST().resolveWellKnownType(boolean.class.getSimpleName());
                }
            } else if (parent instanceof IfStatement) {
                final IfStatement ifStatement= (IfStatement) parent;
                if (ifStatement.getExpression().equals(node)) {
                    return node.getAST().resolveWellKnownType(boolean.class.getSimpleName());
                }
            } else if (parent instanceof WhileStatement) {
                final WhileStatement whileStatement= (WhileStatement) parent;
                if (whileStatement.getExpression().equals(node)) {
                    return node.getAST().resolveWellKnownType(boolean.class.getSimpleName());
                }
            } else if (parent instanceof DoStatement) {
                final DoStatement doStatement= (DoStatement) parent;
                if (doStatement.getExpression().equals(node)) {
                    return node.getAST().resolveWellKnownType(boolean.class.getSimpleName());
                }
            } else if (parent instanceof SwitchStatement) {
                final SwitchStatement switchStatement= (SwitchStatement) parent;
                if (switchStatement.getExpression().equals(node)) {
                    final ITypeBinding discriminentType= switchStatement.getExpression().resolveTypeBinding();
                    if (discriminentType.isPrimitive() || discriminentType.isEnum()
                            || hasType(discriminentType, String.class.getCanonicalName())) {
                        return discriminentType;
                    }
                    return node.getAST()
                            .resolveWellKnownType(Bindings.getUnboxedTypeName(discriminentType.getQualifiedName()));
                }
            }
        }

        return null;
    }

    /**
     * Returns true if a sibling may exist.
     *
     * @param node the start node
     * @return true if a sibling may exist
     */
    public static boolean canHaveSiblings(Statement node) {
        final ASTNode parent= node.getParent();

        if (parent instanceof LabeledStatement) {
            return canHaveSiblings((LabeledStatement) parent);
        }

        return parent instanceof Block;
    }

    /**
     * Returns the previous body declaration in the same block if it exists.
     *
     * @param startNode the start node
     * @return the previous body declaration in the same block if it exists, null
     *         otherwise
     */
    public static BodyDeclaration getPreviousSibling(BodyDeclaration startNode) {
        return getSibling(startNode, true);
    }

    /**
     * Returns the previous statement in the same block if it exists.
     *
     * @param startNode the start node
     * @return the previous statement in the same block if it exists, null otherwise
     */
    public static Statement getPreviousSibling(Statement startNode) {
        return getSibling(startNode, true);
    }

    /**
     * Returns the previous statement in the source file if it exists.
     *
     * @param startNode the start node
     * @return the previous statement in the source file if it exists, null
     *         otherwise
     */
    public static Statement getPreviousStatement(Statement startNode) {
        final Statement previousSibling= getPreviousSibling(startNode);
        if (previousSibling != null) {
            return previousSibling;
        }
        final ASTNode parent= startNode.getParent();
        if (parent instanceof Statement) {
            return getPreviousStatement((Statement) parent);
        }
        return null;
    }

    /**
     * Returns the next body declaration in the same block if it exists.
     *
     * @param startNode the start node
     * @return the next body declaration in the same block if it exists, null
     *         otherwise
     */
    public static BodyDeclaration getNextSibling(BodyDeclaration startNode) {
        return getSibling(startNode, false);
    }

    /**
     * Returns the next statement in the same block if it exists.
     *
     * @param startNode the start node
     * @return the next statement in the same block if it exists, null otherwise
     */
    public static Statement getNextSibling(Statement startNode) {
        return getSibling(startNode, false);
    }

    /**
     * Returns the next statements in the same block if it exists.
     *
     * @param startNode the start node
     * @return the next statements in the same block if it exists, empty list
     *         otherwise
     */
    public static List<Statement> getNextSiblings(Statement startNode) {
        if (startNode.getParent() instanceof Block) {
            final List<Statement> statements= asList((Statement) startNode.getParent());
            final int indexOfNode= statements.indexOf(startNode);
            final int siblingIndex= indexOfNode + 1;
            if (0 <= siblingIndex && siblingIndex < statements.size()) {
                return statements.subList(siblingIndex, statements.size());
            }
        }
        return Collections.emptyList();
    }

    /**
     * Returns the next statement in the source file if it exists.
     *
     * @param startNode the start node
     * @return the next statement in the source file if it exists, null otherwise
     */
    public static Statement getNextStatement(Statement startNode) {
        final Statement nextSibling= getNextSibling(startNode);
        if (nextSibling != null) {
            return nextSibling;
        }
        final ASTNode parent= startNode.getParent();
        if (parent instanceof Statement) {
            return getNextStatement((Statement) parent);
        }
        return null;
    }

    private static BodyDeclaration getSibling(BodyDeclaration node, boolean lookForPrevious) {
        final ASTNode parent= node.getParent();
        if (parent instanceof AbstractTypeDeclaration) {
            final AbstractTypeDeclaration typeDecl= (AbstractTypeDeclaration) parent;
            return getSibling(node, typeDecl, lookForPrevious);
        }
        if (parent instanceof AnonymousClassDeclaration) {
            final AnonymousClassDeclaration classDecl= (AnonymousClassDeclaration) parent;
            return getSibling(node, classDecl, lookForPrevious);
        }
        if (parent instanceof CompilationUnit) {
            final CompilationUnit cu= (CompilationUnit) parent;
            final List<AbstractTypeDeclaration> types= types(cu);
            final int index= types.indexOf(node);
            if (index != -1 && index + 1 < types.size()) {
                return types.get(index + 1);
            }
        }
        return null;
    }

    private static BodyDeclaration getSibling(BodyDeclaration node, AbstractTypeDeclaration parent,
            boolean lookForPrevious) {
        return getSibling(node, bodyDeclarations(parent), lookForPrevious);
    }

    private static BodyDeclaration getSibling(BodyDeclaration node, AnonymousClassDeclaration parent,
            boolean lookForPrevious) {
        return getSibling(node, bodyDeclarations(parent), lookForPrevious);
    }

    private static BodyDeclaration getSibling(BodyDeclaration node, List<BodyDeclaration> bodyDeclarations,
            boolean lookForPrevious) {
        final TreeSet<BodyDeclaration> children= new TreeSet<BodyDeclaration>(new NodeStartPositionComparator());
        children.addAll(bodyDeclarations);

        BodyDeclaration previous= null;
        boolean returnNext= false;
        for (BodyDeclaration child : children) {
            if (lookForPrevious) {
                if (child.equals(node)) {
                    return previous;
                }
            } else if (returnNext) {
                return child;
            }
            previous= child;
            returnNext= child.equals(node);
        }
        return null;
    }

    private static Statement getSibling(Statement node, boolean isPrevious) {
        if (node.getParent() instanceof Block) {
            final List<Statement> statements= asList((Statement) node.getParent());
            final int indexOfNode= statements.indexOf(node);
            final int siblingIndex= isPrevious ? indexOfNode - 1 : indexOfNode + 1;
            if (0 <= siblingIndex && siblingIndex < statements.size()) {
                return statements.get(siblingIndex);
            }
        }
        return null;
    }

    /**
     * Returns the {@link ITypeBinding} of the {@link VariableDeclaration}.
     *
     * @param varDecl the variable declaration
     * @return the fragment's type binding, or null if none can be found
     */
    public static ITypeBinding resolveTypeBinding(final VariableDeclaration varDecl) {
        if (varDecl != null) {
            final IVariableBinding varBinding= varDecl.resolveBinding();
            if (varBinding != null) {
                return varBinding.getType();
            }
        }
        return null;
    }

    // AST checks

    /**
     * Returns whether the provided operator is the same as the one of provided
     * node.
     *
     * @param node     the node for which to test the operator
     * @param anOperator the first operator to test
     * @param operators the other operators to test
     * @return true if the provided node has the provided operator, false otherwise.
     */
    public static boolean hasOperator(Assignment node, Assignment.Operator anOperator, Assignment.Operator... operators) {
        return node != null && isOperatorInList(node.getOperator(), anOperator, operators);
    }

    /**
     * Returns whether the provided operator is the same as the one of provided
     * node.
     *
     * @param node     the node for which to test the operator
     * @param anOperator the first operator to test
     * @param operators the other operators to test
     * @return true if the provided node has the provided operator, false otherwise.
     */
    public static boolean hasOperator(InfixExpression node, InfixExpression.Operator anOperator, InfixExpression.Operator... operators) {
        return node != null && isOperatorInList(node.getOperator(), anOperator, operators);
    }

    /**
     * Returns whether the provided operator is the same as the one of provided
     * node.
     *
     * @param node     the node for which to test the operator
     * @param anOperator the first operator to test
     * @param operators the other operators to test
     * @return true if the provided node has the provided operator, false otherwise.
     */
    public static boolean hasOperator(PostfixExpression node, PostfixExpression.Operator anOperator, PostfixExpression.Operator... operators) {
        return node != null && isOperatorInList(node.getOperator(), anOperator, operators);
    }

    /**
     * Returns whether the provided operator is the same as the one of provided
     * node.
     *
     * @param node     the node for which to test the operator
     * @param anOperator the first operator to test
     * @param operators the other operators to test
     * @return true if the provided node has the provided operator, false otherwise.
     */
    public static boolean hasOperator(PrefixExpression node, PrefixExpression.Operator anOperator, PrefixExpression.Operator... operators) {
        return node != null && isOperatorInList(node.getOperator(), anOperator, operators);
    }

    private static boolean isOperatorInList(Object realOperator, Object anOperator, Object[] operators) {
        return realOperator != null && (realOperator.equals(anOperator) || Arrays.asList(operators).contains(realOperator));
    }

    /**
     * Returns whether the provided expression evaluates to exactly one of the
     * provided type.
     *
     * @param expression              the expression to analyze
     * @param oneOfQualifiedTypeNames the type binding qualified name must be equal
     *                                to one of these qualified type names
     * @return true if the provided expression evaluates to exactly one of the
     *         provided type, false otherwise
     */
    public static boolean hasType(Expression expression, String... oneOfQualifiedTypeNames) {
        return expression != null && hasType(expression.resolveTypeBinding(), oneOfQualifiedTypeNames);
    }

    /**
     * Returns whether the provided type binding is exactly one of the provided
     * type.
     *
     * @param typeBinding             the type binding to analyze
     * @param oneOfQualifiedTypeNames the type binding qualified name must be equal
     *                                to one of these qualified type names
     * @return {@code true} if the provided type binding is exactly one of the
     *         provided type, {@code false} otherwise
     */
    public static boolean hasType(final ITypeBinding typeBinding, String... oneOfQualifiedTypeNames) {
        if (typeBinding != null) {
            final String qualifiedName= typeBinding.getErasure().getQualifiedName();
            for (String qualifiedTypeName : oneOfQualifiedTypeNames) {
                if (qualifiedTypeName.equals(qualifiedName)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Returns whether the provided expressions evaluate to the same type.
     *
     * @param expr1 the first expression to analyze
     * @param expr2 the second expression to analyze
     * @return {@code true} if the provided expression evaluates to exactly one of
     *         the provided type, {@code false} otherwise
     */
    public static boolean haveSameType(Expression expr1, Expression expr2) {
        return expr1 != null && expr2 != null && Utils.equalNotNull(expr1.resolveTypeBinding(), expr2.resolveTypeBinding());
    }

    /**
     * Returns whether the provided expression is an instance of the qualified type
     * name.
     *
     * @param expression        the expression to analyze
     * @param qualifiedTypeName the qualified type name
     * @return {@code true} if the provided expression is an instance of the
     *         qualified type name, {@code false} otherwise
     */
    public static boolean instanceOf(Expression expression, String qualifiedTypeName) {
        return expression != null && instanceOf(expression.resolveTypeBinding(), qualifiedTypeName);
    }

    /**
     * Returns whether the provided type binding is an instance of the qualified
     * type name.
     *
     * @param typeBinding       the type binding to analyze
     * @param qualifiedTypeName the qualified type name
     * @return true if the provided type binding is an instance of the qualified
     *         type name, false otherwise
     */
    public static boolean instanceOf(ITypeBinding typeBinding, String qualifiedTypeName) {
        return findImplementedType(typeBinding, qualifiedTypeName) != null;
    }

    /**
     * Returns whether the provided expression represents an array.
     *
     * @param expression the expression to analyze
     * @return true the provided expression represents an array, false otherwise
     */
    public static boolean isArray(Expression expression) {
        if (expression != null) {
            final ITypeBinding typeBinding= expression.resolveTypeBinding();
            return typeBinding != null && typeBinding.isArray();
        }
        return false;
    }

    /**
     * Returns whether the provided expression represents a constant value.
     *
     * @param expression the expression to analyze
     * @return true the provided expression represents a constant value, false
     *         otherwise
     */
    public static boolean isConstant(final Expression expression) {
        return (expression != null && expression.resolveConstantExpressionValue() != null) || isEnumConstant(expression);
    }

    private static boolean isEnumConstant(final Expression expression) {
        // TODO JNR make it work for enums fields which are static final, but not null
        if (expression instanceof Name) {
            final IBinding binding= ((Name) expression).resolveBinding();
            if (binding instanceof IVariableBinding) {
                return ((IVariableBinding) binding).isEnumConstant();
            }
        }
        return false;
    }

    /**
     * Returns whether the provided expression is hard-coded as a literal in the
     * byte code ignoring parentheses.
     *
     * @param expression the expression to check
     * @return true if the provided expression is hard-coded as a literal in the
     *         byte code ignoring parentheses, false otherwise
     */
    public static boolean isHardCoded(final Expression expression) {
        if (expression == null) {
            return false;
        }
        switch (expression.getNodeType()) {
        case ASTNode.BOOLEAN_LITERAL:
        case ASTNode.CHARACTER_LITERAL:
        case ASTNode.NUMBER_LITERAL:
        case ASTNode.STRING_LITERAL:
        case ASTNode.NULL_LITERAL:
            return true;

        case ASTNode.INFIX_EXPRESSION:
            InfixExpression infixExpression= (InfixExpression) expression;

            if (!isHardCoded(infixExpression.getLeftOperand()) || !isHardCoded(infixExpression.getRightOperand())) {
                return false;
            } else if (infixExpression.hasExtendedOperands()) {
                for (Object operand : infixExpression.extendedOperands()) {
                    if (!isHardCoded((Expression) operand)) {
                        return false;
                    }
                }
            }

            return true;

        case ASTNode.PREFIX_EXPRESSION:
            PrefixExpression prefixExpression= (PrefixExpression) expression;
            return isHardCoded(prefixExpression.getOperand());

        case ASTNode.POSTFIX_EXPRESSION:
            PostfixExpression postfixExpression= (PostfixExpression) expression;
            return isHardCoded(postfixExpression.getOperand());

        case ASTNode.CAST_EXPRESSION:
            return isHardCoded(((CastExpression) expression).getExpression());

        case ASTNode.PARENTHESIZED_EXPRESSION:
            return isHardCoded(((ParenthesizedExpression) expression).getExpression());

        default:
            return expression.resolveConstantExpressionValue() != null || isEnumConstant(expression);
        }
    }

    /**
     * Integer literal.
     *
     * @param input The input
     * @return Integer literal.
     */
    public static Long integerLiteral(Expression input) {
        NumberLiteral contant= as(input, NumberLiteral.class);
        PrefixExpression negativeContant= as(input, PrefixExpression.class);

        if (negativeContant != null && hasOperator(negativeContant, PrefixExpression.Operator.MINUS)) {
            Long value= integerLiteral(negativeContant.getOperand());

            if (value != null) {
                return -value;
            }
        } else if (contant != null) {
            return positiveLiteral(contant);
        }

        return null;
    }

    /**
     * Positive literal.
     *
     * @param input The input
     * @return Positive literal.
     */
    public static Long positiveLiteral(NumberLiteral input) {
        Object number= input.resolveConstantExpressionValue();

        if (number instanceof Short) {
            return (long) ((Short) number).intValue();
        }

        if (number instanceof Integer) {
            return (long) ((Integer) number).intValue();
        }

        if (number instanceof Long) {
            return (Long) number;
        }

        return null;
    }

    /**
     * Returns whether the provided binding represents a local variable.
     *
     * @param binding the binding to analyze
     * @return {@code true} if the provided binding represents a local variable,
     *         {@code false} otherwise
     */
    public static boolean isLocalVariable(IBinding binding) {
        if (binding != null && binding.getKind() == IBinding.VARIABLE) {
            final IVariableBinding bnd= (IVariableBinding) binding;
            return !bnd.isField() && !bnd.isEnumConstant();
        }
        return false;
    }

    /**
     * Returns whether the provided binding and expression represent the same local
     * variable.
     *
     * @param binding    the binding to analyze
     * @param expression the expression to analyze
     * @return {@code true} if the provided binding and expression represent the
     *         same local variable, {@code false} otherwise
     */
    public static boolean isSameLocalVariable(IBinding binding, Expression expression) {
        return isLocalVariable(binding) && expression != null && expression.getNodeType() == SIMPLE_NAME
        // No need to use IVariableBinding.isEqualTo(IBinding) since we are looking for
        // a *local* variable
                && binding.equals(((SimpleName) expression).resolveBinding());
    }

    /**
     * Returns whether the provided expressions represent the same local variable.
     *
     * @param expr1 the first expression to analyze
     * @param expr2 the second expression to analyze
     * @return {@code true} if the provided expressions represent the same local
     *         variable, {@code false} otherwise
     */
    public static boolean isSameLocalVariable(Expression expr1, Expression expr2) {
        return expr1 != null && expr1.getNodeType() == SIMPLE_NAME
                && isSameLocalVariable(((SimpleName) expr1).resolveBinding(), expr2);
    }

    /**
     * Returns whether the provided variable declaration and expression represent
     * the same local variable.
     *
     * @param varDecl    the variable declaration to analyze
     * @param expression the expression to analyze
     * @return {@code true} if the provided nodes represent the same local variable,
     *         {@code false} otherwise
     */
    public static boolean isSameLocalVariable(VariableDeclaration varDecl, Expression expression) {
        return varDecl != null && isSameLocalVariable(varDecl.resolveBinding(), expression);
    }

    /**
     * Returns whether the provided expression evaluates to a primitive type.
     *
     * @param expression        the expression to analyze
     * @param primitiveTypeName the primitive type name
     * @return true if the provided expression evaluates to a primitive type, false
     *         otherwise
     */
    public static boolean isPrimitive(Expression expression, String primitiveTypeName) {
        return expression != null && isPrimitive(expression.resolveTypeBinding(), primitiveTypeName);
    }

    /**
     * Returns whether the provided expression evaluates to a primitive type.
     *
     * @param expression the expression to analyze
     * @return true if the provided expression evaluates to a primitive type, false
     *         otherwise
     */
    public static boolean isPrimitive(Expression expression) {
        return expression != null && isPrimitive(expression.resolveTypeBinding());
    }

    /**
     * Returns whether the provided type binding represents the provided primitive
     * type.
     *
     * @param typeBinding       the type binding to analyze
     * @param primitiveTypeName the primitive type name
     * @return true if the provided type binding represents the provided primitive
     *         type, false otherwise
     */
    public static boolean isPrimitive(ITypeBinding typeBinding, String primitiveTypeName) {
        return typeBinding != null && typeBinding.isPrimitive()
                && typeBinding.getQualifiedName().equals(primitiveTypeName);
    }

    /**
     * Returns whether the provided type binding represents a primitive type.
     *
     * @param typeBinding the type binding to analyze
     * @return true if the provided type binding represents a primitive type, false
     *         otherwise
     */
    public static boolean isPrimitive(ITypeBinding typeBinding) {
        return typeBinding != null && typeBinding.isPrimitive()
                && Arrays.asList(boolean.class.getSimpleName(), byte.class.getSimpleName(), char.class.getSimpleName(), short.class.getSimpleName(), int.class.getSimpleName(), long.class.getSimpleName(), float.class.getSimpleName(), double.class.getSimpleName())
                        .contains(typeBinding.getQualifiedName());
    }

    /**
     * Returns the type binding for the provided qualified type name if it can be
     * found in the type hierarchy of the provided type binding.
     *
     * @param typeBinding       the type binding to analyze
     * @param qualifiedTypeName the qualified type name to find
     * @return the type binding for the provided qualified type name if it can be
     *         found in the type hierarchy of the provided type binding, or
     *         {@code null} otherwise
     */
    public static ITypeBinding findImplementedType(ITypeBinding typeBinding, String qualifiedTypeName) {
        if (typeBinding == null) {
            return null;
        }
        final ITypeBinding typeErasure= typeBinding.getErasure();
        if (qualifiedTypeName.equals(typeBinding.getQualifiedName())
                || qualifiedTypeName.equals(typeErasure.getQualifiedName())) {
            return typeBinding;
        }
        final Set<String> visitedClasses= new HashSet<>();
        visitedClasses.add(typeErasure.getQualifiedName());
        return findImplementedType(typeBinding, qualifiedTypeName, visitedClasses);
    }

    private static ITypeBinding findImplementedType(ITypeBinding typeBinding, String qualifiedTypeName,
            Set<String> visitedInterfaces) {
        final ITypeBinding superclass= typeBinding.getSuperclass();
        if (superclass != null) {
            final String superClassQualifiedName= superclass.getErasure().getQualifiedName();
            if (qualifiedTypeName.equals(superClassQualifiedName)) {
                return superclass;
            }
            visitedInterfaces.add(superClassQualifiedName);
            final ITypeBinding implementedType= findImplementedType(superclass, qualifiedTypeName, visitedInterfaces);
            if (implementedType != null) {
                return implementedType;
            }
        }
        for (ITypeBinding itfBinding : typeBinding.getInterfaces()) {
            final String itfQualifiedName= itfBinding.getErasure().getQualifiedName();
            if (qualifiedTypeName.equals(itfQualifiedName)) {
                return itfBinding;
            }
            visitedInterfaces.add(itfQualifiedName);
            final ITypeBinding implementedType= findImplementedType(itfBinding, qualifiedTypeName, visitedInterfaces);
            if (implementedType != null) {
                return implementedType;
            }
        }
        return null;
    }

    /**
     * Returns the null-checked expression if the provided node is a null check.
     *
     * @param expression the suspected null-checked expression
     * @return the null-checked expression if the provided node is a null-check, or
     *         {@code null} otherwise.
     */
    public static Expression getNullCheckedExpression(Expression expression) {
        final Expression e= getUnparenthesedExpression(expression);
        if (e instanceof InfixExpression) {
            final InfixExpression ie= (InfixExpression) e;
            if (hasOperator(ie, InfixExpression.Operator.NOT_EQUALS) && checkNoExtendedOperands(ie)) {
                if (is(ie.getLeftOperand(), NullLiteral.class)) {
                    return ie.getRightOperand();
                }
                if (is(ie.getRightOperand(), NullLiteral.class)) {
                    return ie.getLeftOperand();
                }
            }
        }
        return null;
    }

    /**
     * Extended operands are used for deeply nested expressions, mostly string
     * concatenation expressions.
     * <p>
     * This will be implemented only if somebody comes up with code where the
     * runtime exception is thrown.
     * </p>
     *
     * @param node the infix expression that must not have extended operands
     * @return {@code true}, or it throws
     */
    public static boolean checkNoExtendedOperands(InfixExpression node) {
        if (!hasType(node, String.class.getCanonicalName()) && node.hasExtendedOperands()) {
            throw new NotImplementedException(node, "for extended operands"); //$NON-NLS-1$
        }
        return true;
    }

    /**
     * Returns the unique {@link VariableDeclarationFragment} declared in the
     * provided {@link VariableDeclarationStatement}.
     *
     * @param node the statement from which to extract the unique fragment
     * @return the unique fragment declared in the provided variable declaration
     *         statement, or {@code null} if more than one exist.
     */
    public static VariableDeclarationFragment getUniqueFragment(VariableDeclarationStatement node) {
        if (node == null) {
            return null;
        }
        final List<VariableDeclarationFragment> fragments= fragments(node);
        return fragments.size() == 1 ? fragments.get(0) : null;
    }

    /**
     * Returns whether the provided node defines a loop.
     *
     * @param node the node
     * @return true if the provided node defines a loop, false otherwise
     */
    public static boolean isLoop(ASTNode node) {
        return node instanceof DoStatement || node instanceof EnhancedForStatement || node instanceof ForStatement
                || node instanceof WhileStatement;
    }

    /**
     * Returns whether the provided node is breakable.
     *
     * @param node the node
     * @return true if the provided node is breakable, false otherwise
     */
    public static boolean isBreakable(ASTNode node) {
        return isLoop(node) || node instanceof SwitchStatement;
    }

    /**
     * Returns whether a checked exception is supposed to be caught.
     *
     * @param node the node
     * @return true if a checked exception is supposed to be caught.
     */
    public static boolean isExceptionExpected(final ASTNode node) {
        ASTNode parentNode= getFirstAncestorOrNull(node, TryStatement.class, BodyDeclaration.class);

        while (parentNode instanceof TryStatement) {
            TryStatement tryStatement= (TryStatement) parentNode;

            for (Object object : tryStatement.catchClauses()) {
                CatchClause catchClause= (CatchClause) object;

                if (catchClause.getException().getType() != null
                        && !instanceOf(catchClause.getException().getType().resolveBinding(),
                                RuntimeException.class.getCanonicalName())) {
                    return true;
                }
            }

            parentNode= getFirstAncestorOrNull(parentNode, TryStatement.class, BodyDeclaration.class);
        }

        return false;
    }

    /**
     * Returns whether the provided qualified name accesses a field with the
     * provided signature.
     *
     * @param node              the qualified name to compare
     * @param qualifiedTypeName the qualified name of the type declaring the field
     * @param fieldNames        the field names
     * @return true if the provided qualified name matches the provided field
     *         signature, false otherwise
     */
    public static boolean isField(QualifiedName node, String qualifiedTypeName, String... fieldNames) {
        return instanceOf(node, qualifiedTypeName)
                && Arrays.asList(fieldNames).contains(node.getName().getIdentifier());
    }

    /**
     * Returns whether the provided method invocation invokes a method with the
     * provided method signature. The method signature is compared against the
     * erasure of the invoked method.
     *
     * @param node                         the method invocation to compare
     * @param typeQualifiedName            the qualified name of the type declaring
     *                                     the method
     * @param methodName                   the method name
     * @param parameterTypesQualifiedNames the qualified names of the parameter
     *                                     types
     * @return true if the provided method invocation matches the provided method
     *         signature, false otherwise
     */
    public static boolean usesGivenSignature(MethodInvocation node, String typeQualifiedName, String methodName,
            String... parameterTypesQualifiedNames) {
        return node != null
                && usesGivenSignature(node.resolveMethodBinding(), typeQualifiedName, methodName, parameterTypesQualifiedNames);
    }

    /**
     * Returns whether the provided method declaration declares a method with the
     * provided method signature. The method signature is compared against the
     * erasure of the declared method.
     *
     * @param node                         the method declaration to compare
     * @param typeQualifiedName            the qualified name of the type declaring
     *                                     the method
     * @param methodName                   the method name
     * @param parameterTypesQualifiedNames the qualified names of the parameter
     *                                     types
     * @return true if the provided method declaration matches the provided method
     *         signature, false otherwise
     */
    public static boolean usesGivenSignature(MethodDeclaration node, String typeQualifiedName, String methodName,
            String... parameterTypesQualifiedNames) {
        return node != null
                && usesGivenSignature(node.resolveBinding(), typeQualifiedName, methodName, parameterTypesQualifiedNames);
    }

    /**
     * Returns whether the provided method binding has the provided method
     * signature. The method signature is compared against the erasure of the
     * invoked method.
     *
     * @param methodBinding                the method binding to compare
     * @param typeQualifiedName            the qualified name of the type declaring
     *                                     the method
     * @param methodName                   the method name
     * @param parameterTypesQualifiedNames the qualified names of the parameter
     *                                     types
     * @return true if the provided method invocation matches the provided method
     *         signature, false otherwise
     */
    public static boolean usesGivenSignature(IMethodBinding methodBinding, String typeQualifiedName, String methodName,
            String... parameterTypesQualifiedNames) {
        // Let's do the fast checks first
        if (methodBinding == null || !methodName.equals(methodBinding.getName())
                || methodBinding.getParameterTypes().length != parameterTypesQualifiedNames.length) {
            return false;
        }
        // OK more heavy checks now
        final ITypeBinding declaringClass= methodBinding.getDeclaringClass();
        final ITypeBinding implementedType= findImplementedType(declaringClass, typeQualifiedName);
        final boolean isInstanceOf= instanceOf(declaringClass, typeQualifiedName);
        if (parameterTypesMatch(implementedType, isInstanceOf, methodBinding, parameterTypesQualifiedNames)) {
            return true;
        }
        // A lot more heavy checks
        // FIXME find a more efficient way to do this. It would be awesome
        // if an API to directly find the overriddenMethod IMethodBinding existed
        IMethodBinding overriddenMethod= findOverridenMethod(declaringClass, typeQualifiedName, methodName,
                parameterTypesQualifiedNames);
        if (overriddenMethod != null && methodBinding.overrides(overriddenMethod)) {
            return true;
        }
        IMethodBinding methodDeclaration= methodBinding.getMethodDeclaration();
        return methodDeclaration != null && methodDeclaration != methodBinding
                && usesGivenSignature(methodDeclaration, typeQualifiedName, methodName, parameterTypesQualifiedNames);
    }

    private static boolean parameterTypesMatch(ITypeBinding implementedType, boolean isInstanceOf,
            IMethodBinding methodBinding, String[] parameterTypesQualifiedNames) {
        if (implementedType != null && !implementedType.isRawType()) {
            final ITypeBinding erasure= implementedType.getErasure();
            if (erasure.isGenericType() || erasure.isParameterizedType()) {
                return parameterizedTypesMatch(implementedType, erasure, methodBinding);
            }
        }
        return isInstanceOf && concreteTypesMatch(methodBinding.getParameterTypes(), parameterTypesQualifiedNames);
    }

    private static boolean concreteTypesMatch(ITypeBinding[] typeBindings, String... typesQualifiedNames) {
        if (typeBindings.length != typesQualifiedNames.length) {
            return false;
        }
        for (int i= 0; i < typesQualifiedNames.length; i++) {
            if (!typesQualifiedNames[i].equals(typeBindings[i].getQualifiedName())
                    && !typesQualifiedNames[i].equals(Bindings.getBoxedTypeName(typeBindings[i].getQualifiedName()))
                    && !typesQualifiedNames[i].equals(Bindings.getUnboxedTypeName(typeBindings[i].getQualifiedName()))) {
                return false;
            }
        }
        return true;
    }

    private static boolean parameterizedTypesMatch(final ITypeBinding clazz, final ITypeBinding clazzErasure,
            IMethodBinding methodBinding) {
        if (clazz.isParameterizedType() && !clazz.equals(clazzErasure)) {
            final Map<ITypeBinding, ITypeBinding> genericToConcreteTypeParamsFromClass= getGenericToConcreteTypeParamsMap(
                    clazz, clazzErasure);

            for (IMethodBinding declaredMethod : clazzErasure.getDeclaredMethods()) {
                if (declaredMethod.getName().equals(methodBinding.getName())) {
                    final Map<ITypeBinding, ITypeBinding> genericToConcreteTypeParams= getGenericToConcreteTypeParamsMap(
                            methodBinding, declaredMethod);
                    genericToConcreteTypeParams.putAll(genericToConcreteTypeParamsFromClass);

                    if (parameterizedTypesMatch2(genericToConcreteTypeParams, methodBinding, declaredMethod)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    private static Map<ITypeBinding, ITypeBinding> getGenericToConcreteTypeParamsMap(final IMethodBinding method,
            final IMethodBinding methodErasure) {
        return getGenericToConcreteTypeParamsMap(method.getTypeArguments(), methodErasure.getTypeParameters());
    }

    private static Map<ITypeBinding, ITypeBinding> getGenericToConcreteTypeParamsMap(final ITypeBinding clazz,
            final ITypeBinding clazzErasure) {
        return getGenericToConcreteTypeParamsMap(clazz.getTypeArguments(), clazzErasure.getTypeParameters());
    }

    private static Map<ITypeBinding, ITypeBinding> getGenericToConcreteTypeParamsMap(final ITypeBinding[] typeParams,
            final ITypeBinding[] genericTypeParams) {
        final Map<ITypeBinding, ITypeBinding> results= new HashMap<>();
        for (int i= 0; i < genericTypeParams.length && i < typeParams.length; i++) {
            results.put(genericTypeParams[i], typeParams[i]);
        }
        return results;
    }

    private static boolean parameterizedTypesMatch2(Map<ITypeBinding, ITypeBinding> genericToConcreteTypeParams,
            IMethodBinding parameterizedMethod, IMethodBinding genericMethod) {
        final ITypeBinding[] paramTypes= parameterizedMethod.getParameterTypes();
        final ITypeBinding[] genericParamTypes= genericMethod.getParameterTypes();
        if (paramTypes.length != genericParamTypes.length) {
            return false;
        }
        for (int i= 0; i < genericParamTypes.length; i++) {
            ITypeBinding genericParamType= genericParamTypes[i];

            ITypeBinding concreteParamType= null;

            if (genericParamType.isArray()) {
                ITypeBinding concreteElementType= genericToConcreteTypeParams.get(genericParamType.getElementType());
                if (concreteElementType != null) {
                    concreteParamType= concreteElementType.createArrayType(genericParamType.getDimensions());
                }
            } else {
                concreteParamType= genericToConcreteTypeParams.get(genericParamType);
            }

            if (concreteParamType == null) {
                concreteParamType= genericParamType;
            }

            final ITypeBinding erasure1= paramTypes[i].getErasure();
            final String erasureName1;
            if (erasure1.isPrimitive()) {
                erasureName1= Bindings.getBoxedTypeName(erasure1.getQualifiedName());
            } else {
                erasureName1= erasure1.getQualifiedName();
            }

            final ITypeBinding erasure2= concreteParamType.getErasure();
            final String erasureName2;
            if (erasure2.isPrimitive()) {
                erasureName2= Bindings.getBoxedTypeName(erasure2.getQualifiedName());
            } else {
                erasureName2= erasure2.getQualifiedName();
            }

            if (!erasureName1.equals(erasureName2)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Returns a set made of all the method bindings which are overridden by the
     * provided method binding.
     *
     * @param overridingMethod the overriding method binding
     * @return a set made of all the method bindings which are overridden by the
     *         provided method binding
     */
    public static Set<IMethodBinding> getOverridenMethods(IMethodBinding overridingMethod) {
        final Set<IMethodBinding> results= new HashSet<>();
        findOverridenMethods(overridingMethod, results, overridingMethod.getDeclaringClass());
        return results;
    }

    private static void findOverridenMethods(IMethodBinding overridingMethod, Set<IMethodBinding> results,
            ITypeBinding declaringClass) {
        final ITypeBinding superclass= declaringClass.getSuperclass();
        if (superclass != null && !addOverridenMethods(overridingMethod, superclass, results)) {
            findOverridenMethods(overridingMethod, results, superclass);
        }

        for (ITypeBinding itf : declaringClass.getInterfaces()) {
            if (!addOverridenMethods(overridingMethod, itf, results)) {
                findOverridenMethods(overridingMethod, results, itf);
            }
        }
    }

    private static boolean addOverridenMethods(IMethodBinding overridingMethod, ITypeBinding superType,
            Set<IMethodBinding> results) {
        for (IMethodBinding methodFromType : superType.getDeclaredMethods()) {
            if (overridingMethod.overrides(methodFromType) && !results.add(methodFromType)) {
                // Type has already been visited
                return true;
            }
        }
        return false;
    }

    private static IMethodBinding findOverridenMethod(ITypeBinding typeBinding, String typeQualifiedName,
            String methodName, String[] parameterTypesQualifiedNames) {
        // Superclass
        ITypeBinding superclassBinding= typeBinding.getSuperclass();
        if (superclassBinding != null) {
            superclassBinding= superclassBinding.getErasure();
            if (typeQualifiedName.equals(superclassBinding.getErasure().getQualifiedName())) {
                // Found the type
                return findOverridenMethod(methodName, parameterTypesQualifiedNames,
                        superclassBinding.getDeclaredMethods());
            }
            IMethodBinding overridenMethod= findOverridenMethod(superclassBinding, typeQualifiedName, methodName,
                    parameterTypesQualifiedNames);
            if (overridenMethod != null) {
                return overridenMethod;
            }
        }
        // Interfaces
        for (ITypeBinding itfBinding : typeBinding.getInterfaces()) {
            itfBinding= itfBinding.getErasure();
            if (typeQualifiedName.equals(itfBinding.getQualifiedName())) {
                // Found the type
                return findOverridenMethod(methodName, parameterTypesQualifiedNames, itfBinding.getDeclaredMethods());
            }
            IMethodBinding overridenMethod= findOverridenMethod(itfBinding, typeQualifiedName, methodName,
                    parameterTypesQualifiedNames);
            if (overridenMethod != null) {
                return overridenMethod;
            }
        }
        return null;
    }

    private static IMethodBinding findOverridenMethod(String methodName, String[] parameterTypesQualifiedNames,
            IMethodBinding[] declaredMethods) {
        for (IMethodBinding methodBinding : declaredMethods) {
            final IMethodBinding methodDecl= methodBinding.getMethodDeclaration();
            if (methodBinding.getName().equals(methodName) && methodDecl != null
                    && concreteTypesMatch(methodDecl.getParameterTypes(), parameterTypesQualifiedNames)) {
                return methodBinding;
            }
        }
        return null;
    }

    /**
     * Returns whether the two names are equal.
     *
     * @param name1 the first name to compare
     * @param name2 the second name to compare
     * @return true if the two names are equal, false otherwise.
     */
    public static boolean isEqual(Name name1, Name name2) {
        if (name1 instanceof SimpleName && name2 instanceof SimpleName) {
            return isEqual((SimpleName) name1, (SimpleName) name2);
        }
        return name1 instanceof QualifiedName && name2 instanceof QualifiedName && isEqual((QualifiedName) name1, (QualifiedName) name2);
    }

    /**
     * Returns whether the two simple names are equal.
     *
     * @param name1 the first simple name to compare
     * @param name2 the second simple name to compare
     * @return true if the two simple names are equal, false otherwise.
     */
    public static boolean isEqual(SimpleName name1, SimpleName name2) {
        return name1.getIdentifier().equals(name2.getIdentifier());
    }

    /**
     * Returns whether the two qualified names are equal.
     *
     * @param name1 the first qualified name to compare
     * @param name2 the second qualified name to compare
     * @return true if the two qualified names are equal, false otherwise.
     */
    public static boolean isEqual(QualifiedName name1, QualifiedName name2) {
        return isEqual(name1.getName(), name2.getName()) && isEqual(name1.getQualifier(), name2.getQualifier());
    }

    /**
     * Returns whether the two provided codes structurally match.
     *
     * @param referenceStatements the first code to compare
     * @param comparedStatements  the second code to compare
     * @return true if the two provided codes structurally match, false otherwise
     */
    public static boolean match(final List<Statement> referenceStatements, final List<Statement> comparedStatements) {
        if (referenceStatements.size() != comparedStatements.size()) {
            return false;
        }

        final ASTSemanticMatcher matcher= new ASTSemanticMatcher();

        for (int codeLine= 0; codeLine < referenceStatements.size(); codeLine++) {
            if (!match(matcher, referenceStatements.get(codeLine), comparedStatements.get(codeLine))) {
                return false;
            }
        }
        return true;
    }

    /**
     * Returns whether the two provided nodes structurally match.
     *
     * @param node1 the first node to compare
     * @param node2 the second node to compare
     * @return true if the two provided nodes structurally match, false otherwise
     */
    public static boolean match(ASTNode node1, ASTNode node2) {
        return match(new ASTSemanticMatcher(), node1, node2);
    }

    /**
     * Returns whether the two provided nodes structurally match.
     *
     * @param matcher the AST matcher
     * @param node1   the first node to compare
     * @param node2   the second node to compare
     * @return true if the two provided nodes structurally match, false otherwise
     */
    public static boolean match(ASTSemanticMatcher matcher, ASTNode node1, ASTNode node2) {
        return matcher.safeSubtreeMatch(node1, node2);
    }

    private static boolean areVariableBindingsEqual(ASTNode node1, ASTNode node2) {
        return areBindingsEqual(varBinding(node1), varBinding(node2));
    }

    /**
     * Returns whether to bindings are equal.
     *
     * @param b1 the first binding
     * @param b2 the second binding
     * @return {@code true} when bindings are equal, {@code false} otherwise
     */
    public static boolean areBindingsEqual(final IBinding b1, final IBinding b2) {
        return b1 != null && b2 != null && b1.isEqualTo(b2);
    }

    private static IBinding varBinding(ASTNode node) {
        switch (node.getNodeType()) {
        case THIS_EXPRESSION:
            return ((ThisExpression) node).resolveTypeBinding();

        case FIELD_ACCESS:
            return ((FieldAccess) node).resolveFieldBinding();

        case QUALIFIED_NAME:
        case SIMPLE_NAME:
            return ((Name) node).resolveBinding();

        case SINGLE_VARIABLE_DECLARATION:
        case VARIABLE_DECLARATION_FRAGMENT:
            return ((VariableDeclaration) node).resolveBinding();

        default:
            return null;
        }
    }

    /**
     * Returns whether the two provided names represent the same variable.
     *
     * @param name1 the first name to compare
     * @param name2 the second name to compare
     * @return true if the two provided names represent the same variable, false
     *         otherwise
     */
    public static boolean isSameVariable(SimpleName name1, QualifiedName name2) {
        return false;
    }

    /**
     * Returns whether the two provided names represent the same variable.
     *
     * @param name1 the first name to compare
     * @param name2 the second name to compare
     * @return true if the two provided names represent the same variable, false
     *         otherwise
     */
    public static boolean isSameVariable(SimpleName name1, SimpleName name2) {
        return areVariableBindingsEqual(name1, name2);
    }

    /**
     * Returns whether the two provided expressions represent the same variable.
     *
     * @param name1  the first expression to compare
     * @param field2 the second expression to compare
     * @return true if the two provided expressions represent the same variable,
     *         false otherwise
     */
    public static boolean isSameVariable(SimpleName name1, FieldAccess field2) {
        return as(field2.getExpression(), ThisExpression.class) != null && areVariableBindingsEqual(field2, name1);
    }

    /**
     * Returns whether the two provided qualified names represent the same variable.
     *
     * @param name1 the first qualified name to compare
     * @param name2 the second qualified name to compare
     * @return true if the two provided qualified names represent the same variable,
     *         false otherwise
     */
    public static boolean isSameVariable(QualifiedName name1, QualifiedName name2) {
        return areVariableBindingsEqual(name1, name2) && isSameVariable(name1.getQualifier(), name2.getQualifier());
    }

    /**
     * Returns whether the two provided expressions represent the same variable.
     *
     * @param name1  the first expression to compare
     * @param field2 the second expression to compare
     * @return true if the two provided expressions represent the same variable,
     *         false otherwise
     */
    public static boolean isSameVariable(QualifiedName name1, FieldAccess field2) {
        return areVariableBindingsEqual(name1, field2) && isSameVariable(field2.getExpression(), name1.getQualifier());
    }

    /**
     * Returns whether the two provided field accesses represent the same variable.
     *
     * @param field1 the first field access to compare
     * @param field2 the second field access to compare
     * @return true if the two provided field accesses represent the same variable,
     *         false otherwise
     */
    public static boolean isSameVariable(FieldAccess field1, FieldAccess field2) {
        return areVariableBindingsEqual(field1, field2)
                && isSameVariable(field1.getExpression(), field2.getExpression());
    }

    /**
     * Returns whether the provided nodes all represent the same variable.
     *
     * @param node0      the first node to compare
     * @param otherNodes the other nodes to compare
     * @return true if all the provided nodes represent the same variable, false
     *         otherwise
     */
    public static boolean areSameVariables(final ASTNode node0, final ASTNode... otherNodes) {
        for (ASTNode nodeN : otherNodes) {
            if (!isSameVariable(node0, nodeN)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Returns whether the two provided nodes represent the same variable.
     *
     * @param node1 the first node to compare
     * @param node2 the second node to compare
     * @return true if the two provided nodes represent the same variable, false
     *         otherwise
     */
    public static boolean isSameVariable(ASTNode node1, ASTNode node2) {
        node1= getUnparenthesedExpression(node1);
        node2= getUnparenthesedExpression(node2);

        if (node1 == null || node2 == null) {
            return false;
        }

        switch (node1.getNodeType()) {
        case THIS_EXPRESSION:
            return node2.getNodeType() == THIS_EXPRESSION;

        case SIMPLE_NAME:
            final SimpleName sn= (SimpleName) node1;
            switch (node2.getNodeType()) {
            case QUALIFIED_NAME:
                return isSameVariable(sn, (QualifiedName) node2);
            case FIELD_ACCESS:
                return isSameVariable(sn, (FieldAccess) node2);
            }
            break;

        case QUALIFIED_NAME:
            final QualifiedName qn= (QualifiedName) node1;
            switch (node2.getNodeType()) {
            case SIMPLE_NAME:
                return isSameVariable((SimpleName) node2, qn);
            case QUALIFIED_NAME:
                return isSameVariable(qn, (QualifiedName) node2);
            case FIELD_ACCESS:
                return isSameVariable(qn, (FieldAccess) node2);
            }
            break;

        case FIELD_ACCESS:
            final FieldAccess fa= (FieldAccess) node1;
            switch (node2.getNodeType()) {
            case SIMPLE_NAME:
                return isSameVariable((SimpleName) node2, fa);
            case QUALIFIED_NAME:
                return isSameVariable((QualifiedName) node2, fa);
            case FIELD_ACCESS:
                return isSameVariable(fa, (FieldAccess) node2);
            }
        }

        return areVariableBindingsEqual(node1, node2);
    }

    /**
     * Returns the last parent node whose class is part of the included classes list
     * or the provided node otherwise.
     *
     * @param node            the node
     * @param includedClasses the classes to include when looking for the parent
     *                        node
     * @return the last parent node of the provided classes, or the current node
     *         otherwise
     */
    public static ASTNode getParent(ASTNode node, Class<?>... includedClasses) {
        final ASTNode parent= node.getParent();
        if (instanceOf(parent, includedClasses)) {
            return getParent(parent, includedClasses);
        }
        return node;
    }

    private static boolean instanceOf(ASTNode node, Class<?>... clazzes) {
        if (node == null) {
            return false;
        }
        for (Class<?> clazz : clazzes) {
            if (clazz.isAssignableFrom(node.getClass())) {
                return true;
            }
        }
        return false;
    }

    /**
     * Returns the first parent node which has a different type that the provided
     * ignored classes.
     *
     * @param node           the node
     * @param ignoredClasses the classes to ignore when looking for the parent node
     * @return the parent node by ignoring the provided types
     */
    public static ASTNode getParentIgnoring(ASTNode node, Class<?>... ignoredClasses) {
        final ASTNode parent= node.getParent();
        if (parent == null) {
            return node;
        }
        if (instanceOf(parent, ignoredClasses)) {
            return getParentIgnoring(parent, ignoredClasses);
        }
        return parent;
    }

    /**
     * Returns the file name where the node comes from, or "FakeClass.java" if this
     * is a fake node.
     *
     * @param node the node
     * @return the file name where the node comes from, or "FakeClass.java" if this
     *         is a fake node.
     */
    public static String getFileName(ASTNode node) {
        if (node.getRoot() instanceof CompilationUnit) {
            CompilationUnit cu= (CompilationUnit) node.getRoot();
            if (cu.getTypeRoot() != null) { // added for unit tests
                return cu.getTypeRoot().getElementName();
            }
            return "FakeClass.java"; //$NON-NLS-1$
        }
        return null;
    }

    /**
     * Returns a string suitable for identifying a location in the source.
     *
     * @param node the node from which to retrieve the source location
     * @return a string suitable for identifying a location in the source
     */
    public static String getSourceLocation(ASTNode node) {
        final ASTNode root= node != null ? node.getRoot() : null;
        if (root instanceof CompilationUnit) {
            final CompilationUnit cu= (CompilationUnit) root;
            final int position= node.getStartPosition();
            final int line= cu.getLineNumber(position);
            final int column= cu.getColumnNumber(position) + 1;
            if (cu.getTypeRoot() != null) {
                return cu.getTypeRoot().getElementName() + ":" + line + ":" + column; //$NON-NLS-1$ //$NON-NLS-2$
            }
            // It was not created from a file
            return line + ":" + column; //$NON-NLS-1$
        }
        return ""; //$NON-NLS-1$
    }

    /**
     * Return the identifiers of variables declared inside the given statement.
     *
     * @param node               The node to visit
     * @param includeInnerScopes True if blocks are visited too.
     *
     * @return The ids of the declared variables.
     */
    public static Set<String> getLocalVariableIdentifiers(final ASTNode node, boolean includeInnerScopes) {
        if (node == null) {
            return Collections.emptySet();
        }
        final VariableDeclarationIdentifierVisitor visitor= new VariableDeclarationIdentifierVisitor(node,
                includeInnerScopes);
        node.accept(visitor);
        return visitor.getVariableNames();
    }

    /**
     * Return true if the node changes nothing.
     *
     * @param node The node to visit.
     *
     * @return True if the node changes nothing.
     */
    public static boolean isPassive(final ASTNode node) {
        final ExprActivityVisitor visitor= new ExprActivityVisitor();
        visitor.visitNode(node);
        return ExprActivity.PASSIVE.equals(visitor.getActivityLevel());
    }

    /**
     * Return true if the statement falls through.
     *
     * @param statement the statement
     * @return true if the statement falls through.
     */
    public static boolean fallsThrough(Statement statement) {
        final List<Statement> statements= asList(statement);
        if (statements.isEmpty()) {
            return false;
        }

        final Statement lastStatement= statements.get(statements.size() - 1);
        switch (lastStatement.getNodeType()) {
        case RETURN_STATEMENT:
        case THROW_STATEMENT:
        case BREAK_STATEMENT:
        case CONTINUE_STATEMENT:
            return true;

        case IF_STATEMENT:
            final IfStatement ifStatement= (IfStatement) lastStatement;
            final Statement thenStatement= ifStatement.getThenStatement();
            final Statement elseStatement= ifStatement.getElseStatement();
            return fallsThrough(thenStatement) && fallsThrough(elseStatement);

        default:
            return false;
        }
    }

    /**
     * Decomposes an initializer into a {@link Pair} with the name of the
     * initialized variable and the initializing expression.
     *
     * @param init the initializer to decompose
     * @return a {@link Pair} with the name of the initialized variable and the
     *         initializing expression, or {@code null} if the initializer could not
     *         be decomposed
     */
    public static Pair<Expression, Expression> decomposeInitializer(Expression init) {
        if (init instanceof VariableDeclarationExpression) {
            final VariableDeclarationExpression vde= (VariableDeclarationExpression) init;
            final List<VariableDeclarationFragment> fragments= fragments(vde);

            if (fragments.size() == 1) {
                final VariableDeclarationFragment fragment= fragments.get(0);
                return Pair.of(fragment.getName(), fragment.getInitializer());
            }
        } else if (init instanceof Assignment) {
            final Assignment as= (Assignment) init;

            if (hasOperator(as, Assignment.Operator.ASSIGN)) {
                final Name name= as(as.getLeftHandSide(), Name.class);
                final FieldAccess fieldAccess= as(as.getLeftHandSide(), FieldAccess.class);

                if (name != null) {
                    return Pair.of(name, as.getRightHandSide());
                }

                if (fieldAccess != null) {
                    return Pair.of(fieldAccess, as.getRightHandSide());
                }
            }
        }

        return Pair.empty();
    }

    /**
     * Returns true if the if statement is in a else statement, that is to say is an else-if statement.
     *
     * @param node The if statement
     * @return true if the if statement is in a else statement
     */
    public static boolean isInElse(final IfStatement node) {
        if (node == null) {
            return false;
        }

        final ASTNode parent= node.getParent();

        if (parent instanceof IfStatement) {
            IfStatement is= (IfStatement) parent;
            return node.equals(is.getElseStatement());
        }

        return false;
    }

    /**
     * Returns true if variables are declared with the same identifier after the given statement.
     *
     * @param node The start
     * @param statementInBlock The statement with variables
     * @return true if variables are declared with the same identifier after the given statement.
     */
    public static boolean hasVariableConflict(Statement node, final Statement statementInBlock) {
        final Set<String> existingVariableNames= getLocalVariableIdentifiers(statementInBlock, false);

        for (Statement statement : getNextSiblings(node)) {
            final VarOccurrenceVisitor varOccurrenceVisitor= new VarOccurrenceVisitor(existingVariableNames);
            varOccurrenceVisitor.visitNode(statement);

            if (varOccurrenceVisitor.isVarUsed()) {
                return true;
            }
        }

        return false;
    }
}

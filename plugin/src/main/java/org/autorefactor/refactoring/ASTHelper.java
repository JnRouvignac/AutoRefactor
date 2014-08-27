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
package org.autorefactor.refactoring;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.autorefactor.util.NotImplementedException;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTMatcher;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.AnnotationTypeDeclaration;
import org.eclipse.jdt.core.dom.AnnotationTypeMemberDeclaration;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.ArrayAccess;
import org.eclipse.jdt.core.dom.ArrayCreation;
import org.eclipse.jdt.core.dom.ArrayInitializer;
import org.eclipse.jdt.core.dom.ArrayType;
import org.eclipse.jdt.core.dom.AssertStatement;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BlockComment;
import org.eclipse.jdt.core.dom.BodyDeclaration;
import org.eclipse.jdt.core.dom.BooleanLiteral;
import org.eclipse.jdt.core.dom.BreakStatement;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.CatchClause;
import org.eclipse.jdt.core.dom.CharacterLiteral;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Comment;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.ConstructorInvocation;
import org.eclipse.jdt.core.dom.ContinueStatement;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EmptyStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.EnumConstantDeclaration;
import org.eclipse.jdt.core.dom.EnumDeclaration;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IExtendedModifier;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.Initializer;
import org.eclipse.jdt.core.dom.InstanceofExpression;
import org.eclipse.jdt.core.dom.Javadoc;
import org.eclipse.jdt.core.dom.LabeledStatement;
import org.eclipse.jdt.core.dom.LineComment;
import org.eclipse.jdt.core.dom.MarkerAnnotation;
import org.eclipse.jdt.core.dom.MemberRef;
import org.eclipse.jdt.core.dom.MemberValuePair;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.MethodRef;
import org.eclipse.jdt.core.dom.MethodRefParameter;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.NormalAnnotation;
import org.eclipse.jdt.core.dom.NullLiteral;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.PackageDeclaration;
import org.eclipse.jdt.core.dom.ParameterizedType;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.QualifiedType;
import org.eclipse.jdt.core.dom.SingleMemberAnnotation;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.jdt.core.dom.SuperFieldAccess;
import org.eclipse.jdt.core.dom.PrimitiveType;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SimpleType;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SuperConstructorInvocation;
import org.eclipse.jdt.core.dom.SuperMethodInvocation;
import org.eclipse.jdt.core.dom.SwitchCase;
import org.eclipse.jdt.core.dom.SwitchStatement;
import org.eclipse.jdt.core.dom.SynchronizedStatement;
import org.eclipse.jdt.core.dom.TagElement;
import org.eclipse.jdt.core.dom.TextElement;
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.jdt.core.dom.ThrowStatement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.TypeDeclarationStatement;
import org.eclipse.jdt.core.dom.TypeLiteral;
import org.eclipse.jdt.core.dom.TypeParameter;
import org.eclipse.jdt.core.dom.UnionType;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.jdt.core.dom.WhileStatement;
import org.eclipse.jdt.core.dom.WildcardType;

/**
 * Helper class for manipulating, converting, navigating and checking {@link ASTNode}s.
 */
public final class ASTHelper {

    /**
     * Boolean constant to use when returning from an {@link ASTVisitor} {{visit(*)}} method
     * to indicate that the visitor does not want to visit a node's subtree. This helps readability.
     */
    public static final boolean DO_NOT_VISIT_SUBTREE = false;
    /**
     * Boolean constant to use when returning from an {@link ASTVisitor} {{visit(*)}} method
     * to indicate that the visitor wants to visit a node's subtree. This helps readability.
     */
    public static final boolean VISIT_SUBTREE = true;

    private ASTHelper() {
        super();
    }

    // AST nodes manipulation

    /**
     * Returns the same node after removing any parentheses around it.
     *
     * @param node the node around which parentheses must be removed
     * @return the same node after removing any parentheses around it.
     *         If there are no parentheses around it then the exact same node is returned
     */
    public static ASTNode removeParentheses(ASTNode node) {
        if (node instanceof Expression) {
            return removeParentheses((Expression) node);
        }
        return node;
    }

    /**
     * Returns the same expression after removing any parentheses around it.
     *
     * @param expr the expression around which parentheses must be removed
     * @return the same expression after removing any parentheses around it
     *         If there are no parentheses around it then the exact same expression is returned
     */
    public static Expression removeParentheses(Expression expr) {
        if (expr instanceof ParenthesizedExpression) {
            return removeParentheses(((ParenthesizedExpression) expr).getExpression());
        }
        return expr;
    }

    /**
     * Parenthesizes the provided expression if its type requires it.
     *
     * @param b the {@link ASTBuilder}
     * @param expr the expression to conditionally parenthesize
     * @return the parenthesized expression or the expression itself
     */
    public static Expression parenthesizeIfNeeded(ASTBuilder b, Expression expr) {
        if (expr instanceof InfixExpression
                || expr instanceof InstanceofExpression) {
            return b.parenthesize(expr);
        }
        return expr;
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
     * @param stmt the statement to analyze
     * @return the provided statement as a non null list of statements
     */
    public static List<Statement> asList(Statement stmt) {
        if (stmt == null) {
            return Collections.emptyList();
        } else if (stmt instanceof Block) {
            return statements((Block) stmt);
        }
        return Arrays.asList(stmt);
    }

    /**
     * Casts the provided statement to an object of the provided type if type matches.
     *
     * @param <T> the required statement type
     * @param stmt the statement to cast
     * @param stmtClazz the class representing the required statement type
     * @return the provided statement as an object of the provided type if type matches, null otherwise
     */
    @SuppressWarnings("unchecked")
    public static <T extends Statement> T as(Statement stmt, Class<T> stmtClazz) {
        if (stmt != null) {
            final List<Statement> stmts = asList(stmt);
            if (stmts.size() == 1
                    && stmtClazz.isAssignableFrom(stmts.get(0).getClass())) {
                return (T) stmts.get(0);
            }
        }
        return null;
    }

    /**
     * Casts the provided expression to an object of the provided type if type matches.
     *
     * @param <T> the required expression type
     * @param expr the expression to cast
     * @param exprClazz the class representing the required expression type
     * @return the provided expression as an object of the provided type if type matches, null otherwise
     */
    @SuppressWarnings("unchecked")
    public static <T extends Expression> T as(Expression expr, Class<T> exprClazz) {
        if (expr != null) {
            if (exprClazz.isAssignableFrom(expr.getClass())) {
                return (T) expr;
            } else if (expr instanceof ParenthesizedExpression) {
                return as(((ParenthesizedExpression) expr).getExpression(), exprClazz);
            }
        }
        return null;
    }

    /**
     * Returns whether the provided expression represents a {@link NullLiteral} ignoring parentheses.
     *
     * @param expr the expression to check
     * @return true if the provided expression represents a {@link NullLiteral} ignoring parentheses, false otherwise
     */
    public static boolean isNullLiteral(Expression expr) {
        return as(expr, NullLiteral.class) != null;
    }

    /**
     * If the provided expression collection only has one element,
     * then that unique expression is cast to an object of the provided type if type matches.
     *
     * @param <T> the required expression type
     * @param exprs the singleton expression to cast
     * @param exprClazz the class representing the required expression type
     * @return the provided singleton expression as an object of the provided type if type matches, null otherwise
     */
    public static <T extends Expression> T as(Collection<? extends Expression> exprs,
            Class<T> exprClazz) {
        if (exprs != null && exprs.size() == 1) {
            return as(exprs.iterator().next(), exprClazz);
        }
        return null;
    }

    /**
     * Returns the {@link Expression} of a specified type out of the provided {@link Statement}.
     * Note the provided statement is first converted to an {@link ExpressionStatement} if possible.
     *
     * @param <T> the required expression type
     * @param stmt the statement
     * @param exprClazz the class representing the required expression type
     * @return the {@link Expression} of a specified type out of an {@link ExpressionStatement}
     */
    public static <T extends Expression> T asExpression(Statement stmt, Class<T> exprClazz) {
        final ExpressionStatement es = as(stmt, ExpressionStatement.class);
        if (es != null) {
            return as(es.getExpression(), exprClazz);
        }
        return null;
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
     * @return a List of expressions
     * @see ParameterizedType#typeArguments()
     */
    @SuppressWarnings("unchecked")
    public static List<Type> typeArguments(ParameterizedType node) {
        return node.typeArguments();
    }

    /**
     * Returns the {@link Boolean} object value represented by the provided expression.
     *
     * @param expr the expression to analyze
     * @return the {@link Boolean} object value if the provided expression represents one, null otherwise
     */
    public static Boolean getBooleanLiteral(Expression expr) {
        final BooleanLiteral bl = as(expr, BooleanLiteral.class);
        if (bl != null) {
            return bl.booleanValue();
        }
        final QualifiedName qn = as(expr, QualifiedName.class);
        if (hasType(qn, "java.lang.Boolean")) {
            return getBooleanObjectAsLiteral(qn);
        }
        return null;
    }

    /**
     * Returns the boolean primitive value of the {@link Boolean} object value
     * represented by the provided qualified name.
     *
     * @param qualifiedName the qualified name that must represent a Boolean object literal
     * @return the boolean primitive value of the {@link Boolean} object value
     *         represented by the provided qualified name.
     */
    public static boolean getBooleanObjectAsLiteral(final QualifiedName qualifiedName) {
        final String fqn = qualifiedName.getFullyQualifiedName();
        if ("Boolean.TRUE".equals(fqn)) {
            return true;
        } else if ("Boolean.FALSE".equals(fqn)) {
            return false;
        }
        throw new NotImplementedException("for fully qualified name \"" + fqn + "\"");
    }

    /**
     * Converts the provided type binding to a new {@link Type} object.
     *
     * @param ast the {@link AST}
     * @param typeBinding the type binding
     * @return a new {@link Type} object
     */
    public static Type toType(final AST ast, final ITypeBinding typeBinding) {
        if (typeBinding == null) {
            return null;
        } else if (typeBinding.isArray()) {
            return ast.newArrayType(toType(ast, typeBinding.getComponentType()));
        } else if (typeBinding.isPrimitive()) {
            if (typeBinding.getName().equals("boolean")) {
                return ast.newPrimitiveType(PrimitiveType.BOOLEAN);
            } else if (typeBinding.getName().equals("byte")) {
                return ast.newPrimitiveType(PrimitiveType.BYTE);
            } else if (typeBinding.getName().equals("char")) {
                return ast.newPrimitiveType(PrimitiveType.CHAR);
            } else if (typeBinding.getName().equals("short")) {
                return ast.newPrimitiveType(PrimitiveType.SHORT);
            } else if (typeBinding.getName().equals("int")) {
                return ast.newPrimitiveType(PrimitiveType.INT);
            } else if (typeBinding.getName().equals("long")) {
                return ast.newPrimitiveType(PrimitiveType.LONG);
            } else if (typeBinding.getName().equals("float")) {
                return ast.newPrimitiveType(PrimitiveType.FLOAT);
            } else if (typeBinding.getName().equals("double")) {
                return ast.newPrimitiveType(PrimitiveType.DOUBLE);
            }
        } else if (typeBinding.isClass() || typeBinding.isInterface()) {
            final String[] qualifiedName = typeBinding.getQualifiedName().split("\\.");
            if (qualifiedName.length == 0) {
                throw new IllegalStateException(
                        "Cannot create a new type from an ITypeBinding without qualified name: " + typeBinding);
            }
            final SimpleType simpleType = ast.newSimpleType(ast.newSimpleName(qualifiedName[0]));
            if (qualifiedName.length == 1) {
                return simpleType;
            }
            Type result = simpleType;
            for (int i = 1; i < qualifiedName.length; i++) {
                result = ast.newQualifiedType(result, ast.newSimpleName(qualifiedName[i]));
            }
            return result;
        }
        throw new NotImplementedException("Unknown type for typeBinding " + typeBinding.getQualifiedName()
                + ", isAnnotation()=" + typeBinding.isAnnotation()
                + ", isAnonymous()=" + typeBinding.isAnonymous()
                + ", isCapture()=" + typeBinding.isCapture()
                + ", isEnum()=" + typeBinding.isEnum()
                + ", isGenericType()=" + typeBinding.isGenericType()
                + ", isParameterizedType()=" + typeBinding.isParameterizedType()
                + ", isTypeVariable()=" + typeBinding.isTypeVariable()
                + ", isRawType()=" + typeBinding.isRawType()
                + ", isWildcardType()=" + typeBinding.isWildcardType());
    }

    // AST navigation

    /**
     * Returns the first ancestor of the provided node which has the required type.
     *
     * @param <T> the required ancestor's type
     * @param node the start node
     * @param ancestorClazz the required ancestor's type
     * @return the first ancestor of the provided node which has the required type
     */
    @SuppressWarnings("unchecked")
    public static <T extends ASTNode> T getAncestor(ASTNode node, Class<T> ancestorClazz) {
        if (node == null || node.getParent() == null) {
            throw new IllegalStateException("Could not find any ancestor for "
                    + ancestorClazz + "and node " + node);
        }
        final ASTNode parent = node.getParent();
        if (ancestorClazz.isAssignableFrom(parent.getClass())) {
            return (T) parent;
        }
        return getAncestor(parent, ancestorClazz);
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
     * Returns the next statement in the same block if it exists.
     *
     * @param startNode the start node
     * @return the next statement in the same block if it exists, null otherwise
     */
    public static Statement getNextSibling(Statement startNode) {
        return getSibling(startNode, false);
    }

    /**
     * Returns the next statement in the source file if it exists.
     *
     * @param startNode the start node
     * @return the next statement in the source file if it exists, null otherwise
     */
    public static Statement getNextStatement(Statement startNode) {
        final Statement nextSibling = getNextSibling(startNode);
        if (nextSibling != null) {
            return nextSibling;
        }
        final ASTNode parent = startNode.getParent();
        if (parent instanceof Statement) {
            return getNextStatement((Statement) parent);
        }
        return null;
    }

    private static Statement getSibling(Statement node, boolean isPrevious) {
        if (node.getParent() instanceof Block) {
            final List<Statement> stmts = asList((Statement) node.getParent());
            final int indexOfNode = stmts.indexOf(node);
            final int siblingIndex = isPrevious ? indexOfNode - 1
                    : indexOfNode + 1;
            if (0 <= siblingIndex && siblingIndex < stmts.size()) {
                return stmts.get(siblingIndex);
            }
        }
        return null;
    }

    // AST checks

    /**
     * Returns whether the provided expression evaluates to exactly one of the provided type.
     *
     * @param expr the expression to analyze
     * @param oneOfQualifiedTypeNames
     *          the type binding qualified name must be equal to one of these qualified type names
     * @return true if the provided expression evaluates to exactly one of the provided type, false otherwise
     */
    public static boolean hasType(Expression expr, String... oneOfQualifiedTypeNames) {
        if (expr == null) {
            return false;
        }
        return hasType(expr.resolveTypeBinding(), oneOfQualifiedTypeNames);
    }

    /**
     * Returns whether the provided type binding is exactly one of the provided type.
     *
     * @param typeBinding the type binding to analyze
     * @param oneOfQualifiedTypeNames
     *          the type binding qualified name must be equal to one of these qualified type names
     * @return true if the provided type binding is exactly one of the provided type, false otherwise
     */
    public static boolean hasType(final ITypeBinding typeBinding, String... oneOfQualifiedTypeNames) {
        if (typeBinding != null) {
            final String qualifiedName = typeBinding.getErasure().getQualifiedName();
            for (String qualifiedTypeName : oneOfQualifiedTypeNames) {
                if (qualifiedTypeName.equals(qualifiedName)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Returns whether the provided expression is an instance of the qualified type name.
     *
     * @param expr the expression to analyze
     * @param qualifiedTypeName the qualified type name
     * @return true if the provided expression is an instance of the qualified type name, false otherwise
     */
    public static boolean instanceOf(Expression expr, String qualifiedTypeName) {
        if (expr == null) {
            return false;
        }
        return instanceOf(expr.resolveTypeBinding(), qualifiedTypeName);
    }

    /**
     * Returns whether the provided type binding is an instance of the qualified type name.
     *
     * @param typeBinding the type binding to analyze
     * @param qualifiedTypeName the qualified type name
     * @return true if the provided type binding is an instance of the qualified type name, false otherwise
     */
    public static boolean instanceOf(ITypeBinding typeBinding, String qualifiedTypeName) {
        return findImplementedType(typeBinding, qualifiedTypeName) != null;
    }

    /**
     * Returns whether the provided expression evaluates to a primitive type.
     *
     * @param expr the expression to analyze
     * @param primitiveTypeName the primitive type name
     * @return true if the provided expression evaluates to a primitive type, false otherwise
     */
    public static boolean isPrimitive(Expression expr, String primitiveTypeName) {
        if (expr == null) {
            return false;
        }
        return isPrimitive(expr.resolveTypeBinding(), primitiveTypeName);
    }

    /**
     * Returns whether the provided type binding represents a primitive type.
     *
     * @param typeBinding the type binding to analyze
     * @param primitiveTypeName the primitive type name
     * @return true if the provided type binding represents a primitive type, false otherwise
     */
    public static boolean isPrimitive(ITypeBinding typeBinding, String primitiveTypeName) {
        return typeBinding != null
                && typeBinding.getQualifiedName().equals(primitiveTypeName);
    }

    private static ITypeBinding findImplementedType(ITypeBinding typeBinding, String qualifiedTypeName) {
        if (typeBinding == null) {
            return null;
        }
        final ITypeBinding typeErasure = typeBinding.getErasure();
        if (qualifiedTypeName.equals(typeBinding.getQualifiedName())
                || qualifiedTypeName.equals(typeErasure.getQualifiedName())) {
            return typeBinding;
        }
        final Set<String> visitedClasses = new HashSet<String>();
        visitedClasses.add(typeErasure.getQualifiedName());
        return findImplementedType(typeBinding, qualifiedTypeName, visitedClasses);
    }

    private static ITypeBinding findImplementedType(ITypeBinding typeBinding,
            String qualifiedTypeName, Set<String> visitedInterfaces) {
        final ITypeBinding superclass = typeBinding.getSuperclass();
        if (superclass != null) {
            final String superClassQualifiedName = superclass.getErasure().getQualifiedName();
            if (qualifiedTypeName.equals(superClassQualifiedName)) {
                return superclass;
            }
            visitedInterfaces.add(superClassQualifiedName);
            final ITypeBinding implementedType =
                    findImplementedType(superclass, qualifiedTypeName, visitedInterfaces);
            if (implementedType != null) {
                return implementedType;
            }
        }
        for (ITypeBinding itfBinding : typeBinding.getInterfaces()) {
            final String itfQualifiedName = itfBinding.getErasure().getQualifiedName();
            if (qualifiedTypeName.equals(itfQualifiedName)) {
                return itfBinding;
            }
            visitedInterfaces.add(itfQualifiedName);
            final ITypeBinding implementedType =
                    findImplementedType(itfBinding, qualifiedTypeName, visitedInterfaces);
            if (implementedType != null) {
                return implementedType;
            }
        }
        return null;
    }

    /**
     * Returns whether the provided node defines a loop.
     *
     * @param node the node
     * @return true if the provided node defines a loop, false otherwise
     */
    public static boolean isLoop(ASTNode node) {
        return node instanceof DoStatement
                || node instanceof EnhancedForStatement
                || node instanceof ForStatement
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
     * Returns whether the provided method invocation invokes a method with the provided method signature.
     * The method signature is compared against the erasure of the invoked method.
     *
     * @param node the method invocation to compare
     * @param typeQualifiedName the qualified name of the type declaring the method
     * @param methodName the method name
     * @param parameterTypesQualifiedNames the qualified names of the parameter types
     * @return true if the provided method invocation matches the provided method signature, false otherwise
     */
    public static boolean isMethod(MethodInvocation node, String typeQualifiedName,
            String methodName, String... parameterTypesQualifiedNames) {
        if (node == null) {
            return false;
        }
        final IMethodBinding methodbinding = node.resolveMethodBinding();
        // let's do the fast checks first
        if (methodbinding == null
                || !methodName.equals(methodbinding.getName())
                || methodbinding.getParameterTypes().length != parameterTypesQualifiedNames.length) {
            return false;
        }
        // ok more heavy checks now
        final ITypeBinding declaringClazz = methodbinding.getDeclaringClass();
        final ITypeBinding implementedType =
                findImplementedType(declaringClazz, typeQualifiedName);
        final boolean isInstanceOf = instanceOf(declaringClazz, typeQualifiedName);
        if (parameterTypesMatch(implementedType, isInstanceOf, methodbinding, parameterTypesQualifiedNames)) {
            return true;
        }
        // a lot more heavy checks
        // FIXME find a more efficient way to do this. It would be awesome
        // if an API to directly find the overridenMethod IMethodBinding existed
        IMethodBinding overridenMethod = findOverridenMethod(declaringClazz, typeQualifiedName,
                methodName, parameterTypesQualifiedNames);
        return overridenMethod != null && methodbinding.overrides(overridenMethod);
    }

    private static boolean parameterTypesMatch(ITypeBinding implementedType,
            boolean isInstanceOf, IMethodBinding methodBinding, String[] parameterTypesQualifiedNames) {
        if (implementedType != null) {
            final ITypeBinding erasure = implementedType.getErasure();
            if (erasure.isGenericType() || erasure.isParameterizedType()) {
                return parameterizedTypesMatch(implementedType, erasure, methodBinding);
            }
        }
        return isInstanceOf && concreteTypesMatch(methodBinding.getParameterTypes(), parameterTypesQualifiedNames);
    }

    private static boolean concreteTypesMatch(ITypeBinding[] typeBindings,
            String... typesQualifiedNames) {
        if (typeBindings.length != typesQualifiedNames.length) {
            return false;
        }
        for (int i = 0; i < typesQualifiedNames.length; i++) {
            if (!typesQualifiedNames[i].equals(typeBindings[i].getQualifiedName())) {
                return false;
            }
        }
        return true;
    }

    private static boolean parameterizedTypesMatch(final ITypeBinding clazz,
            final ITypeBinding clazzErasure, IMethodBinding methodBinding) {
        if (clazz.isParameterizedType() && !clazz.equals(clazzErasure)) {
            final Map<ITypeBinding, ITypeBinding> genericToConcreteTypeParams =
                    getGenericToConcreteTypeParamsMap(clazz, clazzErasure);
            for (IMethodBinding declaredMethod : clazzErasure.getDeclaredMethods()) {
                if (declaredMethod.getName().equals(methodBinding.getName())
                        && parameterizedTypesMatch2(genericToConcreteTypeParams, methodBinding, declaredMethod)) {
                    return true;
                }
            }
        }
        return false;
    }

    private static Map<ITypeBinding, ITypeBinding> getGenericToConcreteTypeParamsMap(
            final ITypeBinding clazz, final ITypeBinding clazzErasure) {
        final ITypeBinding[] typeParams = clazz.getTypeArguments();
        final ITypeBinding[] genericTypeParams = clazzErasure.getTypeParameters();
        final Map<ITypeBinding, ITypeBinding> results = new HashMap<ITypeBinding, ITypeBinding>();
        for (int i = 0; i < typeParams.length; i++) {
            results.put(genericTypeParams[i], typeParams[i]);
        }
        return results;
    }

    private static boolean parameterizedTypesMatch2(
            Map<ITypeBinding, ITypeBinding> genericToConcreteTypeParams,
            IMethodBinding parameterizedMethod, IMethodBinding genericMethod) {
        final ITypeBinding[] paramTypes = parameterizedMethod.getParameterTypes();
        final ITypeBinding[] genericParamTypes = genericMethod.getParameterTypes();
        if (paramTypes.length != genericParamTypes.length) {
            return false;
        }
        for (int i = 0; i < genericParamTypes.length; i++) {
            ITypeBinding genericParamType = genericParamTypes[i];
            ITypeBinding concreteParamType = genericToConcreteTypeParams.get(genericParamType);
            if (concreteParamType == null) {
                concreteParamType = genericParamType;
            }
            final ITypeBinding erasure1 = paramTypes[i].getErasure();
            final ITypeBinding erasure2 = concreteParamType.getErasure();
            if (!erasure1.equals(erasure2)) {
                return false;
            }
        }
        return true;
    }

    private static IMethodBinding findOverridenMethod(ITypeBinding typeBinding, String typeQualifiedName,
            String methodName, String[] parameterTypesQualifiedNames) {
        // superclass
        ITypeBinding superclassBinding = typeBinding.getSuperclass();
        if (superclassBinding != null) {
            superclassBinding = superclassBinding.getErasure();
            if (typeQualifiedName.equals(superclassBinding.getErasure().getQualifiedName())) {
                // found the type
                return findOverridenMethod(methodName, parameterTypesQualifiedNames,
                        superclassBinding.getDeclaredMethods());
            }
            IMethodBinding overridenMethod = findOverridenMethod(superclassBinding, typeQualifiedName,
                    methodName, parameterTypesQualifiedNames);
            if (overridenMethod != null) {
                return overridenMethod;
            }
        }
        // interfaces
        for (ITypeBinding itfBinding : typeBinding.getInterfaces()) {
            itfBinding = itfBinding.getErasure();
            if (typeQualifiedName.equals(itfBinding.getQualifiedName())) {
                // found the type
                return findOverridenMethod(methodName, parameterTypesQualifiedNames,
                        itfBinding.getDeclaredMethods());
            }
            IMethodBinding overridenMethod = findOverridenMethod(itfBinding, typeQualifiedName,
                methodName, parameterTypesQualifiedNames);
            if (overridenMethod != null) {
                return overridenMethod;
            }
        }
        return null;
    }

    private static IMethodBinding findOverridenMethod(String methodName, String[] parameterTypesQualifiedNames,
            IMethodBinding[] declaredMethods) {
        for (IMethodBinding methodBinding : declaredMethods) {
            final IMethodBinding methodDecl = methodBinding.getMethodDeclaration();
            if (methodBinding.getName().equals(methodName)
                    && methodDecl != null
                    && concreteTypesMatch(methodDecl.getParameterTypes(), parameterTypesQualifiedNames)) {
                return methodBinding;
            }
        }
        return null;
    }

    /**
     * Infers what type the parent node expects to be returned by the passed in
     * Expression.
     *
     * @param node
     *        the expression for which to look at the type expected by the
     *        context
     * @return the type expected by the context of the current node
     */
    public static ITypeBinding resolveTypeBindingForcedFromContext(
            Expression node) {
        final ASTNode parent = node.getParent();
        if (parent instanceof InfixExpression) {
            final InfixExpression ie = (InfixExpression) parent;
            return ie.resolveTypeBinding();
        }
        return null;
    }

    /**
     * Returns whether <code>this</code> is referring to the currently surrounding type.
     *
     * @param thisExpression the <code>this</code> expression
     * @return true if <code>this</code> is referring to the currently surrounding type, false otherwise
     */
    public static boolean thisExpressionRefersToSurroundingType(ThisExpression thisExpression) {
        if (thisExpression == null) {
            return false;
        }
        return thisExpressionRefersToSurroundingType(thisExpression.getQualifier(), thisExpression);
    }

    private static boolean thisExpressionRefersToSurroundingType(Name thisQualifierName, ASTNode node) {
        final TypeDeclaration ancestor = getAncestor(node, TypeDeclaration.class);
        if (thisQualifierName == null) {
            return true;
        } else if (thisQualifierName instanceof SimpleName) {
            return isEqual((SimpleName) thisQualifierName, ancestor.getName());
        } else if (thisQualifierName instanceof QualifiedName) {
            final QualifiedName qn = (QualifiedName) thisQualifierName;
            return isEqual(qn.getName(), ancestor.getName())
                    && thisExpressionRefersToSurroundingType(qn.getQualifier(), ancestor);
        }
        throw new NotImplementedException(thisQualifierName);
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
        } else if (name1 instanceof QualifiedName
                && name2 instanceof QualifiedName) {
            return isEqual((QualifiedName) name1, (QualifiedName) name2);
        }
        return false;
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
        if (isEqual(name1.getName(), name2.getName())) {
            return isEqual(name1.getQualifier(), name2.getQualifier());
        }
        return false;
    }

    private static boolean sameClass(ASTNode node1, ASTNode node2) {
        return node1 != null && node2 != null
                && node1.getClass().equals(node2.getClass());
    }

    /**
     * Returns whether the two provided nodes structurally match.
     *
     * @param matcher the AST matcher
     * @param node1 the first node to compare
     * @param node2 the second node to compare
     * @return true if the two provided nodes structurally match, false otherwise
     */
    public static boolean match(ASTMatcher matcher, ASTNode node1, ASTNode node2) {
        if (sameClass(node1, node2)) {
            // FIXME JNR implement all expressions
            // TODO JNR
            // can we match "this.ast" and the unqualified "ast" for example?
            // can we match "MyClass.CONSTANT" and the unqualified "CONSTANT" for example?
            // can we use IVariableBindings to compare them?
            switch (node1.getNodeType()) {
            case ASTNode.ANNOTATION_TYPE_DECLARATION:
                return matcher.match((AnnotationTypeDeclaration) node1, node2);
            case ASTNode.ANNOTATION_TYPE_MEMBER_DECLARATION:
                return matcher.match((AnnotationTypeMemberDeclaration) node1, node2);
            case ASTNode.ANONYMOUS_CLASS_DECLARATION:
                return matcher.match((AnonymousClassDeclaration) node1, node2);
            case ASTNode.ARRAY_ACCESS:
                return matcher.match((ArrayAccess) node1, node2);
            case ASTNode.ARRAY_CREATION:
                return matcher.match((ArrayCreation) node1, node2);
            case ASTNode.ARRAY_INITIALIZER:
                return matcher.match((ArrayInitializer) node1, node2);
            case ASTNode.ARRAY_TYPE:
                return matcher.match((ArrayType) node1, node2);
            case ASTNode.ASSERT_STATEMENT:
                return matcher.match((AssertStatement) node1, node2);
            case ASTNode.ASSIGNMENT:
                return matcher.match((Assignment) node1, node2);
            case ASTNode.BLOCK:
                return matcher.match((Block) node1, node2);
            case ASTNode.BLOCK_COMMENT:
                return matcher.match((BlockComment) node1, node2);
            case ASTNode.BOOLEAN_LITERAL:
                return matcher.match((BooleanLiteral) node1, node2);
            case ASTNode.BREAK_STATEMENT:
                return matcher.match((BreakStatement) node1, node2);
            case ASTNode.CAST_EXPRESSION:
                return matcher.match((CastExpression) node1, node2);
            case ASTNode.CATCH_CLAUSE:
                return matcher.match((CatchClause) node1, node2);
            case ASTNode.CHARACTER_LITERAL:
                return matcher.match((CharacterLiteral) node1, node2);
            case ASTNode.CLASS_INSTANCE_CREATION:
                return matcher.match((ClassInstanceCreation) node1, node2);
            case ASTNode.COMPILATION_UNIT:
                return matcher.match((CompilationUnit) node1, node2);
            case ASTNode.CONDITIONAL_EXPRESSION:
                return matcher.match((ConditionalExpression) node1, node2);
            case ASTNode.CONSTRUCTOR_INVOCATION:
                return matcher.match((ConstructorInvocation) node1, node2);
            case ASTNode.CONTINUE_STATEMENT:
                return matcher.match((ContinueStatement) node1, node2);
            case ASTNode.DO_STATEMENT:
                return matcher.match((DoStatement) node1, node2);
            case ASTNode.EMPTY_STATEMENT:
                return matcher.match((EmptyStatement) node1, node2);
            case ASTNode.ENHANCED_FOR_STATEMENT:
                return matcher.match((EnhancedForStatement) node1, node2);
            case ASTNode.ENUM_DECLARATION:
                return matcher.match((EnumDeclaration) node1, node2);
            case ASTNode.ENUM_CONSTANT_DECLARATION:
                return matcher.match((EnumConstantDeclaration) node1, node2);
            case ASTNode.EXPRESSION_STATEMENT:
                return matcher.match((ExpressionStatement) node1, node2);
            case ASTNode.FIELD_ACCESS:
                return matcher.match((FieldAccess) node1, node2);
            case ASTNode.FIELD_DECLARATION:
                return matcher.match((FieldDeclaration) node1, node2);
            case ASTNode.FOR_STATEMENT:
                return matcher.match((ForStatement) node1, node2);
            case ASTNode.IF_STATEMENT:
                return matcher.match((IfStatement) node1, node2);
            case ASTNode.IMPORT_DECLARATION:
                return matcher.match((ImportDeclaration) node1, node2);
            case ASTNode.INFIX_EXPRESSION:
                return matcher.match((InfixExpression) node1, node2);
            case ASTNode.INITIALIZER:
                return matcher.match((Initializer) node1, node2);
            case ASTNode.INSTANCEOF_EXPRESSION:
                return matcher.match((InstanceofExpression) node1, node2);
            case ASTNode.JAVADOC:
                return matcher.match((Javadoc) node1, node2);
            case ASTNode.LABELED_STATEMENT:
                return matcher.match((LabeledStatement) node1, node2);
            case ASTNode.LINE_COMMENT:
                return matcher.match((LineComment) node1, node2);
            case ASTNode.MARKER_ANNOTATION:
                return matcher.match((MarkerAnnotation) node1, node2);
            case ASTNode.MEMBER_REF:
                return matcher.match((MemberRef) node1, node2);
            case ASTNode.MEMBER_VALUE_PAIR:
                return matcher.match((MemberValuePair) node1, node2);
            case ASTNode.METHOD_DECLARATION:
                return matcher.match((MethodDeclaration) node1, node2);
            case ASTNode.METHOD_INVOCATION:
                return matcher.match((MethodInvocation) node1, node2);
            case ASTNode.METHOD_REF:
                return matcher.match((MethodRef) node1, node2);
            case ASTNode.METHOD_REF_PARAMETER:
                return matcher.match((MethodRefParameter) node1, node2);
            case ASTNode.MODIFIER:
                return matcher.match((Modifier) node1, node2);
            case ASTNode.NORMAL_ANNOTATION:
                return matcher.match((NormalAnnotation) node1, node2);
            case ASTNode.NULL_LITERAL:
                return matcher.match((NullLiteral) node1, node2);
            case ASTNode.NUMBER_LITERAL:
                return matcher.match((NumberLiteral) node1, node2);
            case ASTNode.PACKAGE_DECLARATION:
                return matcher.match((PackageDeclaration) node1, node2);
            case ASTNode.PARAMETERIZED_TYPE:
                return matcher.match((ParameterizedType) node1, node2);
            case ASTNode.PARENTHESIZED_EXPRESSION:
                return matcher.match((ParenthesizedExpression) node1, node2);
            case ASTNode.POSTFIX_EXPRESSION:
                return matcher.match((PostfixExpression) node1, node2);
            case ASTNode.PREFIX_EXPRESSION:
                return matcher.match((PrefixExpression) node1, node2);
            case ASTNode.PRIMITIVE_TYPE:
                return matcher.match((PrimitiveType) node1, node2);
            case ASTNode.QUALIFIED_NAME:
                return matcher.match((QualifiedName) node1, node2);
            case ASTNode.QUALIFIED_TYPE:
                return matcher.match((QualifiedType) node1, node2);
            case ASTNode.RETURN_STATEMENT:
                return matcher.match((ReturnStatement) node1, node2);
            case ASTNode.SIMPLE_NAME:
                return matcher.match((SimpleName) node1, node2);
            case ASTNode.SIMPLE_TYPE:
                return matcher.match((SimpleType) node1, node2);
            case ASTNode.SINGLE_MEMBER_ANNOTATION:
                return matcher.match((SingleMemberAnnotation) node1, node2);
            case ASTNode.SINGLE_VARIABLE_DECLARATION:
                return matcher.match((SingleVariableDeclaration) node1, node2);
            case ASTNode.STRING_LITERAL:
                return matcher.match((StringLiteral) node1, node2);
            case ASTNode.SUPER_CONSTRUCTOR_INVOCATION:
                return matcher.match((SuperConstructorInvocation) node1, node2);
            case ASTNode.SUPER_FIELD_ACCESS:
                return matcher.match((SuperFieldAccess) node1, node2);
            case ASTNode.SUPER_METHOD_INVOCATION:
                return matcher.match((SuperMethodInvocation) node1, node2);
            case ASTNode.SWITCH_CASE:
                return matcher.match((SwitchCase) node1, node2);
            case ASTNode.SWITCH_STATEMENT:
                return matcher.match((SwitchStatement) node1, node2);
            case ASTNode.SYNCHRONIZED_STATEMENT:
                return matcher.match((SynchronizedStatement) node1, node2);
            case ASTNode.TAG_ELEMENT:
                return matcher.match((TagElement) node1, node2);
            case ASTNode.TEXT_ELEMENT:
                return matcher.match((TextElement) node1, node2);
            case ASTNode.THIS_EXPRESSION:
                return matcher.match((ThisExpression) node1, node2);
            case ASTNode.THROW_STATEMENT:
                return matcher.match((ThrowStatement) node1, node2);
            case ASTNode.TRY_STATEMENT:
                return matcher.match((TryStatement) node1, node2);
            case ASTNode.TYPE_DECLARATION:
                return matcher.match((TypeDeclaration) node1, node2);
            case ASTNode.TYPE_DECLARATION_STATEMENT:
                return matcher.match((TypeDeclarationStatement) node1, node2);
            case ASTNode.TYPE_LITERAL:
                return matcher.match((TypeLiteral) node1, node2);
            case ASTNode.TYPE_PARAMETER:
                return matcher.match((TypeParameter) node1, node2);
            case ASTNode.UNION_TYPE:
                return matcher.match((UnionType) node1, node2);
            case ASTNode.VARIABLE_DECLARATION_EXPRESSION:
                return matcher.match((VariableDeclarationExpression) node1, node2);
            case ASTNode.VARIABLE_DECLARATION_FRAGMENT:
                return matcher.match((VariableDeclarationFragment) node1, node2);
            case ASTNode.VARIABLE_DECLARATION_STATEMENT:
                return matcher.match((VariableDeclarationStatement) node1, node2);
            case ASTNode.WHILE_STATEMENT:
                return matcher.match((WhileStatement) node1, node2);
            case ASTNode.WILDCARD_TYPE:
                return matcher.match((WildcardType) node1, node2);
            default:
                throw new NotImplementedException(node1);
            }
        }
        return false;
    }

    /**
     * Returns whether the two provided simple names represent the same variable.
     *
     * @param name1 the first simple name to compare
     * @param name2 the second simple name to compare
     * @return true if the two provided simple names represent the same variable, false otherwise
     */
    public static boolean isSameVariable(SimpleName name1, SimpleName name2) {
        final IBinding bnd1 = name1.resolveBinding();
        final IBinding bnd2 = name2.resolveBinding();
        if (bnd1 == null || bnd2 == null) {
            return false;
        }
        return bnd1.isEqualTo(bnd2);
    }

    /**
     * Returns whether the two provided names represent the same variable.
     *
     * @param name1 the first name to compare
     * @param name2 the second name to compare
     * @return true if the two provided names represent the same variable, false otherwise
     */
    public static boolean isSameVariable(SimpleName name1, QualifiedName name2) {
        return false;
    }

    /**
     * Returns whether the two provided expressions represent the same variable.
     *
     * @param name1 the first expression to compare
     * @param field2 the second expression to compare
     * @return true if the two provided expressions represent the same variable, false otherwise
     */
    public static boolean isSameVariable(SimpleName name1, FieldAccess field2) {
        if (!(field2.getExpression() instanceof ThisExpression)) {
            // TODO JNR parenthesized expr??
            return false;
        }
        IBinding cb = name1.resolveBinding();
        IBinding sb = field2.resolveFieldBinding();
        if (cb == null || sb == null) {
            return false;
        }
        return cb.isEqualTo(sb);
    }

    /**
     * Returns whether the two provided qualified names represent the same variable.
     *
     * @param name1 the first qualified name to compare
     * @param name2 the second qualified name to compare
     * @return true if the two provided qualified names represent the same variable, false otherwise
     */
    public static boolean isSameVariable(QualifiedName name1, QualifiedName name2) {
        IBinding cb = name1.resolveBinding();
        IBinding sb = name2.resolveBinding();
        if (cb == null || sb == null) {
            return false;
        }
        if (cb.isEqualTo(sb)) {
            return isSameVariable(name1.getQualifier(), name2.getQualifier());
        }
        return false;
    }

    /**
     * Returns whether the two provided expressions represent the same variable.
     *
     * @param name1 the first expression to compare
     * @param field2 the second expression to compare
     * @return true if the two provided expressions represent the same variable, false otherwise
     */
    public static boolean isSameVariable(QualifiedName name1, FieldAccess field2) {
        IBinding sb = name1.resolveBinding();
        IBinding cb = field2.resolveFieldBinding();
        if (cb == null || sb == null) {
            return false;
        }
        if (cb.isEqualTo(sb)) {
            return isSameVariable(field2.getExpression(), name1.getQualifier());
        }
        return false;
    }

    /**
     * Returns whether the two provided field accesses represent the same variable.
     *
     * @param field1 the first field access to compare
     * @param field2 the second field access to compare
     * @return true if the two provided field accesses represent the same variable, false otherwise
     */
    public static boolean isSameVariable(FieldAccess field1, FieldAccess field2) {
        IBinding cb = field1.resolveFieldBinding();
        IBinding sb = field2.resolveFieldBinding();
        if (cb == null || sb == null) {
            return false;
        }
        if (cb.isEqualTo(sb)) {
            return isSameVariable(field1.getExpression(), field2.getExpression());
        }
        return false;
    }

    /**
     * Returns whether the two provided expressions represent the same variable.
     *
     * @param expr1 the first expression to compare
     * @param expr2 the second expression to compare
     * @return true if the two provided expressions represent the same variable, false otherwise
     */
    public static boolean isSameVariable(Expression expr1, Expression expr2) {
        if (expr1 instanceof ThisExpression) {
            // TODO JNR
            // ThisExpression te = (ThisExpression) name1;
            // te.getQualifier();
            return expr2 instanceof ThisExpression;
        } else if (expr1 instanceof SimpleName) {
            final SimpleName sn = (SimpleName) expr1;
            if (expr2 instanceof SimpleName) {
                return isSameVariable(sn, (SimpleName) expr2);
            } else if (expr2 instanceof QualifiedName) {
                return isSameVariable(sn, (QualifiedName) expr2);
            } else if (expr2 instanceof FieldAccess) {
                return isSameVariable(sn, (FieldAccess) expr2);
            }
        } else if (expr1 instanceof QualifiedName) {
            final QualifiedName qn = (QualifiedName) expr1;
            if (expr2 instanceof SimpleName) {
                return isSameVariable((SimpleName) expr2, qn);
            } else if (expr2 instanceof QualifiedName) {
                return isSameVariable(qn, (QualifiedName) expr2);
            } else if (expr2 instanceof FieldAccess) {
                return isSameVariable(qn, (FieldAccess) expr2);
            }
        } else if (expr1 instanceof FieldAccess) {
            final FieldAccess fa = (FieldAccess) expr1;
            if (expr2 instanceof SimpleName) {
                return isSameVariable((SimpleName) expr2, fa);
            } else if (expr2 instanceof QualifiedName) {
                return isSameVariable((QualifiedName) expr2, fa);
            } else if (expr2 instanceof FieldAccess) {
                return isSameVariable(fa, (FieldAccess) expr2);
            }
        }
        return false;
    }

    /**
     * Returns the parent node filtering out ParenthesizedExpression and Blocks.
     *
     * @param node the node
     * @return the parent node filtering out ParenthesizedExpression and Blocks
     */
    public static ASTNode getParent(ASTNode node) {
        ASTNode parent = node.getParent();
        if (parent instanceof ParenthesizedExpression) {
            return getParent(((ParenthesizedExpression) parent).getParent());
        } else if (parent instanceof Block) {
            return getParent(((Block) parent).getParent());
        }
        return parent;
    }

    /**
     * Returns the file name where the node comes from, or "FakeClass.java" if this is a fake node.
     *
     * @param node the node
     * @return the file name where the node comes from, or "FakeClass.java" if this is a fake node.
     */
    public static String getFileName(ASTNode node) {
        if (node.getRoot() instanceof CompilationUnit) {
            CompilationUnit cu = (CompilationUnit) node.getRoot();
            if (cu.getTypeRoot() != null) { // added for unit tests
                return cu.getTypeRoot().getElementName();
            }
            return "FakeClass.java";
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
        final ASTNode root = node.getRoot();
        if (root instanceof CompilationUnit) {
            final CompilationUnit cu = (CompilationUnit) root;
            final int position = node.getStartPosition();
            if (cu.getTypeRoot() != null) {
                return cu.getTypeRoot().getElementName()
                    + ":" + cu.getLineNumber(position) + ":" + cu.getColumnNumber(position);
            }
            // it was not created from a file
            return cu.getLineNumber(position) + ":" + cu.getColumnNumber(position);
        }
        return "";
    }
}

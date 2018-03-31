/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Zsombor Gegesy - various additions
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

import static org.autorefactor.refactoring.ASTHelper.arguments;
import static org.autorefactor.refactoring.ASTHelper.catchClauses;
import static org.autorefactor.refactoring.ASTHelper.extendedOperands;
import static org.autorefactor.refactoring.ASTHelper.hasOperator;
import static org.autorefactor.refactoring.ASTHelper.modifiers;
import static org.autorefactor.refactoring.ASTHelper.removeParentheses;
import static org.autorefactor.refactoring.ASTHelper.statements;
import static org.autorefactor.refactoring.ASTHelper.typeArguments;
import static org.autorefactor.util.Utils.equal;
import static org.eclipse.jdt.core.dom.ASTNode.ARRAY_TYPE;
import static org.eclipse.jdt.core.dom.ASTNode.ASSIGNMENT;
import static org.eclipse.jdt.core.dom.ASTNode.CONDITIONAL_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.EMPTY_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.INFIX_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.INSTANCEOF_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.PREFIX_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.PRIMITIVE_TYPE;
import static org.eclipse.jdt.core.dom.ASTNode.QUALIFIED_TYPE;
import static org.eclipse.jdt.core.dom.ASTNode.SIMPLE_TYPE;
import static org.eclipse.jdt.core.dom.Modifier.ModifierKeyword.FINAL_KEYWORD;
import static org.eclipse.jdt.core.dom.Modifier.ModifierKeyword.PRIVATE_KEYWORD;
import static org.eclipse.jdt.core.dom.Modifier.ModifierKeyword.PROTECTED_KEYWORD;
import static org.eclipse.jdt.core.dom.Modifier.ModifierKeyword.PUBLIC_KEYWORD;
import static org.eclipse.jdt.core.dom.Modifier.ModifierKeyword.STATIC_KEYWORD;
import static org.eclipse.jdt.core.dom.PrefixExpression.Operator.NOT;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import org.autorefactor.util.IllegalArgumentException;
import org.autorefactor.util.NotImplementedException;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Annotation;
import org.eclipse.jdt.core.dom.ArrayCreation;
import org.eclipse.jdt.core.dom.ArrayInitializer;
import org.eclipse.jdt.core.dom.ArrayType;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BooleanLiteral;
import org.eclipse.jdt.core.dom.BreakStatement;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.CatchClause;
import org.eclipse.jdt.core.dom.CharacterLiteral;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.CreationReference;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionMethodReference;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.IExtendedModifier;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.LambdaExpression;
import org.eclipse.jdt.core.dom.MarkerAnnotation;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.NullLiteral;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.ParameterizedType;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.PrimitiveType;
import org.eclipse.jdt.core.dom.PrimitiveType.Code;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SimpleType;
import org.eclipse.jdt.core.dom.SingleMemberAnnotation;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.jdt.core.dom.StructuralPropertyDescriptor;
import org.eclipse.jdt.core.dom.SuperMethodInvocation;
import org.eclipse.jdt.core.dom.SuperMethodReference;
import org.eclipse.jdt.core.dom.SwitchCase;
import org.eclipse.jdt.core.dom.SwitchStatement;
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.jdt.core.dom.ThrowStatement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.TypeMethodReference;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.jdt.core.dom.WildcardType;

/**
 * Helper class for building AST note in a somewhat fluent API.
 * Method names which are also java keywords are postfixed with a "0".
 */
public class ASTBuilder {
    /** Copy operations to be performed deeply into {@link ASTBuilder} methods. */
    public enum Copy {
        /** Do not perform any copy. Returns the node as is. */
        NONE {
            @Override
            protected <T extends ASTNode> T perform(ASTBuilder b, T node) {
                return node;
            }
        },
        /** Delegates to {@link ASTBuilder#copy(ASTNode)}. */
        COPY {
            @Override
            protected <T extends ASTNode> T perform(ASTBuilder b, T node) {
                return b.copy(node);
            }
        },
        /** Delegates to {@link ASTBuilder#move(ASTNode)}. */
        MOVE {
            @Override
            protected <T extends ASTNode> T perform(ASTBuilder b, T node) {
                return b.move(node);
            }
        };

        /**
         * Performs the copy operation on the provided node  with the provided {@link ASTBuilder}.
         *
         * @param b the {@link ASTBuilder} allowing to copy the provided node
         * @param node the node on which to perform the copy operation
         * @param <T> the node type
         * @return the copied node
         */
        protected abstract <T extends ASTNode> T perform(ASTBuilder b, T node);
    }

    private final AST ast;
    private final Refactorings refactorings;

    /**
     * Class constructor.
     *
     * @param refactorings the refactorings
     */
    public ASTBuilder(final Refactorings refactorings) {
        this.refactorings = refactorings;
        this.ast = refactorings.getAST();
    }

    /**
     * Returns the {@link AST}.
     *
     * @return the {@link AST}
     */
    public AST getAST() {
        return ast;
    }

    /**
     * Builds a new {@link Annotation} instance.
     *
     * @param typeName the annotation type name
     * @return a new annotation
     */
    public Annotation annotation(String typeName) {
        // TODO handle SingleMemberAnnotation and NormalAnnotation
        return markerAnnotation(simpleName(typeName));
    }

    /**
     * Helper to create a list of parameters.
     *
     * @param variableDeclarations the list of parameters
     * @return a new list of parameters
     */
    public List<SingleVariableDeclaration> parameters(SingleVariableDeclaration... variableDeclarations) {
        return Arrays.asList(variableDeclarations);
    }

    /**
     * Builds a new {@link Assignment} instance.
     *
     * @param lhs the left hand side expression
     * @param operator the assignment operator
     * @param rhs the right hand side expression
     * @return a new Block
     */
    public Assignment assign(final Expression lhs, final Assignment.Operator operator, final Expression rhs) {
        final Assignment assign = ast.newAssignment();
        assign.setLeftHandSide(lhs);
        assign.setOperator(operator);
        assign.setRightHandSide(rhs);
        return assign;
    }

    /**
     * Builds a new {@link Block} instance.
     *
     * @param stmts the statements to add to the block
     * @return a new Block
     */
    public Block block(final Statement... stmts) {
        final Block block = ast.newBlock();
        addAll(statements(block), stmts);
        return block;
    }

    /**
     * Builds a new {@link BooleanLiteral} instance.
     *
     * @param boolValue the boolean literal value
     * @return a new boolean literal
     */
    public BooleanLiteral boolean0(boolean boolValue) {
        return ast.newBooleanLiteral(boolValue);
    }

    /**
     * Builds a new {@link BreakStatement} instance.
     *
     * @return a new break statement
     */
    public BreakStatement break0() {
        return ast.newBreakStatement();
    }

    /**
     * Builds a new {@link SwitchCase} instance.
     *
     * @param expr
     *            the case expression
     * @return a new switch case statement
     */
    public SwitchCase case0(Expression expr) {
        final SwitchCase sc = ast.newSwitchCase();
        sc.setExpression(expr);
        return sc;
    }

    /**
     * Builds a new {@link CastExpression} instance.
     *
     * @param type the type being cast to
     * @param expr the expression being cast
     * @return a new CastExpression
     */
    public CastExpression cast(Type type, Expression expr) {
        final CastExpression ce = ast.newCastExpression();
        ce.setType(type);
        ce.setExpression(parenthesizeIfNeeded(expr));
        return ce;
    }

    /**
     * Builds a new {@link SwitchCase} instance which represents a {@code default} statement.
     *
     * @return a new switch case statement representing a {@code default} statement
     */
    public SwitchCase default0() {
        return case0(null);
    }

    /**
     * Returns a type for the provided type name (simple or qualified name).
     *
     * @param typeName the type name (simple or qualified name)
     * @return a type for the provided type name
     */
    public Type type(String typeName) {
        if (typeName.indexOf('.') == -1) {
            return simpleType(typeName);
        }
        return qualifiedType(typeName.split("\\."));
    }

    private Type simpleType(final String name) {
        final Code primitiveTypeCode = PrimitiveType.toCode(name);
        if (primitiveTypeCode != null) {
            return ast.newPrimitiveType(primitiveTypeCode);
        }
        return ast.newSimpleType(ast.newSimpleName(name));
    }

    private Type qualifiedType(String... names) {
        switch (names.length) {
        case 0:
            throw new IllegalArgumentException(null, "Expected one or more names, but got 0");
        case 1:
            return simpleType(names[0]);
        default:
            Type type = ast.newSimpleType(ast.newSimpleName(names[0]));
            for (int i = 1; i < names.length; i++) {
                type = ast.newQualifiedType(type, ast.newSimpleName(names[i]));
            }
            return type;
        }
    }

    /**
     * Returns a parameterized type with the provided type name and type arguments.
     *
     * @param typeName the type name (simple or qualified name)
     * @param typeArguments the type arguments
     * @return a new parameterized type
     */
    public Type genericType(String typeName, Type... typeArguments) {
        final Type type = type(typeName);
        if (typeArguments.length == 0) {
            return type;
        }

        final ParameterizedType parameterizedType = ast.newParameterizedType(type);
        Collections.addAll(typeArguments(parameterizedType), typeArguments);
        return parameterizedType;
    }

    /**
     * Builds a new {@link CatchClause} instance.
     *
     * @param exceptionTypeName the exception type name
     * @param caughtExceptionName the local name for the caught exception
     * @param stmts the statements to add to the catch clause
     * @return a new catch clause
     */
    public CatchClause catch0(String exceptionTypeName, String caughtExceptionName, Statement... stmts) {
        final CatchClause cc = ast.newCatchClause();
        final SingleVariableDeclaration svd = ast.newSingleVariableDeclaration();
        svd.setType(simpleType(exceptionTypeName));
        svd.setName(ast.newSimpleName(caughtExceptionName));
        cc.setException(svd);

        final Block block = ast.newBlock();
        addAll(statements(block), stmts);
        cc.setBody(block);
        return cc;
    }

    /**
     * Returns a copy of the provided {@link ASTNode}.
     *
     * @param <T> the actual node type
     * @param nodeToCopy the node to copy
     * @return a copy of the node
     */
    public <T extends ASTNode> T copy(T nodeToCopy) {
        if (nodeToCopy.getNodeType() == ARRAY_TYPE) {
            return (T) copyType((Type) nodeToCopy);
        } else if (isValidInCurrentAST(nodeToCopy)) {
            return refactorings.createCopyTarget(nodeToCopy);
        }
        return copySubtree(nodeToCopy);
    }

    private boolean isValidInCurrentAST(ASTNode node) {
        return node.getAST() == ast
                && node.getStartPosition() != -1;
    }

    /**
     * Creates a type by copying the type binding of the provided expression.
     *
     * @param expr the expression whose type must be copied
     * @param typeNameDecider decides on how the type should be referenced (simple name or qualified name)
     * @return a new type
     */
    public Type copyType(Expression expr, TypeNameDecider typeNameDecider) {
        return toType(expr.resolveTypeBinding(), typeNameDecider);
    }

    /**
     * Converts a type binding into a type.
     *
     * @param typeBinding the type binding to convert
     * @param typeNameDecider decides on how the type should be referenced (simple name or qualified name)
     * @return a new type
     */
    public Type toType(ITypeBinding typeBinding, TypeNameDecider typeNameDecider) {
        if (typeBinding == null) {
            throw new IllegalArgumentException(null, "typeBinding cannot be null");
        }

        if (typeBinding.isParameterizedType()) {
            final ParameterizedType type = ast.newParameterizedType(toType(typeBinding.getErasure(), typeNameDecider));
            final List<Type> typeArgs = typeArguments(type);
            for (ITypeBinding typeArg : typeBinding.getTypeArguments()) {
                typeArgs.add(toType(typeArg, typeNameDecider));
            }
            return type;
        } else if (typeBinding.isPrimitive()) {
            return type(typeBinding.getName());
        } else if (typeBinding.isClass()
                || typeBinding.isInterface()
                || typeBinding.isEnum()
                || typeBinding.isAnnotation()
                || typeBinding.isNullType()
                || typeBinding.isRawType()) {
            return type(typeNameDecider.useSimplestPossibleName(typeBinding));
        } else  if (typeBinding.isArray()) {
            return ast.newArrayType(toType(typeBinding.getElementType(), typeNameDecider));
        } else if (typeBinding.isWildcardType()) {
            final WildcardType type = ast.newWildcardType();
            if (typeBinding.getBound() != null) {
                type.setBound(toType(typeBinding.getBound(), typeNameDecider), typeBinding.isUpperbound());
            }
            return type;
        } else if (typeBinding.isTypeVariable()) {
            return type(typeBinding.getName());
        } else if (typeBinding.isCapture()) {
            if (typeBinding.getTypeBounds().length > 1) {
                throw new NotImplementedException(null,
                    "because it violates the javadoc of `ITypeBinding.getTypeBounds()`: "
                    + "\"Note that per construction, it can only contain one class or array type, "
                    + "at most, and then it is located in first position.\"");
            }
            return toType(typeBinding.getWildcard(), typeNameDecider);
        }
        throw new NotImplementedException(null, " for the type binding '" + typeBinding + "'");
    }

    private Type copyType(final Type type) {
        switch (type.getNodeType()) {
        case ARRAY_TYPE:
            final ArrayType arrayType = (ArrayType) type;
            return ast.newArrayType(
                    copyType(arrayType.getElementType()),
                    arrayType.getDimensions());

        case PRIMITIVE_TYPE:
            final Code code = ((PrimitiveType) type).getPrimitiveTypeCode();
            return ast.newPrimitiveType(code);

        case QUALIFIED_TYPE:
            return type(type.resolveBinding().getQualifiedName());

        case SIMPLE_TYPE:
            final SimpleType sType = (SimpleType) type;
            return ast.newSimpleType(copy(sType.getName()));
        }

        throw new NotImplementedException(null, "Unknown type for type " + type);
    }

    /**
     * Returns a copy of the expression of the provided {@link MethodInvocation} or null if no such expression exists.
     *
     * @param node the {@link MethodInvocation} for which to copy the expression
     * @return a copy of the expression, or false if no such expression exists
     */
    public Expression copyExpression(MethodInvocation node) {
        return node.getExpression() != null ? copy(node.getExpression()) : null;
    }

    /**
     * Returns a copy of the provided nodes list.
     *
     * @param <T> the actual nodes's type
     * @param nodes the nodes list to copy
     * @return a single node, representing a copy of the nodes list
     */
    public <T extends ASTNode> T copyRange(List<T> nodes) {
        if (nodes.isEmpty()) {
            return null;
        }
        if (!isValidForRangeOperation(nodes)) {
            throw new IllegalArgumentException(nodes.get(0),
                    "The provided nodes are not valid for doing a range copy: " + nodes);
        }
        return refactorings.createCopyTarget(nodes.get(0), nodes.get(nodes.size() - 1));
    }

    /**
     * Returns a move for the provided nodes list.
     *
     * @param <T> the actual nodes's type
     * @param nodes the nodes list to move
     * @return a single node, representing a move of the nodes list
     */
    public <T extends ASTNode> T moveRange(List<T> nodes) {
        if (nodes.isEmpty()) {
            return null;
        }
        if (!isValidForRangeOperation(nodes)) {
            throw new IllegalArgumentException(nodes.get(0),
                    "The provided nodes are not valid for doing a range move: " + nodes);
        }
        return refactorings.createMoveTarget(nodes.get(0), nodes.get(nodes.size() - 1));
    }

    private boolean isValidForRangeOperation(List<? extends ASTNode> nodes) {
        return nodesHaveSameParentAndLocation(nodes) && refactorings.isValidRange(nodes);
    }

    private boolean nodesHaveSameParentAndLocation(List<? extends ASTNode> nodes) {
        if (nodes.isEmpty()) {
            return true;
        }
        final ASTNode firstNode = nodes.get(0);
        final ASTNode parent = firstNode.getParent();
        final StructuralPropertyDescriptor locInParent = firstNode.getLocationInParent();
        for (ASTNode node : nodes) {
            if (!equal(node.getParent(), parent)
                    || !equal(node.getLocationInParent(), locInParent)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Returns a copy of the provided {@link ASTNode}.
     * This method loses code comments. Prefer using {@link #copy(ASTNode)}.
     *
     * @param <T> the actual node type
     * @param node the node to copy
     * @return a copy of the node
     */
    @SuppressWarnings("unchecked")
    public <T extends ASTNode> T copySubtree(T node) {
        return (T) ASTNode.copySubtree(ast, node);
    }

    /**
     * Builds a new {@link VariableDeclarationStatement} instance.
     *
     * @param type
     *            the type of the variable being declared
     * @param varName
     *            the name of the variable being declared
     * @param initializer
     *            the variable initializer, can be null
     * @return a new variable declaration statement
     */
    public VariableDeclarationStatement declareStmt(Type type, SimpleName varName, Expression initializer) {
        final VariableDeclarationFragment fragment = declareFragment(varName, initializer);
        final VariableDeclarationStatement vds = ast.newVariableDeclarationStatement(fragment);
        vds.setType(type);
        return vds;
    }

    /**
     * Builds a new {@link VariableDeclarationExpression} instance.
     *
     * @param type
     *            the type of the variable being declared
     * @param varName
     *            the name of the variable being declared
     * @param initializer
     *            the variable initializer, can be null
     * @return a new variable declaration expression
     */
    public VariableDeclarationExpression declareExpr(Type type, SimpleName varName, Expression initializer) {
        final VariableDeclarationFragment fragment = declareFragment(varName, initializer);
        final VariableDeclarationExpression vde = ast.newVariableDeclarationExpression(fragment);
        modifiers(vde).add(final0());
        vde.setType(type);
        return vde;
    }

    /**
     * Builds a new {@link VariableDeclarationExpression} instance.
     *
     * @param type
     *            the declared variable type
     * @param fragment
     *            the variable declaration fragment
     * @return a new variable declaration expression
     */
    public VariableDeclarationExpression declareExpr(Type type, VariableDeclarationFragment fragment) {
        final VariableDeclarationExpression vde = ast.newVariableDeclarationExpression(fragment);
        vde.setType(type);
        return vde;
    }

    /**
     * Builds a new {@link FieldDeclaration} instance.
     *
     * @param type
     *            the declared variable type
     * @param fragment
     *            the variable declaration fragment
     * @return a new field declaration
     */
    public FieldDeclaration declareField(Type type, VariableDeclarationFragment fragment) {
        final FieldDeclaration fd = ast.newFieldDeclaration(fragment);
        fd.setType(type);
        return fd;
    }

    /**
     * Builds a new {@link VariableDeclarationFragment} instance.
     *
     * @param varName
     *            the declared variable name
     * @return a new variable declaration fragment
     */
    public VariableDeclarationFragment declareFragment(SimpleName varName) {
        final VariableDeclarationFragment vdf = ast.newVariableDeclarationFragment();
        vdf.setName(varName);
        return vdf;
    }

    /**
     * Builds a new {@link VariableDeclarationFragment} instance.
     *
     * @param varName
     *            the declared variable name
     * @param initializer
     *            the variable initializer
     * @return a new variable declaration fragment
     */
    public VariableDeclarationFragment declareFragment(SimpleName varName, Expression initializer) {
        final VariableDeclarationFragment vdf = ast.newVariableDeclarationFragment();
        vdf.setName(varName);
        vdf.setInitializer(initializer);
        return vdf;
    }

    /**
     * Helper to create a list of modifiers.
     *
     * @param modifiers the list of modifiers
     * @return a new list of modifiers
     */
    public List<IExtendedModifier> extendedModifiers(IExtendedModifier... modifiers) {
        return Arrays.asList(modifiers);
    }

    /**
     * Builds a new {@link FieldAccess} instance.
     *
     * @param expr
     *            the expression on which the field is accessed
     * @param fieldName
     *            the field name being accessed
     * @return a new single field access
     */
    public FieldAccess fieldAccess(Expression expr, SimpleName fieldName) {
        final FieldAccess fa = getAST().newFieldAccess();
        fa.setExpression(expr);
        fa.setName(fieldName);
        return fa;
    }

    /**
     * Builds a new {@link Modifier} with keyword {@code final}.
     *
     * @return a {@code final} modifier
     */
    public Modifier final0() {
        return ast.newModifier(FINAL_KEYWORD);
    }

    /**
     * Builds a new {@link SingleVariableDeclaration} instance.
     *
     * @param varName
     *            the name of the variable being declared
     * @param type
     *            the type of the variable being declared
     * @return a new single variable declaration
     */
    public SingleVariableDeclaration declareSingleVariable(String varName, Type type) {
        final SingleVariableDeclaration svd = ast.newSingleVariableDeclaration();
        svd.setName(simpleName(varName));
        svd.setType(type);
        return svd;
    }

    /**
     * Builds a new {@link IfStatement} instance.
     *
     * @param condition
     *            the if condition
     * @param thenStatement
     *            the then statement
     * @return a new if statement
     */
    public IfStatement if0(Expression condition, Statement thenStatement) {
        return if0(condition, thenStatement, null);
    }

    /**
     * Builds a new {@link DoStatement} instance.
     *
     * @param condition the while condition
     * @param statement the statement of the loop
     * @return a new do statement
     */
    public DoStatement doWhile(Expression condition, Statement statement) {
        final DoStatement ds = ast.newDoStatement();
        ds.setExpression(condition);
        ds.setBody(statement);
        return ds;
    }

    /**
     * Builds a new {@link IfStatement} instance.
     *
     * @param condition the if condition
     * @param thenStatement the statement of the then clause
     * @param elseStatement the statement of the else clause
     * @return a new if statement
     */
    public IfStatement if0(Expression condition, Statement thenStatement, Statement elseStatement) {
        final IfStatement is = ast.newIfStatement();
        is.setExpression(condition);
        is.setThenStatement(thenStatement);
        is.setElseStatement(elseStatement);
        return is;
    }

    /**
     * Builds a new {@link InfixExpression} instance.
     *
     * @param operator the infix operator
     * @param allOperands the operands
     * @return a new infix expression
     */
    public InfixExpression infixExpr(InfixExpression.Operator operator,
            Collection<? extends Expression> allOperands) {
        if (allOperands.size() < 2) {
            throw new IllegalArgumentException(null, "Not enough operands for an infix expression: "
                    + "needed at least 2, but got " + allOperands.size());
        }
        final Iterator<? extends Expression> it = allOperands.iterator();
        final InfixExpression ie = ast.newInfixExpression();
        ie.setLeftOperand(it.next());
        ie.setOperator(operator);
        ie.setRightOperand(it.next());
        while (it.hasNext()) {
            extendedOperands(ie).add(it.next());
        }
        return ie;
    }

    /**
     * Builds a new {@link ConditionalExpression} instance.
     *
     * @param mainExpression the main expression
     * @param thenExpression the evaluated expression if the main expression is true
     * @param elseExpression the evaluated expression if the main expression is false
     * @return a new conditional expression
     */
    public ConditionalExpression conditionalExpr(Expression mainExpression,
            Expression thenExpression,
            Expression elseExpression) {
        final ConditionalExpression ce = ast.newConditionalExpression();
        ce.setExpression(mainExpression);
        ce.setThenExpression(thenExpression);
        ce.setElseExpression(elseExpression);
        return ce;
    }

    /**
     * Builds a new {@link InfixExpression} instance.
     *
     * @param leftOperand the left operand
     * @param operator the infix operator
     * @param rightOperand the right operand
     * @param extendedOperands the extended operands
     * @return a new infix expression
     */
    public InfixExpression infixExpr(Expression leftOperand, InfixExpression.Operator operator,
            Expression rightOperand, Expression... extendedOperands) {
        final InfixExpression ie = ast.newInfixExpression();
        ie.setLeftOperand(leftOperand);
        ie.setOperator(operator);
        ie.setRightOperand(rightOperand);
        Collections.addAll(extendedOperands(ie), extendedOperands);
        return ie;
    }

    /**
     * Builds a new {@link NumberLiteral} instance.
     *
     * @param intValue the number literal value
     * @return a new number literal
     */
    public NumberLiteral int0(int intValue) {
        return ast.newNumberLiteral(Integer.toString(intValue));
    }

    /**
     * Builds a new {@link MethodInvocation} instance.
     *
     * @param expression the method invocation expression
     * @param methodName the name of the invoked method
     * @param arguments the arguments for the method invocation
     * @return a new method invocation
     */
    public MethodInvocation invoke(String expression, String methodName, Expression... arguments) {
        final MethodInvocation mi = ast.newMethodInvocation();
        mi.setExpression(ast.newSimpleName(expression));
        mi.setName(ast.newSimpleName(methodName));
        addAll(arguments(mi), arguments);
        return mi;
    }

    /**
     * Builds a new {@link MethodInvocation} instance.
     *
     * @param methodName the name of the invoked method
     * @param arguments the arguments for the method invocation
     * @return a new method invocation
     */
    public MethodInvocation invoke(String methodName, Expression... arguments) {
        final MethodInvocation mi = ast.newMethodInvocation();
        mi.setName(ast.newSimpleName(methodName));
        addAll(arguments(mi), arguments);
        return mi;
    }

    /**
     * Builds a new {@link MethodInvocation} instance.
     *
     * @param expression the method invocation expression
     * @param methodName the name of the invoked method
     * @param arguments the arguments for the method invocation
     * @return a new method invocation
     */
    public MethodInvocation invoke(Expression expression, String methodName, Expression... arguments) {
        final MethodInvocation mi = ast.newMethodInvocation();
        mi.setExpression(expression);
        mi.setName(ast.newSimpleName(methodName));
        addAll(arguments(mi), arguments);
        return mi;
    }

    /**
     * Builds a new {@link MethodInvocation} instance.
     *
     * @param <E> the arguments type
     * @param expression the method invocation expression
     * @param methodName the name of the invoked method
     * @param arguments the arguments for the method invocation
     * @return a new method invocation
     */
    public <E extends Expression> MethodInvocation invoke(Expression expression, String methodName, List<E> arguments) {
        final MethodInvocation mi = ast.newMethodInvocation();
        mi.setExpression(expression);
        mi.setName(ast.newSimpleName(methodName));
        addAll(mi, arguments);
        return mi;
    }

    private boolean isEmptyRangeCopy(ASTNode... nodes) {
        return nodes.length == 1 && nodes[0] == null;
    }

    private <E extends ASTNode> boolean isEmptyRangeCopy(List<E> nodes) {
        return nodes.size() == 1 && nodes.get(0) == null;
    }

    /**
     * Builds a new {@link NumberLiteral} instance.
     *
     * @return a new number literal
     */
    public NumberLiteral numberLiteral() {
        return ast.newNumberLiteral();
    }

    /**
     * Builds a new {@link CharacterLiteral} instance.
     *
     * @return a new character literal
     */
    public CharacterLiteral charLiteral() {
        return ast.newCharacterLiteral();
    }

    /**
     * Builds a new {@link LambdaExpression} instance.
     *
     * @return a new lambda expression
     */
    public LambdaExpression lambda() {
        return ast.newLambdaExpression();
    }

    /**
     * Builds a new {@link ExpressionMethodReference} instance.
     *
     * @return a new expression method reference
     */
    public ExpressionMethodReference exprMethodRef() {
        return ast.newExpressionMethodReference();
    }

    /**
     * Builds a new {@link TypeMethodReference} instance.
     *
     * @return a new type method reference
     */
    public TypeMethodReference typeMethodRef() {
        return ast.newTypeMethodReference();
    }

    /**
     * Builds a new {@link CreationReference} instance.
     *
     * @return a new creation reference
     */
    public CreationReference creationRef() {
        return ast.newCreationReference();
    }

    /**
     * Builds a new {@link SuperMethodReference} instance.
     *
     * @return a new super method reference
     */
    public SuperMethodReference superMethodRef() {
        return ast.newSuperMethodReference();
    }

    /**
     * Returns a placeholder where to move the provided {@link ASTNode}.
     *
     * @param <T> the actual node type
     * @param nodeToMove the node to move
     * @return a placeholder for the moved node
     */
    public <T extends ASTNode> T move(T nodeToMove) {
        return refactorings.createMoveTarget(nodeToMove);
    }

    /**
     * Moves all the provided {@link ASTNode}s in place.
     *
     * @param <T> the actual nodes type
     * @param nodes the nodes to move
     * @return the provided list with all nodes moved
     */
    public <T extends ASTNode> List<T> move(final List<T> nodes) {
        for (ListIterator<T> it = nodes.listIterator(); it.hasNext();) {
            it.set(move(it.next()));
        }
        return nodes;
    }

    /**
     * Builds a new {@link Name} instance. If only a single name is provided then a {@link SimpleName} is returned,
     * if several names are provided then a {@link QualifiedName} is built.
     *
     * @param names the qualified or simple name
     * @return a new name
     * @throws IllegalArgumentException if no names are provided
     */
    public Name name(String... names) {
        if (names.length == 0) {
            throw new IllegalArgumentException(null, "Expected at least one name, but was given 0 names");
        }
        if (names.length == 1) {
            return simpleName(names[0]);
        }
        return ast.newName(names);
    }

    /**
     * Builds a new {@link SimpleName} instance.
     *
     * @param simpleName the simple name
     * @return a new simple name
     */
    public SimpleName simpleName(String simpleName) {
        return ast.newSimpleName(simpleName);
    }

    /**
     * Builds a new {@link ClassInstanceCreation} instance.
     *
     * @param typeName the instantiated type name
     * @param arguments the constructor invocation arguments
     * @return a new class instance creation
     */
    public ClassInstanceCreation new0(String typeName, Expression... arguments) {
        final ClassInstanceCreation cic = ast.newClassInstanceCreation();
        cic.setType(simpleType(typeName));
        addAll(arguments(cic), arguments);
        return cic;
    }

    /**
     * Builds a new {@link ClassInstanceCreation} instance.
     *
     * @param type the instantiated type
     * @param arguments the constructor invocation arguments
     * @return a new class instance creation
     */
    public ClassInstanceCreation new0(Type type, Expression... arguments) {
        final ClassInstanceCreation cic = ast.newClassInstanceCreation();
        cic.setType(type);
        addAll(arguments(cic), arguments);
        return cic;
    }

    @SuppressWarnings("unchecked")
    private <T extends ASTNode> void addAll(List<T> whereToAdd, T... toAdd) {
        if (!isEmptyRangeCopy(toAdd)) {
            Collections.addAll(whereToAdd, toAdd);
        }
    }

    private <E extends Expression> void addAll(MethodInvocation mi, List<E> arguments) {
        if (!isEmptyRangeCopy(arguments)) {
            arguments(mi).addAll(arguments);
        }
    }

    /**
     * Builds a new {@link ArrayCreation} instance.
     *
     * @param arrayType the array type
     * @param arrayInitializer the array initializer
     * @return a new array creation instance
     */
    public ArrayCreation newArray(ArrayType arrayType, ArrayInitializer arrayInitializer) {
        final ArrayCreation ac = ast.newArrayCreation();
        ac.setType(arrayType);
        ac.setInitializer(arrayInitializer);
        return ac;
    }

    /**
     * Builds a new {@link PrefixExpression} instance using the not operator ('!').
     *
     * @param expr the expression to negate
     * @return a new prefix expression
     */
    public Expression not(Expression expr) {
        return prefixExpr(NOT, expr);
    }

    /**
     * Negates the provided expression by moving it in the AST.
     *
     * @param expr the expression to negate
     * @return the negated expression, moved in the AST
     */
    public Expression negate(Expression expr) {
        return negate(expr, Copy.MOVE);
    }

    /**
     * Negates the provided expression and applies the provided copy operation on the returned expression.
     *
     * @param expr the expression to negate
     * @param copy the copy operation to perform
     * @return the negated expression, copied according to the copy operation
     */
    public Expression negate(Expression expr, Copy copy) {
        final Expression exprNoParen = removeParentheses(expr);
        if (exprNoParen.getNodeType() == PREFIX_EXPRESSION) {
            final PrefixExpression pe = (PrefixExpression) exprNoParen;
            if (hasOperator(pe, NOT)) {
                return copy.perform(this, removeParentheses(pe.getOperand()));
            }
        }

        return not(parenthesizeIfNeeded(copy.perform(this, expr)));
    }

    /**
     * Builds a new {@link NumberLiteral} instance.
     *
     * @param s the number literal value
     * @return a new number literal
     */
    public NumberLiteral number(String s) {
        return ast.newNumberLiteral(s);
    }

    /**
     * Builds a new {@link ParenthesizedExpression} instance.
     *
     * @param expression the expression to wrap with parentheses
     * @return a new parenthesized expression
     */
    public ParenthesizedExpression parenthesize(Expression expression) {
        final ParenthesizedExpression pe = ast.newParenthesizedExpression();
        pe.setExpression(expression);
        return pe;
    }

    private Expression prefixExpr(PrefixExpression.Operator operator, Expression operand) {
        final PrefixExpression pe = ast.newPrefixExpression();
        pe.setOperator(operator);
        pe.setOperand(operand);
        return pe;
    }

    /**
     * Builds a new {@link ReturnStatement} instance.
     *
     * @param expression the expression to return
     * @return a new return statement
     */
    public ReturnStatement return0(Expression expression) {
        final ReturnStatement rs = ast.newReturnStatement();
        rs.setExpression(expression);
        return rs;
    }

    /**
     * Builds a new {@link MarkerAnnotation} instance.
     *
     * @param typeName the annotation type name
     * @return a new marker annotation
     */
    public MarkerAnnotation markerAnnotation(Name typeName) {
        final MarkerAnnotation ma = ast.newMarkerAnnotation();
        ma.setTypeName(typeName);
        return ma;
    }

    /**
     * Builds a new {@link SingleMemberAnnotation} instance.
     *
     * @param typeName the annotation type name
     * @param value the annotation single value
     * @return a new single member annotation
     */
    public SingleMemberAnnotation singleValueAnnotation(Name typeName, Expression value) {
        final SingleMemberAnnotation sma = ast.newSingleMemberAnnotation();
        sma.setTypeName(typeName);
        sma.setValue(value);
        return sma;
    }

    /**
     * Builds a new {@link StringLiteral} instance.
     *
     * @param s the string literal value
     * @return a new string literal
     */
    public StringLiteral string(String s) {
        final StringLiteral sl = ast.newStringLiteral();
        sl.setLiteralValue(s);
        return sl;
    }

    /**
     * Builds a new {@link SwitchStatement} instance.
     *
     * @param expr
     *            the switch expression
     * @return a new switch statement
     */
    public SwitchStatement switch0(Expression expr) {
        final SwitchStatement ss = ast.newSwitchStatement();
        ss.setExpression(expr);
        return ss;
    }

    /**
     * Builds a new {@link ThisExpression} instance.
     *
     * @return a new this expression
     */
    public ThisExpression this0() {
        return ast.newThisExpression();
    }

    /**
     * Builds a new {@link ThrowStatement} instance.
     *
     * @param expression the expression to throw
     * @return a new throw statement
     */
    public ThrowStatement throw0(final Expression expression) {
        final ThrowStatement throwS = ast.newThrowStatement();
        throwS.setExpression(expression);
        return throwS;
    }

    /**
     * Builds a new {@link ExpressionStatement} instance.
     *
     * @param expression the expression to transform into a statement
     * @return a new expression statement
     */
    public ExpressionStatement toStmt(final Expression expression) {
        return ast.newExpressionStatement(expression);
    }

    /**
     * Builds a new {@link TryStatement} instance.
     *
     * @param body the try body
     * @param catchClauses the catch clauses for the try
     * @return a new try statement
     */
    public TryStatement try0(final Block body, CatchClause... catchClauses) {
        final TryStatement tryS = ast.newTryStatement();
        tryS.setBody(body);
        addAll(catchClauses(tryS), catchClauses);
        return tryS;
    }

    /**
     * Parenthesizes the provided expression if its type requires it.
     *
     * @param expr the expression to conditionally return parenthesized
     * @return the parenthesized expression of the provided expression to return or this expression itself
     */
    public Expression parenthesizeIfNeeded(Expression expr) {
        switch (expr.getNodeType()) {
        case ASSIGNMENT:
        case CONDITIONAL_EXPRESSION:
        case INFIX_EXPRESSION:
        case INSTANCEOF_EXPRESSION:
            return parenthesize(expr);
        default:
            return expr;
        }
    }

    /**
     * Builds a new {@link Statement} instance which is basically a newline.
     *
     * @return a newline statement
     */
    public Statement newlinePlaceholder() {
        return (Statement) refactorings.getRewrite().createStringPlaceholder("\n", EMPTY_STATEMENT);
    }

    /**
     * Builds a new {@link Modifier} with keyword {@code public}.
     *
     * @return a {@code public} modifier
     */
    public Modifier public0() {
        return ast.newModifier(PUBLIC_KEYWORD);
    }

    /**
     * Builds a new {@link Modifier} with keyword {@code private}.
     *
     * @return a {@code private} modifier
     */
    public Modifier private0() {
        return ast.newModifier(PRIVATE_KEYWORD);
    }

    /**
     * Builds a new {@link Modifier} with keyword {@code protected}.
     *
     * @return a {@code protected} modifier
     */
    public Modifier protected0() {
        return ast.newModifier(PROTECTED_KEYWORD);
    }

    /**
     * Builds a new {@link Modifier} with keyword {@code static}.
     *
     * @return a {@code static} modifier
     */
    public Modifier static0() {
        return ast.newModifier(STATIC_KEYWORD);
    }

    /**
     * Builds a new super method invocation.
     *
     * @param methodName name of the method to be invoked
     * @return expression with a method invocation
     */
    public Expression superInvoke(String methodName) {
        SuperMethodInvocation smi = ast.newSuperMethodInvocation();
        smi.setName(simpleName(methodName));
        return smi;
    }

    /**
     * Builds a new {@link MethodDeclaration} node.
     *
     * @param modifiers list of modifiers of the method
     * @param methodName the method name
     * @param parameters list of parameters
     * @param block the block of the method
     * @return a new method declaration
     */
    public MethodDeclaration method(List<IExtendedModifier> modifiers, String methodName,
            List<SingleVariableDeclaration> parameters, Block block) {
        final MethodDeclaration md = ast.newMethodDeclaration();
        modifiers(md).addAll(modifiers);
        md.setName(simpleName(methodName));
        md.parameters().addAll(parameters);
        md.setBody(block);
        return md;
    }

    /**
     * Builds a new {@link NullLiteral}.
     *
     * @return a {@code null} literal
     */
    public NullLiteral null0() {
        return ast.newNullLiteral();
    }
}

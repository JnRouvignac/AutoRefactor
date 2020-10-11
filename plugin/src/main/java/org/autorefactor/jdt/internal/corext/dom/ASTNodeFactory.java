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
package org.autorefactor.jdt.internal.corext.dom;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;

import org.autorefactor.jdt.core.dom.ASTRewrite;
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
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.LambdaExpression;
import org.eclipse.jdt.core.dom.MarkerAnnotation;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.Modifier.ModifierKeyword;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.NullLiteral;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.ParameterizedType;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PostfixExpression;
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
 * Helper class for building AST note in a somewhat fluent API. Method names
 * which are also java keywords are postfixed with a "0".
 */
public class ASTNodeFactory {
	private final AST ast;
	private final ASTRewrite rewrite;

	/**
	 * Class constructor.
	 *
	 * @param refactorings the cleanups
	 */
	public ASTNodeFactory(final ASTRewrite refactorings) {
		this.rewrite= refactorings;
		this.ast= refactorings.getAST();
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
	public Annotation annotation(final String typeName) {
		return newMarkerAnnotation(newSimpleName(typeName));
	}

	/**
	 * Helper to create a list of parameters.
	 *
	 * @param variableDeclarations the list of parameters
	 * @return a new list of parameters
	 */
	public List<SingleVariableDeclaration> parameters(final SingleVariableDeclaration... variableDeclarations) {
		return Arrays.asList(variableDeclarations);
	}

	/**
	 * Builds a new {@link Assignment} instance.
	 *
	 * @return a new Assignment
	 */
	public Assignment newAssignment() {
		return ast.newAssignment();
	}

	/**
	 * Builds a new {@link Assignment} instance.
	 *
	 * @param leftHandSide      the left hand side expression
	 * @param operator the assignment operator
	 * @param rightHandSide      the right hand side expression
	 * @return a new Assignment
	 */
	public Assignment newAssignment(final Expression leftHandSide, final Assignment.Operator operator, final Expression rightHandSide) {
		Assignment newAssignment= newAssignment();
		newAssignment.setLeftHandSide(leftHandSide);
		newAssignment.setOperator(operator);
		newAssignment.setRightHandSide(rightHandSide);
		return newAssignment;
	}

	/**
	 * Builds a new {@link Block} instance.
	 *
	 * @param statements the statements to add to the block
	 * @return a new Block
	 */
	public Block newBlock(final Statement... statements) {
		Block block= ast.newBlock();
		addAll(block.statements(), statements);
		return block;
	}

	/**
	 * Builds a new {@link Block} instance.
	 *
	 * @param statements the statements to add to the block
	 * @return a new Block
	 */
	public Block newBlock(final Collection<Statement> statements) {
		Block block= ast.newBlock();
		block.statements().addAll(statements);
		return block;
	}

	/**
	 * Builds a new {@link BooleanLiteral} instance.
	 *
	 * @param boolValue the boolean literal value
	 * @return a new boolean literal
	 */
	public BooleanLiteral newBooleanLiteral(final boolean boolValue) {
		return ast.newBooleanLiteral(boolValue);
	}

	/**
	 * Builds a new {@link BreakStatement} instance.
	 *
	 * @return a new break statement
	 */
	public BreakStatement newBreakStatement() {
		return ast.newBreakStatement();
	}

	/**
	 * Builds a new {@link SwitchCase} instance.
	 *
	 * @param expression the case expression
	 * @return a new switch case statement
	 */
	@SuppressWarnings("deprecation")
	public SwitchCase newSwitchCase(final Expression expression) {
		SwitchCase sc= ast.newSwitchCase();
		sc.setExpression(expression);
		return sc;
	}

	/**
	 * Builds a new {@link CastExpression} instance.
	 *
	 * @return a new CastExpression
	 */
	public CastExpression newCastExpression() {
		return ast.newCastExpression();
	}

	/**
	 * Builds a new {@link CastExpression} instance.
	 *
	 * @param type the type being cast to
	 * @param expression the expression being cast
	 * @return a new CastExpression
	 */
	public CastExpression newCastExpression(final Type type, final Expression expression) {
		CastExpression newCastExpression= newCastExpression();
		newCastExpression.setType(type);
		newCastExpression.setExpression(parenthesizeIfNeeded(expression));
		return newCastExpression;
	}

	/**
	 * Builds a new {@link ArrayInitializer} instance.
	 *
	 * @return a new array initializer
	 */
	public ArrayInitializer newArrayInitializer() {
		return ast.newArrayInitializer();
	}

	/**
	 * Builds a new {@link SwitchCase} instance which represents a {@code default}
	 * statement.
	 *
	 * @return a new switch case statement representing a {@code default} statement
	 */
	public SwitchCase default0() {
		return newSwitchCase(null);
	}

	/**
	 * Returns a type for the provided type name (simple or qualified name).
	 *
	 * @param typeName the type name (simple or qualified name)
	 * @return a type for the provided type name
	 */
	public Type type(final String typeName) {
		if (typeName.indexOf('.') == -1) {
			return simpleType(typeName);
		}

		return qualifiedType(typeName.split("\\.")); //$NON-NLS-1$
	}

	private Type simpleType(final String name) {
		Code primitiveTypeCode= PrimitiveType.toCode(name);
		if (primitiveTypeCode != null) {
			return ast.newPrimitiveType(primitiveTypeCode);
		}

		return ast.newSimpleType(ast.newSimpleName(name));
	}

	private Type qualifiedType(final String... names) {
		switch (names.length) {
		case 0:
			throw new IllegalArgumentException(null, "Expected one or more names, but got 0"); //$NON-NLS-1$
		case 1:
			return simpleType(names[0]);
		default:
			Type type= ast.newSimpleType(ast.newSimpleName(names[0]));
			for (int i= 1; i < names.length; i++) {
				type= ast.newQualifiedType(type, ast.newSimpleName(names[i]));
			}

			return type;
		}
	}

	/**
	 * Returns a parameterized type with the provided type name and type arguments.
	 *
	 * @param typeName      the type name (simple or qualified name)
	 * @param typeArguments the type arguments
	 * @return a new parameterized type
	 */
	public Type newParameterizedType(final String typeName, final Type... typeArguments) {
		Type type= type(typeName);
		ParameterizedType parameterizedType= ast.newParameterizedType(type);
		Collections.addAll(parameterizedType.typeArguments(), typeArguments);
		return parameterizedType;
	}

	/**
	 * Builds a new {@link CatchClause} instance.
	 *
	 * @param exceptionTypeName   the exception type name
	 * @param caughtExceptionName the local name for the caught exception
	 * @param statements          the statements to add to the catch clause
	 * @return a new catch clause
	 */
	public CatchClause newCatchClause(final String exceptionTypeName, final String caughtExceptionName, final Statement... statements) {
		CatchClause cc= ast.newCatchClause();
		SingleVariableDeclaration svd= ast.newSingleVariableDeclaration();
		svd.setType(simpleType(exceptionTypeName));
		svd.setName(ast.newSimpleName(caughtExceptionName));
		cc.setException(svd);

		Block block= ast.newBlock();
		addAll(block.statements(), statements);
		cc.setBody(block);
		return cc;
	}

	private boolean isValidInCurrentAST(final ASTNode node) {
		return node.getAST() == ast && node.getStartPosition() != -1;
	}

	/**
	 * Creates a type by copying the type binding of the provided expression.
	 *
	 * @param expression      the expression whose type must be copied
	 * @param typeNameDecider decides on how the type should be referenced (simple
	 *                        name or qualified name)
	 * @return a new type
	 */
	public Type copyType(final Expression expression, final TypeNameDecider typeNameDecider) {
		return toType(expression.resolveTypeBinding(), typeNameDecider);
	}

	/**
	 * Converts a type binding into a type.
	 *
	 * @param typeBinding     the type binding to convert
	 * @param typeNameDecider decides on how the type should be referenced (simple
	 *                        name or qualified name)
	 * @return a new type
	 */
	public Type toType(final ITypeBinding typeBinding, final TypeNameDecider typeNameDecider) {
		if (typeBinding == null) {
			throw new IllegalArgumentException(null, "typeBinding cannot be null"); //$NON-NLS-1$
		}

		if (typeBinding.isParameterizedType()) {
			ParameterizedType type= ast.newParameterizedType(toType(typeBinding.getErasure(), typeNameDecider));
			List<Type> typeArgs= type.typeArguments();
			for (ITypeBinding typeArg : typeBinding.getTypeArguments()) {
				typeArgs.add(toType(typeArg, typeNameDecider));
			}

			return type;
		}
		if (typeBinding.isPrimitive()) {
			return type(typeBinding.getName());
		}
		if (typeBinding.isClass() || typeBinding.isInterface() || typeBinding.isEnum()
				|| typeBinding.isAnnotation() || typeBinding.isNullType() || typeBinding.isRawType()) {
			return type(typeNameDecider.useSimplestPossibleName(typeBinding));
		}
		if (typeBinding.isArray()) {
			return ast.newArrayType(toType(typeBinding.getElementType(), typeNameDecider), typeBinding.getDimensions());
		}
		if (typeBinding.isWildcardType()) {
			WildcardType type= ast.newWildcardType();
			if (typeBinding.getBound() != null) {
				type.setBound(toType(typeBinding.getBound(), typeNameDecider), typeBinding.isUpperbound());
			}

			return type;
		}
		if (typeBinding.isTypeVariable()) {
			return type(typeBinding.getName());
		}
		if (typeBinding.isCapture()) {
			if (typeBinding.getTypeBounds().length > 1) {
				throw new NotImplementedException(null,
						"because it violates the javadoc of `ITypeBinding.getTypeBounds()`: " //$NON-NLS-1$
						+ "\"Note that per construction, it can only contain one class or array type, " //$NON-NLS-1$
						+ "at most, and then it is located in first position.\""); //$NON-NLS-1$
			}

			return toType(typeBinding.getWildcard(), typeNameDecider);
		}
		throw new NotImplementedException(null, " for the type binding '" + typeBinding + "'"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	private Type copyType(final Type type) {
		switch (type.getNodeType()) {
		case ASTNode.ARRAY_TYPE:
			ArrayType arrayType= (ArrayType) type;
			return ast.newArrayType(copyType(arrayType.getElementType()), arrayType.getDimensions());

		case ASTNode.PRIMITIVE_TYPE:
			Code code= ((PrimitiveType) type).getPrimitiveTypeCode();
			return ast.newPrimitiveType(code);

		case ASTNode.QUALIFIED_TYPE:
			ITypeBinding typeBinding= type.resolveBinding();

			if (typeBinding == null) {
				return null;
			}

			return type(typeBinding.getQualifiedName());

		case ASTNode.SIMPLE_TYPE:
			SimpleType sType= (SimpleType) type;
			return ast.newSimpleType(createCopyTarget(sType.getName()));

		case ASTNode.PARAMETERIZED_TYPE:
			ParameterizedType pType= (ParameterizedType) type;
			ParameterizedType copyOfType= ast.newParameterizedType(createCopyTarget(pType.getType()));
			List<Type> newTypeArgs= copyOfType.typeArguments();
			for (Object typeArg : pType.typeArguments()) {
				if (((Type) typeArg).isWildcardType()) {
					newTypeArgs.add(ast.newWildcardType());
				} else {
					newTypeArgs.add(createCopyTarget((Type) typeArg));
				}
			}

			return copyOfType;
		}

		throw new NotImplementedException(null, "Unknown type for type " + type); //$NON-NLS-1$
	}

	/**
	 * Returns a copy of the expression of the provided {@link MethodInvocation} or
	 * null if no such expression exists.
	 *
	 * @param node the {@link MethodInvocation} for which to copy the expression
	 * @return a copy of the expression, or false if no such expression exists
	 */
	public Expression copyExpression(final MethodInvocation node) {
		return node.getExpression() != null ? createCopyTarget(node.getExpression()) : null;
	}

	/**
	 * Returns a copy of the provided nodes list.
	 *
	 * @param <T>   the actual nodes's type
	 * @param nodes the nodes list to copy
	 * @return a single node, representing a copy of the nodes list
	 */
	public <T extends ASTNode> T copyRange(final List<T> nodes) {
		if (nodes.isEmpty()) {
			return null;
		}
		if (!isValidForRangeOperation(nodes)) {
			throw new IllegalArgumentException(nodes.get(0),
					"The provided nodes are not valid for doing a range copy: " + nodes); //$NON-NLS-1$
		}

		return rewrite.createCopyTarget(nodes.get(0), nodes.get(nodes.size() - 1));
	}

	/**
	 * Returns a move for the provided nodes list.
	 *
	 * @param <T>   the actual nodes's type
	 * @param nodes the nodes list to move
	 * @return a single node, representing a move of the nodes list
	 */
	public <T extends ASTNode> T moveRange(final List<T> nodes) {
		if (nodes.isEmpty()) {
			return null;
		}
		if (!isValidForRangeOperation(nodes)) {
			throw new IllegalArgumentException(nodes.get(0),
					"The provided nodes are not valid for doing a range move: " + nodes); //$NON-NLS-1$
		}

		return rewrite.createMoveTarget(nodes.get(0), nodes.get(nodes.size() - 1));
	}

	private boolean isValidForRangeOperation(final List<? extends ASTNode> nodes) {
		return nodesHaveSameParentAndLocation(nodes) && rewrite.isValidRange(nodes);
	}

	private boolean nodesHaveSameParentAndLocation(final List<? extends ASTNode> nodes) {
		if (nodes.isEmpty()) {
			return true;
		}
		ASTNode firstNode= nodes.get(0);
		ASTNode parent= firstNode.getParent();
		StructuralPropertyDescriptor locInParent= firstNode.getLocationInParent();
		for (ASTNode node : nodes) {
			if (!Objects.equals(node.getParent(), parent) || !Objects.equals(node.getLocationInParent(), locInParent)) {
				return false;
			}
		}

		return true;
	}

	/**
	 * Returns a copy of the provided {@link ASTNode}. This method loses code
	 * comments. Prefer using {@link #createCopyTarget(ASTNode)}.
	 *
	 * @param <T>  the actual node type
	 * @param node the node to copy
	 * @return a copy of the node
	 */
	public <T extends ASTNode> T copySubtree(final T node) {
		return (T) ASTNode.copySubtree(ast, node);
	}

	/**
	 * Builds a new {@link VariableDeclarationStatement} instance.
	 *
	 * @param type        the type of the variable being declared
	 * @param varName     the name of the variable being declared
	 * @param initializer the variable initializer, can be null
	 * @return a new variable declaration statement
	 */
	public VariableDeclarationStatement declareStatement(final Type type, final SimpleName varName, final Expression initializer) {
		VariableDeclarationFragment fragment= newVariableDeclarationFragment(varName, initializer);
		return newVariableDeclarationStatement(type, fragment);
	}

	/**
	 * Builds a new {@link VariableDeclarationStatement} instance.
	 *
	 * @param type     the type of the variable being declared
	 * @param fragment the fragment being declared
	 * @return a new variable declaration statement
	 */
	public VariableDeclarationStatement newVariableDeclarationStatement(final Type type, final VariableDeclarationFragment fragment) {
		VariableDeclarationStatement variableDeclarationStatement= ast.newVariableDeclarationStatement(fragment);
		variableDeclarationStatement.setType(type);
		return variableDeclarationStatement;
	}

	/**
	 * Builds a new {@link VariableDeclarationExpression} instance.
	 *
	 * @param type        the type of the variable being declared
	 * @param varName     the name of the variable being declared
	 * @param initializer the variable initializer, can be null
	 * @return a new variable declaration expression
	 */
	public VariableDeclarationExpression newVariableDeclarationExpression(final Type type, final SimpleName varName, final Expression initializer) {
		VariableDeclarationFragment fragment= newVariableDeclarationFragment(varName, initializer);
		VariableDeclarationExpression variableDeclarationExpression= ast.newVariableDeclarationExpression(fragment);
		variableDeclarationExpression.modifiers().add(final0());
		variableDeclarationExpression.setType(type);
		return variableDeclarationExpression;
	}

	/**
	 * Builds a new {@link VariableDeclarationExpression} instance.
	 *
	 * @param type     the declared variable type
	 * @param fragment the variable declaration fragment
	 * @return a new variable declaration expression
	 */
	public VariableDeclarationExpression newVariableDeclarationExpression(final Type type, final VariableDeclarationFragment fragment) {
		VariableDeclarationExpression variableDeclarationExpression= ast.newVariableDeclarationExpression(fragment);
		variableDeclarationExpression.setType(type);
		return variableDeclarationExpression;
	}

	/**
	 * Builds a new {@link FieldDeclaration} instance.
	 *
	 * @param type     the declared variable type
	 * @param fragment the variable declaration fragment
	 * @return a new field declaration
	 */
	public FieldDeclaration newFieldDeclaration(final Type type, final VariableDeclarationFragment fragment) {
		FieldDeclaration fd= ast.newFieldDeclaration(fragment);
		fd.setType(type);
		return fd;
	}

	/**
	 * Builds a new {@link VariableDeclarationFragment} instance.
	 *
	 * @param varName the declared variable name
	 * @return a new variable declaration fragment
	 */
	public VariableDeclarationFragment newVariableDeclarationFragment(final SimpleName varName) {
		VariableDeclarationFragment fragment= ast.newVariableDeclarationFragment();
		fragment.setName(varName);
		return fragment;
	}

	/**
	 * Builds a new {@link VariableDeclarationFragment} instance.
	 *
	 * @param varName     the declared variable name
	 * @param initializer the variable initializer
	 * @return a new variable declaration fragment
	 */
	public VariableDeclarationFragment newVariableDeclarationFragment(final SimpleName varName, final Expression initializer) {
		VariableDeclarationFragment fragment= ast.newVariableDeclarationFragment();
		fragment.setName(varName);
		fragment.setInitializer(initializer);
		return fragment;
	}

	/**
	 * Helper to create a list of modifiers.
	 *
	 * @param modifiers the list of modifiers
	 * @return a new list of modifiers
	 */
	public List<IExtendedModifier> extendedModifiers(final IExtendedModifier... modifiers) {
		return Arrays.asList(modifiers);
	}

	/**
	 * Builds a new {@link FieldAccess} instance.
	 *
	 * @param expression the expression on which the field is accessed
	 * @param fieldName  the field name being accessed
	 * @return a new single field access
	 */
	public FieldAccess newFieldAccess(final Expression expression, final SimpleName fieldName) {
		FieldAccess fa= getAST().newFieldAccess();
		fa.setExpression(expression);
		fa.setName(fieldName);
		return fa;
	}

	/**
	 * Builds a new {@link Modifier} with keyword {@code final}.
	 *
	 * @return a {@code final} modifier
	 */
	public Modifier final0() {
		return ast.newModifier(ModifierKeyword.FINAL_KEYWORD);
	}

	/**
	 * Builds a new {@link SingleVariableDeclaration} instance.
	 *
	 * @param varName the name of the variable being declared
	 * @param type    the type of the variable being declared
	 * @return a new single variable declaration
	 */
	public SingleVariableDeclaration newSingleVariableDeclaration(final String varName, final Type type) {
		SingleVariableDeclaration svd= ast.newSingleVariableDeclaration();
		svd.setName(newSimpleName(varName));
		svd.setType(type);
		return svd;
	}

	/**
	 * Builds a new {@link DoStatement} instance.
	 *
	 * @param condition the while condition
	 * @param statement the statement of the loop
	 * @return a new do statement
	 */
	public DoStatement newDoStatement(final Expression condition, final Statement statement) {
		DoStatement ds= ast.newDoStatement();
		ds.setExpression(condition);
		ds.setBody(statement);
		return ds;
	}

	/**
	 * Builds a new {@link IfStatement} instance.
	 *
	 * @return a new if statement
	 */
	public IfStatement newIfStatement() {
		return ast.newIfStatement();
	}

	/**
	 * Builds a new {@link ImportDeclaration} instance.
	 *
	 * @param name the if name
	 * @return a new if statement
	 */
	public ImportDeclaration newImportDeclaration(final Name name) {
		ImportDeclaration id= ast.newImportDeclaration();
		id.setName(name);
		id.setStatic(false);
		id.setOnDemand(false);
		return id;
	}

	/**
	 * Builds a new {@link PostfixExpression} instance.
	 *
	 * @param operand the operand
	 * @param operator    the post operator
	 * @return a new post expression
	 */
	public PostfixExpression newPostfixExpression(final Expression operand, final PostfixExpression.Operator operator) {
		PostfixExpression pe= ast.newPostfixExpression();
		pe.setOperand(operand);
		pe.setOperator(operator);

		return pe;
	}

	/**
	 * Builds a new {@link InfixExpression} instance.
	 *
	 * @param operator    the infix operator
	 * @param allOperands the operands
	 * @return a new infix expression
	 */
	public InfixExpression newInfixExpression(final InfixExpression.Operator operator, final Collection<? extends Expression> allOperands) {
		if (allOperands.size() < 2) {
			throw new IllegalArgumentException(null, "Not enough operands for an infix expression: " //$NON-NLS-1$
					+ "needed at least 2, but got " + allOperands.size()); //$NON-NLS-1$
		}
		Iterator<? extends Expression> it= allOperands.iterator();
		InfixExpression infixExpression= ast.newInfixExpression();
		infixExpression.setLeftOperand(it.next());
		infixExpression.setOperator(operator);
		infixExpression.setRightOperand(it.next());
		while (it.hasNext()) {
			infixExpression.extendedOperands().add(it.next());
		}

		return infixExpression;
	}

	/**
	 * Builds a new {@link ConditionalExpression} instance.
	 *
	 * @param mainExpression the main expression
	 * @param thenExpression the evaluated expression if the main expression is true
	 * @param elseExpression the evaluated expression if the main expression is
	 *                       false
	 * @return a new conditional expression
	 */
	public ConditionalExpression newConditionalExpression(final Expression mainExpression, final Expression thenExpression,
			final Expression elseExpression) {
		ConditionalExpression ce= ast.newConditionalExpression();
		ce.setExpression(mainExpression);
		ce.setThenExpression(thenExpression);
		ce.setElseExpression(elseExpression);
		return ce;
	}

	/**
	 * Builds a new {@link InfixExpression} instance.
	 *
	 * @param leftOperand      the left operand
	 * @param operator         the infix operator
	 * @param rightOperand     the right operand
	 * @param extendedOperands the extended operands
	 * @return a new infix expression
	 */
	public InfixExpression newInfixExpression(final Expression leftOperand, final InfixExpression.Operator operator, final Expression rightOperand,
			final Expression... extendedOperands) {
		InfixExpression infixExpression= ast.newInfixExpression();
		infixExpression.setLeftOperand(leftOperand);
		infixExpression.setOperator(operator);
		infixExpression.setRightOperand(rightOperand);
		Collections.addAll(infixExpression.extendedOperands(), extendedOperands);
		return infixExpression;
	}

	/**
	 * Builds a new {@link NumberLiteral} instance.
	 *
	 * @param intValue the number literal value
	 * @return a new number literal
	 */
	public NumberLiteral newNumberLiteral(final int intValue) {
		return ast.newNumberLiteral(Integer.toString(intValue));
	}

	/**
	 * Builds a new {@link MethodInvocation} instance.
	 *
	 * @param expression the method invocation expression
	 * @param methodName the name of the invoked method
	 * @param arguments  the arguments for the method invocation
	 * @return a new method invocation
	 */
	public MethodInvocation newMethodInvocation(final String expression, final String methodName, final Expression... arguments) {
		return newMethodInvocation(newName(this, expression), methodName, arguments);
	}

	/**
	 * Builds a new {@link MethodInvocation} instance.
	 *
	 * @param expression the method invocation expression
	 * @param methodName the name of the invoked method
	 * @param arguments  the arguments for the method invocation
	 * @return a new method invocation
	 */
	public MethodInvocation newMethodInvocation(final Expression expression, final String methodName, final Expression... arguments) {
		MethodInvocation methodInvocation= newMethodInvocation(methodName, arguments);
		methodInvocation.setExpression(expression);
		return methodInvocation;
	}

	/**
	 * Builds a new {@link MethodInvocation} instance.
	 *
	 * @param methodName the name of the invoked method
	 * @param arguments  the arguments for the method invocation
	 * @return a new method invocation
	 */
	public MethodInvocation newMethodInvocation(final String methodName, final Expression... arguments) {
		MethodInvocation methodInvocation= ast.newMethodInvocation();
		methodInvocation.setName(ast.newSimpleName(methodName));
		addAll(methodInvocation.arguments(), arguments);
		return methodInvocation;
	}

	/**
	 * Builds a new {@link MethodInvocation} instance.
	 *
	 * @param <E>        the arguments type
	 * @param expression the method invocation expression
	 * @param methodName the name of the invoked method
	 * @param arguments  the arguments for the method invocation
	 * @return a new method invocation
	 */
	public <E extends Expression> MethodInvocation newMethodInvocation(final Expression expression, final String methodName, final List<E> arguments) {
		MethodInvocation methodInvocation= ast.newMethodInvocation();
		methodInvocation.setExpression(expression);
		methodInvocation.setName(ast.newSimpleName(methodName));
		addAll(methodInvocation, arguments);
		return methodInvocation;
	}

	private boolean isEmptyRangeCopy(final ASTNode... nodes) {
		return nodes.length == 1 && nodes[0] == null;
	}

	private <E extends ASTNode> boolean isEmptyRangeCopy(final List<E> nodes) {
		return nodes.size() == 1 && nodes.get(0) == null;
	}

	/**
	 * Builds a new {@link CharacterLiteral} instance.
	 *
	 * @return a new character literal
	 */
	public CharacterLiteral newCharacterLiteral() {
		return ast.newCharacterLiteral();
	}

	/**
	 * Builds a new {@link LambdaExpression} instance.
	 *
	 * @return a new lambda expression
	 */
	public LambdaExpression newLambdaExpression() {
		return ast.newLambdaExpression();
	}

	/**
	 * Builds a new {@link ExpressionMethodReference} instance.
	 *
	 * @return a new expression method reference
	 */
	public ExpressionMethodReference newExpressionMethodReference() {
		return ast.newExpressionMethodReference();
	}

	/**
	 * Builds a new {@link TypeMethodReference} instance.
	 *
	 * @return a new type method reference
	 */
	public TypeMethodReference newTypeMethodReference() {
		return ast.newTypeMethodReference();
	}

	/**
	 * Builds a new {@link CreationReference} instance.
	 *
	 * @return a new creation reference
	 */
	public CreationReference newCreationReference() {
		return ast.newCreationReference();
	}

	/**
	 * Builds a new {@link SuperMethodReference} instance.
	 *
	 * @return a new super method reference
	 */
	public SuperMethodReference newSuperMethodReference() {
		return ast.newSuperMethodReference();
	}

	/**
	 * Returns a copy of the provided {@link ASTNode}.
	 *
	 * @param <T>        the actual node type
	 * @param nodeToCopy the node to copy
	 * @return a copy of the node
	 */
	public <T extends ASTNode> T createCopyTarget(final T nodeToCopy) {
		if (nodeToCopy.getNodeType() == ASTNode.ARRAY_TYPE) {
			return (T) copyType((Type) nodeToCopy);
		}
		if (isValidInCurrentAST(nodeToCopy)) {
			return rewrite.createCopyTarget(nodeToCopy);
		}

		return copySubtree(nodeToCopy);
	}

	private <T extends ASTNode> T createMoveTarget(final T nodeToMove) {
		return ASTNodes.createMoveTarget(rewrite, nodeToMove);
	}

	/**
	 * Builds a new {@link Name} instance. If only a single name is provided then a
	 * {@link SimpleName} is returned, if several names are provided then a
	 * {@link QualifiedName} is built.
	 *
	 * @param astNodeFactory the AST node factory
	 * @param names the qualified or simple name
	 * @return a new name
	 * @throws IllegalArgumentException if no names are provided
	 */
	public static Name newName(final ASTNodeFactory astNodeFactory, final String... names) {
		if (names.length == 0) {
			throw new IllegalArgumentException(null, "Expected at least one name, but was given 0 names"); //$NON-NLS-1$
		}

		if (names.length == 1) {
			String[] simpleNames= names[0].split("\\."); //$NON-NLS-1$
			if (simpleNames.length == 1) {
				return astNodeFactory.newSimpleName(simpleNames[0]);
			}

			return astNodeFactory.ast.newName(simpleNames);
		}

		return astNodeFactory.ast.newName(names);
	}

	/**
	 * Builds a new {@link SimpleName} instance.
	 *
	 * @param simpleName the simple name
	 * @return a new simple name
	 */
	public SimpleName newSimpleName(final String simpleName) {
		return ast.newSimpleName(simpleName);
	}

	/**
	 * Builds a new {@link ClassInstanceCreation} instance.
	 *
	 * @param typeName  the instantiated type name
	 * @param arguments the constructor invocation arguments
	 * @return a new class instance creation
	 */
	public ClassInstanceCreation newClassInstanceCreation(final String typeName, final Expression... arguments) {
		ClassInstanceCreation cic= ast.newClassInstanceCreation();
		cic.setType(simpleType(typeName));
		addAll(cic.arguments(), arguments);
		return cic;
	}

	/**
	 * Builds a new {@link ClassInstanceCreation} instance.
	 *
	 * @param type      the instantiated type
	 * @param arguments the constructor invocation arguments
	 * @return a new class instance creation
	 */
	public ClassInstanceCreation newClassInstanceCreation(final Type type, final Expression... arguments) {
		ClassInstanceCreation cic= ast.newClassInstanceCreation();
		cic.setType(type);
		addAll(cic.arguments(), arguments);
		return cic;
	}

	private <T extends ASTNode> void addAll(final List<T> whereToAdd, final T... toAdd) {
		if (!isEmptyRangeCopy(toAdd)) {
			Collections.addAll(whereToAdd, toAdd);
		}
	}

	private <E extends Expression> void addAll(final MethodInvocation methodInvocation, final List<E> arguments) {
		if (!isEmptyRangeCopy(arguments)) {
			methodInvocation.arguments().addAll(arguments);
		}
	}

	/**
	 * Builds a new {@link ArrayCreation} instance.
	 *
	 * @param arrayType        the array type
	 * @param arrayInitializer the array initializer
	 * @return a new array creation instance
	 */
	public ArrayCreation newArrayCreation(final ArrayType arrayType, final ArrayInitializer arrayInitializer) {
		ArrayCreation ac= ast.newArrayCreation();
		ac.setType(arrayType);
		ac.setInitializer(arrayInitializer);
		return ac;
	}

	/**
	 * Builds a new {@link PrefixExpression} instance using the not operator ('!').
	 *
	 * @param expression the expression to negate
	 * @return a new prefix expression
	 */
	public Expression not(final Expression expression) {
		return newPrefixExpression(PrefixExpression.Operator.NOT, parenthesizeIfNeeded(expression));
	}

	/**
	 * Negates the provided expression and applies the provided copy operation on
	 * the returned expression:
	 *
	 * isValid                        =>  !isValid
	 * !isValid                       =>  isValid
	 * true                           =>  false
	 * false                          =>  true
	 * i > 0                          =>  i <= 0
	 * isValid || isEnabled           =>  !isValid && !isEnabled
	 * !isValid || !isEnabled         =>  isValid && isEnabled
	 * isValid ? (i > 0) : !isEnabled =>  isValid ? (i <= 0) : isEnabled
	 * @param booleanExpression the expression to negate
	 * @param isMove False if the returned nodes need to be new nodes
	 *
	 * @return the negated expression, as move or copy
	 */
	public Expression negate(final Expression booleanExpression, final boolean isMove) {
		Expression unparenthesedExpression= ASTNodes.getUnparenthesedExpression(booleanExpression);

		if (unparenthesedExpression instanceof PrefixExpression) {
			PrefixExpression prefixExpression= (PrefixExpression) unparenthesedExpression;

			if (ASTNodes.hasOperator(prefixExpression, PrefixExpression.Operator.NOT)) {
				Expression otherExpression= prefixExpression.getOperand();
				PrefixExpression otherPrefixExpression= ASTNodes.as(otherExpression, PrefixExpression.class);

				if (otherPrefixExpression != null && ASTNodes.hasOperator(otherPrefixExpression, PrefixExpression.Operator.NOT)) {
					return negate(otherPrefixExpression.getOperand(), isMove);
				}

				return isMove ? createMoveTarget(otherExpression) : createCopyTarget(otherExpression);
			}
		} else if (unparenthesedExpression instanceof InfixExpression) {
			InfixExpression booleanOperation= (InfixExpression) unparenthesedExpression;
			InfixExpression.Operator negatedOperator= ASTNodes.negatedInfixOperator(booleanOperation.getOperator());

			if (negatedOperator != null) {
				return getNegatedOperation(booleanOperation, negatedOperator, isMove);
			}
		} else if (unparenthesedExpression instanceof ConditionalExpression) {
			ConditionalExpression aConditionalExpression= (ConditionalExpression) unparenthesedExpression;

			ConditionalExpression newConditionalExpression= newConditionalExpression(isMove ? createMoveTarget(aConditionalExpression.getExpression()) : createCopyTarget(aConditionalExpression.getExpression()),
					negate(aConditionalExpression.getThenExpression(), isMove),
					negate(aConditionalExpression.getElseExpression(), isMove));
			return newConditionalExpression;
		} else {
			Boolean constant= ASTNodes.getBooleanLiteral(unparenthesedExpression);

			if (constant != null) {
				return newBooleanLiteral(!constant.booleanValue());
			}
		}

		if (isMove) {
			return not(createMoveTarget(unparenthesedExpression));
		}

		return not(createCopyTarget(unparenthesedExpression));
	}

	private Expression getNegatedOperation(final InfixExpression booleanOperation, final InfixExpression.Operator negatedOperator, final boolean isMove) {
		List<Expression> allOperands= ASTNodes.allOperands(booleanOperation);
		List<Expression> allTargetOperands;

		if (ASTNodes.hasOperator(booleanOperation, InfixExpression.Operator.CONDITIONAL_AND, InfixExpression.Operator.CONDITIONAL_OR, InfixExpression.Operator.AND,
				InfixExpression.Operator.OR)) {
			allTargetOperands= new ArrayList<>(allOperands.size());

			for (Expression booleanOperand : allOperands) {
				Expression negatedOperand= negate(booleanOperand, isMove);

				if (negatedOperand != null) {
					allTargetOperands.add(negatedOperand);
				} else {
					PrefixExpression prefixExpression= newPrefixExpression(PrefixExpression.Operator.NOT, isMove ? createMoveTarget(booleanOperand) : createCopyTarget(booleanOperand));

					allTargetOperands.add(prefixExpression);
				}
			}
		} else {
			allTargetOperands= new ArrayList<>(allOperands.size());

			if (isMove) {
				for (Expression anOperand : allOperands) {
					allTargetOperands.add(createMoveTarget(anOperand));
				}
			} else {
				for (Expression anOperand : allOperands) {
					allTargetOperands.add(createCopyTarget(anOperand));
				}
			}
		}

		return newInfixExpression(negatedOperator, allTargetOperands);
	}

	/**
	 * Builds a new {@link NumberLiteral} instance.
	 *
	 * @param s the number literal value
	 * @return a new number literal
	 */
	public NumberLiteral newNumberLiteral(final String s) {
		return ast.newNumberLiteral(s);
	}

	/**
	 * Builds a new {@link ParenthesizedExpression} instance.
	 *
	 * @param expression the expression to wrap with parentheses
	 * @return a new parenthesized expression
	 */
	public ParenthesizedExpression newParenthesizedExpression(final Expression expression) {
		ParenthesizedExpression pe= ast.newParenthesizedExpression();
		pe.setExpression(expression);
		return pe;
	}

	private PrefixExpression newPrefixExpression(final PrefixExpression.Operator operator, final Expression operand) {
		PrefixExpression pe= ast.newPrefixExpression();
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
	public ReturnStatement newReturnStatement(final Expression expression) {
		ReturnStatement rs= ast.newReturnStatement();
		rs.setExpression(expression);
		return rs;
	}

	/**
	 * Builds a new {@link MarkerAnnotation} instance.
	 *
	 * @param typeName the annotation type name
	 * @return a new marker annotation
	 */
	public MarkerAnnotation newMarkerAnnotation(final Name typeName) {
		MarkerAnnotation ma= ast.newMarkerAnnotation();
		ma.setTypeName(typeName);
		return ma;
	}

	/**
	 * Builds a new {@link SingleMemberAnnotation} instance.
	 *
	 * @param typeName the annotation type name
	 * @param value    the annotation single value
	 * @return a new single member annotation
	 */
	public SingleMemberAnnotation newSingleMemberAnnotation(final Name typeName, final Expression value) {
		SingleMemberAnnotation sma= ast.newSingleMemberAnnotation();
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
	public StringLiteral newStringLiteral(final String s) {
		StringLiteral sl= ast.newStringLiteral();
		sl.setLiteralValue(s);
		return sl;
	}

	/**
	 * Builds a new {@link SwitchStatement} instance.
	 *
	 * @param expression the switch expression
	 * @return a new switch statement
	 */
	public SwitchStatement newSwitchStatement(final Expression expression) {
		SwitchStatement ss= ast.newSwitchStatement();
		ss.setExpression(expression);
		return ss;
	}

	/**
	 * Builds a new {@link ThisExpression} instance.
	 *
	 * @return a new this expression
	 */
	public ThisExpression newThisExpression() {
		return ast.newThisExpression();
	}

	/**
	 * Builds a new {@link ThrowStatement} instance.
	 *
	 * @param expression the expression to throw
	 * @return a new throw statement
	 */
	public ThrowStatement newThrowStatement(final Expression expression) {
		ThrowStatement throwS= ast.newThrowStatement();
		throwS.setExpression(expression);
		return throwS;
	}

	/**
	 * Builds a new {@link ExpressionStatement} instance.
	 *
	 * @param expression the expression to transform into a statement
	 * @return a new expression statement
	 */
	public ExpressionStatement newExpressionStatement(final Expression expression) {
		return ast.newExpressionStatement(expression);
	}

	/**
	 * Builds a new {@link TryStatement} instance.
	 *
	 * @param body         the try body
	 * @param catchClauses the catch clauses for the try
	 * @return a new try statement
	 */
	public TryStatement newTryStatement(final Block body, final CatchClause... catchClauses) {
		TryStatement tryS= ast.newTryStatement();
		tryS.setBody(body);
		addAll(tryS.catchClauses(), catchClauses);
		return tryS;
	}

	/**
	 * Builds a new {@link Statement} instance which is basically a newline.
	 *
	 * @return a newline statement
	 */
	public Statement newlinePlaceholder() {
		return (Statement) rewrite.getRewrite().createStringPlaceholder("\n", ASTNode.EMPTY_STATEMENT); //$NON-NLS-1$
	}

	/**
	 * Builds a new {@link ASTNode} instance containing raw comment source code.
	 *
	 * @param text the raw source code of comment
	 *
	 * @return a newline statement
	 */
	public ASTNode rawComment(String text) {
		return rewrite.getRewrite().createStringPlaceholder(text, ASTNode.EMPTY_STATEMENT);
	}

	/**
	 * Builds a new {@link Modifier} with keyword {@code public}.
	 *
	 * @return a {@code public} modifier
	 */
	public Modifier public0() {
		return ast.newModifier(ModifierKeyword.PUBLIC_KEYWORD);
	}

	/**
	 * Builds a new {@link Modifier} with keyword {@code private}.
	 *
	 * @return a {@code private} modifier
	 */
	public Modifier private0() {
		return ast.newModifier(ModifierKeyword.PRIVATE_KEYWORD);
	}

	/**
	 * Builds a new {@link Modifier} with keyword {@code protected}.
	 *
	 * @return a {@code protected} modifier
	 */
	public Modifier protected0() {
		return ast.newModifier(ModifierKeyword.PROTECTED_KEYWORD);
	}

	/**
	 * Builds a new {@link Modifier} with keyword {@code static}.
	 *
	 * @return a {@code static} modifier
	 */
	public Modifier static0() {
		return ast.newModifier(ModifierKeyword.STATIC_KEYWORD);
	}

	/**
	 * Builds a new super method invocation.
	 *
	 * @param methodName name of the method to be invoked
	 * @return expression with a method invocation
	 */
	public Expression newSuperMethodInvocation(final String methodName) {
		SuperMethodInvocation smi= ast.newSuperMethodInvocation();
		smi.setName(newSimpleName(methodName));
		return smi;
	}

	/**
	 * Builds a new {@link MethodDeclaration} node.
	 *
	 * @param modifiers  list of modifiers of the method
	 * @param methodName the method name
	 * @param parameters list of parameters
	 * @param block      the block of the method
	 * @return a new method declaration
	 */
	public MethodDeclaration newMethodDeclaration(final List<IExtendedModifier> modifiers, final String methodName,
			final List<SingleVariableDeclaration> parameters, final Block block) {
		MethodDeclaration md= ast.newMethodDeclaration();
		md.modifiers().addAll(modifiers);
		md.setName(newSimpleName(methodName));
		md.parameters().addAll(parameters);
		md.setBody(block);
		return md;
	}

	/**
	 * Builds a new {@link NullLiteral}.
	 *
	 * @return a {@code null} literal
	 */
	public NullLiteral newNullLiteral() {
		return ast.newNullLiteral();
	}

	/**
	 * Parenthesizes the provided expression if its type requires it.
	 *
	 * @param expression the expression to conditionally return parenthesized
	 * @return the parenthesized expression of the provided expression to return or
	 *         this expression itself
	 */
	public Expression parenthesizeIfNeeded(final Expression expression) {
		switch (expression.getNodeType()) {
		case ASTNode.ANNOTATION_TYPE_DECLARATION:
		case ASTNode.ANNOTATION_TYPE_MEMBER_DECLARATION:
		case ASTNode.ANONYMOUS_CLASS_DECLARATION:
		case ASTNode.ARRAY_ACCESS:
		case ASTNode.ARRAY_CREATION:
		case ASTNode.ARRAY_INITIALIZER:
		case ASTNode.BOOLEAN_LITERAL:
		case ASTNode.CHARACTER_LITERAL:
		case ASTNode.CLASS_INSTANCE_CREATION:
		case ASTNode.CREATION_REFERENCE:
		case ASTNode.EXPRESSION_METHOD_REFERENCE:
		case ASTNode.FIELD_ACCESS:
		case ASTNode.MEMBER_REF:
		case ASTNode.METHOD_INVOCATION:
		case ASTNode.METHOD_REF:
		case ASTNode.NULL_LITERAL:
		case ASTNode.NUMBER_LITERAL:
		case ASTNode.PARENTHESIZED_EXPRESSION:
		case ASTNode.POSTFIX_EXPRESSION:
		case ASTNode.PREFIX_EXPRESSION:
		case ASTNode.QUALIFIED_NAME:
		case ASTNode.SIMPLE_NAME:
		case ASTNode.STRING_LITERAL:
		case ASTNode.SUPER_FIELD_ACCESS:
		case ASTNode.SUPER_METHOD_INVOCATION:
		case ASTNode.SUPER_METHOD_REFERENCE:
		case ASTNode.THIS_EXPRESSION:
		case ASTNode.TYPE_LITERAL:
		case ASTNode.TYPE_METHOD_REFERENCE:
		case ASTNode.VARIABLE_DECLARATION_EXPRESSION:
			return expression;

		default:
			return newParenthesizedExpression(expression);
		}
	}
}

/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice TIERCELIN - initial API and implementation
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Bindings;
import org.autorefactor.jdt.internal.corext.dom.InterruptibleVisitor;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SimpleType;
import org.eclipse.jdt.core.dom.SuperFieldAccess;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public abstract class AbstractPrimitiveRatherThanWrapperCleanUp extends AbstractCleanUpRule {
	/**
	 * Get the primitive type name.
	 *
	 * @return the primitive type name.
	 */
	public abstract String getPrimitiveTypeName();

	/**
	 * Get the literal class.
	 *
	 * @return the literal class.
	 */
	public abstract Class<? extends Expression> getLiteralClass();

	/**
	 * Get the wrapper fully qualified name.
	 *
	 * @return the wrapper fully qualified name.
	 */
	public String getWrapperFullyQualifiedName() {
		return Bindings.getBoxedTypeName(getPrimitiveTypeName());
	}

	/**
	 * Get the prefix in safe operators.
	 *
	 * @return the prefix in safe operators.
	 */
	public List<PrefixExpression.Operator> getPrefixInSafeOperators() {
		return new ArrayList<>(0);
	}

	/**
	 * Get the Infix In Safe Operators.
	 *
	 * @return the Infix In Safe Operators.
	 */
	public List<InfixExpression.Operator> getInfixInSafeOperators() {
		return Collections.emptyList();
	}

	/**
	 * Get the postfix in safe operators.
	 *
	 * @return the postfix in safe operators.
	 */
	public List<PostfixExpression.Operator> getPostfixInSafeOperators() {
		return Collections.emptyList();
	}

	/**
	 * Get the prefix out safe operators.
	 *
	 * @return the prefix out safe operators.
	 */
	public List<PrefixExpression.Operator> getPrefixOutSafeOperators() {
		return Collections.emptyList();
	}

	/**
	 * Get the infix out safe operators.
	 *
	 * @return the infix out safe operators.
	 */
	public List<InfixExpression.Operator> getInfixOutSafeOperators() {
		return Collections.emptyList();
	}

	/**
	 * Get the postfix out safe operators.
	 *
	 * @return the postfix out safe operators.
	 */
	public List<PostfixExpression.Operator> getPostfixOutSafeOperators() {
		return Collections.emptyList();
	}

	/**
	 * Get the assignment out safe operators.
	 *
	 * @return the assignment out safe operators.
	 */
	public List<Assignment.Operator> getAssignmentOutSafeOperators() {
		return Collections.emptyList();
	}

	/**
	 * Get the safe in constants.
	 *
	 * @return the safe in constants.
	 */
	public String[] getSafeInConstants() {
		return new String[0];
	}

	/**
	 * True if the specific primitive is allowed.
	 *
	 * @param node The node
	 *
	 * @return True if the specific primitive is allowed.
	 */
	public boolean isSpecificPrimitiveAllowed(final ASTNode node) {
		return false;
	}

	@Override
	public boolean visit(final VariableDeclarationStatement visited) {
		VariableDeclarationFragment fragment= ASTNodes.getUniqueFragment(visited);

		if (fragment != null
				&& (fragment.resolveBinding() != null && ASTNodes.hasType(fragment.resolveBinding().getType(), getWrapperFullyQualifiedName())
						|| visited.getType() != null && visited.getType().resolveBinding() != null && ASTNodes.hasType(visited.getType().resolveBinding(), getWrapperFullyQualifiedName()))
				&& fragment.getInitializer() != null
				&& isNotNull(fragment.getInitializer())) {
			VarOccurrenceVisitor varOccurrenceVisitor= new VarOccurrenceVisitor(fragment);
			Block parentBlock= ASTNodes.getTypedAncestor(fragment, Block.class);

			if (parentBlock != null) {
				varOccurrenceVisitor.traverseNodeInterruptibly(parentBlock);

				if (varOccurrenceVisitor.isPrimitiveAllowed() && varOccurrenceVisitor.getAutoBoxingCount() < 2) {
					refactorWrapper(visited, fragment.getInitializer(), varOccurrenceVisitor.getToStringMethods(), varOccurrenceVisitor.getCompareToMethods(), varOccurrenceVisitor.getPrimitiveValueMethods());
					return false;
				}
			}
		}

		return true;
	}

	private void refactorWrapper(
			final VariableDeclarationStatement visited,
			final Expression initializer,
			final List<MethodInvocation> toStringMethods,
			final List<MethodInvocation> compareToMethods,
			final List<MethodInvocation> primitiveValueMethods) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(""); //$NON-NLS-1$

		String parsingMethodName= getParsingMethodName(getWrapperFullyQualifiedName());

		if (initializer instanceof MethodInvocation) {
			MethodInvocation methodInvocation= (MethodInvocation) initializer;

			if (ASTNodes.usesGivenSignature(methodInvocation, getWrapperFullyQualifiedName(), "valueOf", getPrimitiveTypeName())) { //$NON-NLS-1$
				rewrite.replace(methodInvocation, ASTNodes.createMoveTarget(rewrite, (Expression) methodInvocation.arguments().get(0)), group);
			}

			if (ASTNodes.usesGivenSignature(methodInvocation, getWrapperFullyQualifiedName(), "valueOf", String.class.getCanonicalName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(methodInvocation, getWrapperFullyQualifiedName(), "valueOf", String.class.getCanonicalName(), int.class.getSimpleName())) { //$NON-NLS-1$
				rewrite.set(methodInvocation, MethodInvocation.NAME_PROPERTY, ast.newSimpleName(parsingMethodName), group);
			}
		} else if (initializer instanceof ClassInstanceCreation) {
			ClassInstanceCreation classInstanceCreation= (ClassInstanceCreation) initializer;
			List<Expression> classInstanceCreationArguments= classInstanceCreation.arguments();

			if (classInstanceCreationArguments.size() == 1
					&& parsingMethodName != null
					&& !Character.class.getCanonicalName().equals(getWrapperFullyQualifiedName())
					&& ASTNodes.hasType(classInstanceCreation, getWrapperFullyQualifiedName())) {
				Expression arg0= classInstanceCreationArguments.get(0);

				if (ASTNodes.hasType(arg0, String.class.getCanonicalName())) {
					MethodInvocation newMethodInvocation= ast.newMethodInvocation();
					newMethodInvocation.setExpression(rewrite.createCopyTarget(((SimpleType) visited.getType()).getName()));
					newMethodInvocation.setName(ast.newSimpleName(parsingMethodName));
					newMethodInvocation.arguments().add(ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(arg0)));

					ASTNodes.replaceButKeepComment(rewrite, initializer, newMethodInvocation, group);
				}
			}
		}

		for (MethodInvocation primitiveValueMethod : primitiveValueMethods) {
			rewrite.replace(primitiveValueMethod, ASTNodes.createMoveTarget(rewrite, primitiveValueMethod.getExpression()), group);
		}

		for (MethodInvocation toStringMethod : toStringMethods) {
			Type wrapperType= rewrite.createCopyTarget(visited.getType());

			rewrite.insertFirst(toStringMethod, MethodInvocation.ARGUMENTS_PROPERTY, ASTNodes.createMoveTarget(rewrite, toStringMethod.getExpression()), group);
			rewrite.set(toStringMethod, MethodInvocation.EXPRESSION_PROPERTY, wrapperType, group);
		}

		for (MethodInvocation compareToMethod : compareToMethods) {
			Type wrapperType= rewrite.createCopyTarget(visited.getType());

			rewrite.insertFirst(compareToMethod, MethodInvocation.ARGUMENTS_PROPERTY, ASTNodes.createMoveTarget(rewrite, compareToMethod.getExpression()), group);
			rewrite.set(compareToMethod, MethodInvocation.EXPRESSION_PROPERTY, wrapperType, group);
			rewrite.replace(compareToMethod.getName(), ast.newSimpleName("compare"), group); //$NON-NLS-1$
		}

		Type primitiveType= ast.type(getPrimitiveTypeName());

		ASTNodes.replaceButKeepComment(rewrite, visited.getType(), primitiveType, group);
	}

	private boolean isNotNull(final Expression expression) {
		if (expression instanceof ParenthesizedExpression) {
			ParenthesizedExpression parenthesizedExpression= (ParenthesizedExpression) expression;
			return isNotNull(parenthesizedExpression.getExpression());
		}

		if (expression instanceof ConditionalExpression) {
			ConditionalExpression prefixExpression= (ConditionalExpression) expression;
			return isNotNull(prefixExpression.getThenExpression()) && isNotNull(prefixExpression.getElseExpression());
		}

		if (getLiteralClass().equals(expression.getClass())) {
			return true;
		}

		if (expression instanceof QualifiedName) {
			QualifiedName qualifiedName= (QualifiedName) expression;
			return ASTNodes.hasType(qualifiedName.getQualifier(), getWrapperFullyQualifiedName())
					&& (ASTNodes.isField(qualifiedName, getWrapperFullyQualifiedName(), getSafeInConstants())
							|| ASTNodes.isField(qualifiedName, getPrimitiveTypeName(), getSafeInConstants()));
		}

		if (expression instanceof InfixExpression) {
			InfixExpression infixExpression= (InfixExpression) expression;
			return getInfixInSafeOperators().contains(infixExpression.getOperator());
		}

		if (expression instanceof PrefixExpression) {
			PrefixExpression prefixExpression= (PrefixExpression) expression;
			return getPrefixInSafeOperators().contains(prefixExpression.getOperator());
		}

		if (expression instanceof PostfixExpression) {
			PostfixExpression postfixExpression= (PostfixExpression) expression;
			return getPostfixInSafeOperators().contains(postfixExpression.getOperator());
		}

		if (expression instanceof CastExpression) {
			CastExpression castExpression= (CastExpression) expression;
			return ASTNodes.hasType(castExpression.getType().resolveBinding(), getPrimitiveTypeName())
					|| ASTNodes.hasType(castExpression.getType().resolveBinding(), getWrapperFullyQualifiedName())
							&& isNotNull(castExpression.getExpression());
		}

		if (expression instanceof MethodInvocation) {
			MethodInvocation methodInvocation= (MethodInvocation) expression;
			return ASTNodes.usesGivenSignature(methodInvocation, getWrapperFullyQualifiedName(), "valueOf", getPrimitiveTypeName()) //$NON-NLS-1$
					|| getParsingMethodName(getWrapperFullyQualifiedName()) != null
					&& (
							ASTNodes.usesGivenSignature(methodInvocation, getWrapperFullyQualifiedName(), "valueOf", String.class.getCanonicalName()) //$NON-NLS-1$
							|| ASTNodes.usesGivenSignature(methodInvocation, getWrapperFullyQualifiedName(), "valueOf", String.class.getCanonicalName(), int.class.getSimpleName()) //$NON-NLS-1$
							);
		}

		if (expression instanceof ClassInstanceCreation) {
			ClassInstanceCreation classInstanceCreation= (ClassInstanceCreation) expression;
			List<Expression> classInstanceCreationArguments= classInstanceCreation.arguments();

			if (classInstanceCreationArguments.size() == 1) {
				Expression arg0= classInstanceCreationArguments.get(0);

				return ASTNodes.hasType(arg0, String.class.getCanonicalName());
			}
		}

		return false;
	}

	private String getParsingMethodName(final String wrapperFullyQualifiedName) {
		if (Boolean.class.getCanonicalName().equals(wrapperFullyQualifiedName) && getJavaMinorVersion() >= 5) {
			return "parseBoolean"; //$NON-NLS-1$
		}

		if (Integer.class.getCanonicalName().equals(wrapperFullyQualifiedName)) {
			return "parseInt"; //$NON-NLS-1$
		}

		if (Long.class.getCanonicalName().equals(wrapperFullyQualifiedName)) {
			return "parseLong"; //$NON-NLS-1$
		}

		if (Double.class.getCanonicalName().equals(wrapperFullyQualifiedName) && getJavaMinorVersion() >= 2) {
			return "parseDouble"; //$NON-NLS-1$
		}

		if (Float.class.getCanonicalName().equals(wrapperFullyQualifiedName) && getJavaMinorVersion() >= 2) {
			return "parseFloat"; //$NON-NLS-1$
		}

		if (Short.class.getCanonicalName().equals(wrapperFullyQualifiedName)) {
			return "parseShort"; //$NON-NLS-1$
		}

		if (Byte.class.getCanonicalName().equals(wrapperFullyQualifiedName)) {
			return "parseByte"; //$NON-NLS-1$
		}

		return null;
	}

	private class VarOccurrenceVisitor extends InterruptibleVisitor {
		private final VariableDeclarationFragment varDecl;
		private final List<MethodInvocation> toStringMethods = new ArrayList<>();
		private final List<MethodInvocation> compareToMethods = new ArrayList<>();
		private final List<MethodInvocation> primitiveValueMethods = new ArrayList<>();
		private boolean isPrimitiveAllowed= true;
		private boolean isVarReturned;
		private int autoBoxingCount;

		public VarOccurrenceVisitor(final VariableDeclarationFragment var) {
			varDecl= var;
		}

		public boolean isPrimitiveAllowed() {
			return isPrimitiveAllowed;
		}

		public int getAutoBoxingCount() {
			return autoBoxingCount;
		}

		public List<MethodInvocation> getToStringMethods() {
			return toStringMethods;
		}

		public List<MethodInvocation> getCompareToMethods() {
			return compareToMethods;
		}

		public List<MethodInvocation> getPrimitiveValueMethods() {
			return primitiveValueMethods;
		}

		@Override
		public boolean visit(final SimpleName aVar) {
			if (isPrimitiveAllowed
					&& ASTNodes.isSameVariable(aVar, varDecl.getName())
					&& !aVar.getParent().equals(varDecl)) {
				isPrimitiveAllowed= isPrimitiveAllowed(aVar);

				if (!isPrimitiveAllowed) {
					return interruptVisit();
				}
			}

			return true;
		}

		private boolean isPrimitiveAllowed(final ASTNode node) {
			ASTNode parentNode= node.getParent();

			switch (parentNode.getNodeType()) {
			case ASTNode.PARENTHESIZED_EXPRESSION:
				return isPrimitiveAllowed(parentNode);

			case ASTNode.CAST_EXPRESSION:
				CastExpression castExpression= (CastExpression) parentNode;
				return ASTNodes.hasType(castExpression.getType().resolveBinding(), getPrimitiveTypeName());

			case ASTNode.ASSIGNMENT:
				Assignment assignment= (Assignment) parentNode;

				if (getAssignmentOutSafeOperators().contains(assignment.getOperator())) {
					return true;
				}

				if (assignment.getLeftHandSide().equals(node)) {
					return isNotNull(assignment.getRightHandSide());
				}

				if (assignment.getRightHandSide().equals(node)) {
					if (assignment.getLeftHandSide() instanceof Name) {
						return isOfType(((Name) assignment.getLeftHandSide()).resolveTypeBinding());
					}

					if (assignment.getLeftHandSide() instanceof FieldAccess) {
						return isOfType(((FieldAccess) assignment.getLeftHandSide()).resolveTypeBinding());
					}

					if (assignment.getLeftHandSide() instanceof SuperFieldAccess) {
						return isOfType(((SuperFieldAccess) assignment.getLeftHandSide()).resolveTypeBinding());
					}
				}

				return false;

			case ASTNode.VARIABLE_DECLARATION_FRAGMENT:
				VariableDeclarationFragment fragment= (VariableDeclarationFragment) parentNode;
				return node.getLocationInParent() == VariableDeclarationFragment.INITIALIZER_PROPERTY && isOfType(fragment.getName().resolveTypeBinding());

			case ASTNode.RETURN_STATEMENT:
				if (node.getLocationInParent() == ReturnStatement.EXPRESSION_PROPERTY) {
					MethodDeclaration method= ASTNodes.getTypedAncestor(parentNode, MethodDeclaration.class);

					if (method != null && method.getReturnType2() != null) {
						if (ASTNodes.hasType(method.getReturnType2().resolveBinding(), getPrimitiveTypeName())) {
							return true;
						}

						if (ASTNodes.hasType(method.getReturnType2().resolveBinding(), getWrapperFullyQualifiedName())) {
							if (!isVarReturned) {
								isVarReturned= true;
								autoBoxingCount++;
							}

							return true;
						}
					}
				}

				return false;

			case ASTNode.CONDITIONAL_EXPRESSION:
				return node.getLocationInParent() == ConditionalExpression.EXPRESSION_PROPERTY;

			case ASTNode.PREFIX_EXPRESSION:
				return getPrefixOutSafeOperators().contains(((PrefixExpression) parentNode).getOperator());

			case ASTNode.INFIX_EXPRESSION:
				return getInfixOutSafeOperators().contains(((InfixExpression) parentNode).getOperator());

			case ASTNode.POSTFIX_EXPRESSION:
				return getPostfixOutSafeOperators().contains(((PostfixExpression) parentNode).getOperator());

			case ASTNode.METHOD_INVOCATION:
				MethodInvocation methodInvocation= (MethodInvocation) parentNode;

				if (node.getLocationInParent() == MethodInvocation.EXPRESSION_PROPERTY) {
					if (ASTNodes.usesGivenSignature(methodInvocation, getWrapperFullyQualifiedName(), getPrimitiveTypeName() + "Value")) { //$NON-NLS-1$
						primitiveValueMethods.add(methodInvocation);
						return true;
					}

					if (ASTNodes.usesGivenSignature(methodInvocation, getWrapperFullyQualifiedName(), "toString")) { //$NON-NLS-1$
						toStringMethods.add(methodInvocation);
						return true;
					}

					if (ASTNodes.usesGivenSignature(methodInvocation, getWrapperFullyQualifiedName(), "compareTo", getWrapperFullyQualifiedName())) { //$NON-NLS-1$
						if (ASTNodes.hasType((Expression) methodInvocation.arguments().get(0), getWrapperFullyQualifiedName())) {
							autoBoxingCount++;
						}

						compareToMethods.add(methodInvocation);
						return true;
					}
				}

				break;

			default:
			}

			return isSpecificPrimitiveAllowed(node);
		}

		private boolean isOfType(final ITypeBinding resolveTypeBinding) {
			if (ASTNodes.hasType(resolveTypeBinding, getPrimitiveTypeName())) {
				return true;
			}

			if (ASTNodes.hasType(resolveTypeBinding, getWrapperFullyQualifiedName())) {
				autoBoxingCount++;
				return true;
			}

			return false;
		}
	}
}

/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - #199 Replace unnecessary Boolean constant on boolean assignment
 *                                        #200 Compile error when Float myFloat = new Float(doubleObject);
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

import java.util.List;
import java.util.Objects;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class PrimitiveWrapperCreationCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.PrimitiveWrapperCreationCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.PrimitiveWrapperCreationCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.PrimitiveWrapperCreationCleanUp_reason;
	}

	@Override
	public boolean visit(final MethodInvocation node) {
		if (node.getExpression() == null) {
			return true;
		}

		ITypeBinding destinationTypeBinding= ASTNodes.getTargetType(node);

		if (destinationTypeBinding != null && destinationTypeBinding.isPrimitive()
				&& "valueOf".equals(node.getName().getIdentifier())) { //$NON-NLS-1$
			if (ASTNodes.usesGivenSignature(node, Boolean.class.getCanonicalName(), "valueOf", boolean.class.getSimpleName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(node, Byte.class.getCanonicalName(), "valueOf", byte.class.getSimpleName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(node, Character.class.getCanonicalName(), "valueOf", char.class.getSimpleName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(node, Short.class.getCanonicalName(), "valueOf", short.class.getSimpleName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(node, Integer.class.getCanonicalName(), "valueOf", int.class.getSimpleName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(node, Long.class.getCanonicalName(), "valueOf", long.class.getSimpleName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(node, Float.class.getCanonicalName(), "valueOf", float.class.getSimpleName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(node, Double.class.getCanonicalName(), "valueOf", double.class.getSimpleName())) { //$NON-NLS-1$
				replaceWithTheSingleArgument(node);
				return false;
			}

			if (is(node, Byte.class.getCanonicalName())) {
				return replaceMethodName(node, "parseByte"); //$NON-NLS-1$
			}

			if (is(node, Short.class.getCanonicalName())) {
				return replaceMethodName(node, "parseShort"); //$NON-NLS-1$
			}

			if (is(node, Integer.class.getCanonicalName())) {
				return replaceMethodName(node, "parseInt"); //$NON-NLS-1$
			}

			if (is(node, Long.class.getCanonicalName())) {
				return replaceMethodName(node, "parseLong"); //$NON-NLS-1$
			}

			if (ASTNodes.usesGivenSignature(node, Boolean.class.getCanonicalName(), "valueOf", String.class.getCanonicalName())) { //$NON-NLS-1$
				return replaceMethodName(node, "parseBoolean"); //$NON-NLS-1$
			}

			if (is(node, Float.class.getCanonicalName())) {
				return replaceMethodName(node, "parseFloat"); //$NON-NLS-1$
			}

			if (is(node, Double.class.getCanonicalName())) {
				return replaceMethodName(node, "parseDouble"); //$NON-NLS-1$
			}
		}

		ITypeBinding typeBinding= node.getExpression().resolveTypeBinding();
		ClassInstanceCreation classInstanceCreation= ASTNodes.as(node.getExpression(), ClassInstanceCreation.class);

		if (typeBinding != null && classInstanceCreation != null) {
			List<?> cicArgs= classInstanceCreation.arguments();

			if (cicArgs.size() == 1) {
				Expression arg0= (Expression) cicArgs.get(0);

				if (node.arguments().isEmpty() && ASTNodes.hasType(arg0, String.class.getCanonicalName())) {
					String methodName= getMethodName(typeBinding.getQualifiedName(),
							node.getName().getIdentifier());

					if (methodName != null) {
						TextEditGroup group= new TextEditGroup(MultiFixMessages.PrimitiveWrapperCreationCleanUp_description);
						ASTNodes.replaceButKeepComment(cuRewrite.getASTRewrite(), node,
								newMethodInvocation(typeBinding.getName(), methodName, arg0), group);
						return false;
					}
				}
			}
		}

		return true;
	}

	private boolean is(final MethodInvocation node, final String declaringTypeQualifiedName) {
		return ASTNodes.usesGivenSignature(node, declaringTypeQualifiedName, "valueOf", String.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(node, declaringTypeQualifiedName, "valueOf", String.class.getCanonicalName(), int.class.getSimpleName()) //$NON-NLS-1$
						&& Objects.equals(10, ((Expression) node.arguments().get(1)).resolveConstantExpressionValue());
	}

	private boolean replaceMethodName(final MethodInvocation node, final String methodName) {
		SimpleName name= cuRewrite.getASTBuilder().newSimpleName(methodName);
		TextEditGroup group= new TextEditGroup(MultiFixMessages.PrimitiveWrapperCreationCleanUp_description);
		cuRewrite.getASTRewrite().set(node, MethodInvocation.NAME_PROPERTY, name, group);
		return false;
	}

	private void replaceWithTheSingleArgument(final MethodInvocation node) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.PrimitiveWrapperCreationCleanUp_description);
		ASTNodes.replaceButKeepComment(rewrite, node, ASTNodes.createMoveTarget(rewrite, (Expression) node.arguments().get(0)), group);
	}

	private String getMethodName(final String typeName, final String invokedMethodName) {
		if (Boolean.class.getCanonicalName().equals(typeName) && "booleanValue".equals(invokedMethodName)) { //$NON-NLS-1$
			return "valueOf"; //$NON-NLS-1$
		}

		if (Byte.class.getCanonicalName().equals(typeName) && "byteValue".equals(invokedMethodName)) { //$NON-NLS-1$
			return "parseByte"; //$NON-NLS-1$
		}

		if (Double.class.getCanonicalName().equals(typeName) && "doubleValue".equals(invokedMethodName)) { //$NON-NLS-1$
			return "parseDouble"; //$NON-NLS-1$
		}

		if (Float.class.getCanonicalName().equals(typeName) && "floatValue".equals(invokedMethodName)) { //$NON-NLS-1$
			return "parseFloat"; //$NON-NLS-1$
		}

		if (Long.class.getCanonicalName().equals(typeName) && "longValue".equals(invokedMethodName)) { //$NON-NLS-1$
			return "parseLong"; //$NON-NLS-1$
		}

		if (Short.class.getCanonicalName().equals(typeName) && "shortValue".equals(invokedMethodName)) { //$NON-NLS-1$
			return "parseShort"; //$NON-NLS-1$
		}

		if (Integer.class.getCanonicalName().equals(typeName) && "intValue".equals(invokedMethodName)) { //$NON-NLS-1$
			return "parseInt"; //$NON-NLS-1$
		}

		return null;
	}

	@Override
	public boolean visit(final ClassInstanceCreation node) {
		ITypeBinding typeBinding= node.getType().resolveBinding();
		@SuppressWarnings("unchecked")
		List<Expression> args= node.arguments();

		if (getJavaMinorVersion() >= 5 && args.size() == 1) {
			if (ASTNodes.hasType(typeBinding, Boolean.class.getCanonicalName(), Byte.class.getCanonicalName(), Character.class.getCanonicalName(), Double.class.getCanonicalName(),
					Long.class.getCanonicalName(), Short.class.getCanonicalName(), Integer.class.getCanonicalName())) {
				replaceWithValueOf(node, typeBinding);
				return false;
			}

			if (ASTNodes.hasType(typeBinding, Float.class.getCanonicalName())) {
				replaceFloatInstanceWithValueOf(node, typeBinding, args);
				return false;
			}
		}

		return true;
	}

	private void replaceFloatInstanceWithValueOf(final ClassInstanceCreation node, final ITypeBinding typeBinding,
			final List<Expression> args) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.PrimitiveWrapperCreationCleanUp_description);

		Expression arg0= args.get(0);

		if (ASTNodes.isPrimitive(arg0, double.class.getSimpleName())) {
			ASTNodes.replaceButKeepComment(rewrite, node,
					ast.newMethodInvocation(typeBinding.getName(), "valueOf", ast.newCastExpression(ast.type(float.class.getSimpleName()), ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(arg0)))), group); //$NON-NLS-1$
		} else if (ASTNodes.hasType(arg0, Double.class.getCanonicalName())) {
			ASTNodes.replaceButKeepComment(rewrite, node, ast.newMethodInvocation(ASTNodes.createMoveTarget(rewrite, arg0), "floatValue"), group); //$NON-NLS-1$
		} else {
			replaceWithValueOf(node, typeBinding);
		}
	}

	private void replaceWithValueOf(final ClassInstanceCreation node, final ITypeBinding typeBinding) {
		TextEditGroup group= new TextEditGroup(MultiFixMessages.PrimitiveWrapperCreationCleanUp_description);
		ASTNodes.replaceButKeepComment(cuRewrite.getASTRewrite(), node,
				newMethodInvocation(typeBinding.getName(), "valueOf", (Expression) node.arguments().get(0)), group); //$NON-NLS-1$
	}

	private MethodInvocation newMethodInvocation(final String typeName, final String methodName, final Expression arg) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		return ast.newMethodInvocation(typeName, methodName, ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(arg)));
	}
}

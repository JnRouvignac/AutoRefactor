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
	public boolean visit(final ClassInstanceCreation visited) {
		ITypeBinding typeBinding= visited.getType().resolveBinding();
		List<Expression> args= visited.arguments();

		if (getJavaMinorVersion() >= 5 && args.size() == 1) {
			Expression arg0= args.get(0);

			if (ASTNodes.hasType(typeBinding, Float.class.getCanonicalName())) {
				if (ASTNodes.hasType(arg0, double.class.getSimpleName())) {
					replaceFloatWithValueOf(visited, typeBinding, arg0);
					return false;
				}

				if (ASTNodes.hasType(arg0, Double.class.getCanonicalName())) {
					replaceFloatWithFloatValue(visited, arg0);
					return false;
				}
			}

			if (ASTNodes.hasType(typeBinding, Boolean.class.getCanonicalName(), Integer.class.getCanonicalName(), Long.class.getCanonicalName(), Double.class.getCanonicalName(),
					Short.class.getCanonicalName(), Float.class.getCanonicalName(), Byte.class.getCanonicalName(), Character.class.getCanonicalName())) {
				ITypeBinding destinationTypeBinding= ASTNodes.getTargetType(visited);

				if (destinationTypeBinding != null
						&& destinationTypeBinding.isPrimitive()) {
					replaceWithTheSingleArgument(visited);
					return false;
				}

				replaceWithValueOf(visited, typeBinding, arg0);
				return false;
			}
		}

		return true;
	}

	private void replaceFloatWithValueOf(final ClassInstanceCreation visited, final ITypeBinding typeBinding, final Expression arg0) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.PrimitiveWrapperCreationCleanUp_description);

		MethodInvocation newMethodInvocation= ast.newMethodInvocation(typeBinding.getName(), "valueOf", ast.newCastExpression(ast.type(float.class.getSimpleName()), ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(arg0)))); //$NON-NLS-1$
		ASTNodes.replaceButKeepComment(rewrite, visited, newMethodInvocation, group);
	}

	private void replaceFloatWithFloatValue(final ClassInstanceCreation visited, final Expression arg0) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.PrimitiveWrapperCreationCleanUp_description);

		MethodInvocation newMethodInvocation= ast.newMethodInvocation(ASTNodes.createMoveTarget(rewrite, arg0), "floatValue"); //$NON-NLS-1$
		ASTNodes.replaceButKeepComment(rewrite, visited, newMethodInvocation, group);
	}

	private void replaceWithTheSingleArgument(final ClassInstanceCreation visited) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.PrimitiveWrapperCreationCleanUp_description);

		ASTNodes.replaceButKeepComment(rewrite, visited, ASTNodes.createMoveTarget(rewrite, (Expression) visited.arguments().get(0)), group);
	}

	private void replaceWithValueOf(final ClassInstanceCreation visited, final ITypeBinding typeBinding, final Expression arg0) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.PrimitiveWrapperCreationCleanUp_description);

		MethodInvocation newMethodInvocation= ast.newMethodInvocation(typeBinding.getName(), "valueOf", ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(arg0))); //$NON-NLS-1$
		ASTNodes.replaceButKeepComment(rewrite, visited, newMethodInvocation, group);
	}

	@Override
	public boolean visit(final MethodInvocation visited) {
		if (visited.getExpression() == null) {
			return true;
		}

		ITypeBinding destinationTypeBinding= ASTNodes.getTargetType(visited);

		if (destinationTypeBinding != null
				&& destinationTypeBinding.isPrimitive()
				&& "valueOf".equals(visited.getName().getIdentifier())) { //$NON-NLS-1$
			if (ASTNodes.usesGivenSignature(visited, Boolean.class.getCanonicalName(), "valueOf", boolean.class.getSimpleName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, Integer.class.getCanonicalName(), "valueOf", int.class.getSimpleName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, Long.class.getCanonicalName(), "valueOf", long.class.getSimpleName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, Double.class.getCanonicalName(), "valueOf", double.class.getSimpleName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, Float.class.getCanonicalName(), "valueOf", float.class.getSimpleName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, Byte.class.getCanonicalName(), "valueOf", byte.class.getSimpleName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, Character.class.getCanonicalName(), "valueOf", char.class.getSimpleName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, Short.class.getCanonicalName(), "valueOf", short.class.getSimpleName())) { //$NON-NLS-1$
				replaceWithTheSingleArgument(visited);
				return false;
			}

			if (is(visited, Integer.class.getCanonicalName())) {
				replaceMethodName(visited, "parseInt"); //$NON-NLS-1$
				return false;
			}

			if (is(visited, Long.class.getCanonicalName())) {
				replaceMethodName(visited, "parseLong"); //$NON-NLS-1$
				return false;
			}

			if (ASTNodes.usesGivenSignature(visited, Boolean.class.getCanonicalName(), "valueOf", String.class.getCanonicalName())) { //$NON-NLS-1$
				replaceMethodName(visited, "parseBoolean"); //$NON-NLS-1$
				return false;
			}

			if (is(visited, Float.class.getCanonicalName())) {
				replaceMethodName(visited, "parseFloat"); //$NON-NLS-1$
				return false;
			}

			if (is(visited, Double.class.getCanonicalName())) {
				replaceMethodName(visited, "parseDouble"); //$NON-NLS-1$
				return false;
			}

			if (is(visited, Byte.class.getCanonicalName())) {
				replaceMethodName(visited, "parseByte"); //$NON-NLS-1$
				return false;
			}

			if (is(visited, Short.class.getCanonicalName())) {
				replaceMethodName(visited, "parseShort"); //$NON-NLS-1$
				return false;
			}
		}

		ITypeBinding typeBinding= visited.getExpression().resolveTypeBinding();
		ClassInstanceCreation classInstanceCreation= ASTNodes.as(visited.getExpression(), ClassInstanceCreation.class);

		if (typeBinding != null && classInstanceCreation != null) {
			List<Expression> classInstanceCreationArguments= classInstanceCreation.arguments();

			if (classInstanceCreationArguments.size() == 1) {
				Expression arg0= classInstanceCreationArguments.get(0);

				if (visited.arguments().isEmpty() && ASTNodes.hasType(arg0, String.class.getCanonicalName())) {
					String methodName= getMethodName(typeBinding.getQualifiedName(),
							visited.getName().getIdentifier());

					if (methodName != null) {
						replaceInstanceCreation(visited, typeBinding, methodName, arg0);
						return false;
					}
				}
			}
		}

		return true;
	}

	private boolean is(final MethodInvocation visited, final String declaringTypeQualifiedName) {
		return ASTNodes.usesGivenSignature(visited, declaringTypeQualifiedName, "valueOf", String.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(visited, declaringTypeQualifiedName, "valueOf", String.class.getCanonicalName(), int.class.getSimpleName()) //$NON-NLS-1$
						&& Objects.equals(10, ((Expression) visited.arguments().get(1)).resolveConstantExpressionValue());
	}

	private String getMethodName(final String typeName, final String invokedMethodName) {
		if (Boolean.class.getCanonicalName().equals(typeName) && "booleanValue".equals(invokedMethodName)) { //$NON-NLS-1$
			return "valueOf"; //$NON-NLS-1$
		}

		if (Integer.class.getCanonicalName().equals(typeName) && "intValue".equals(invokedMethodName)) { //$NON-NLS-1$
			return "parseInt"; //$NON-NLS-1$
		}

		if (Long.class.getCanonicalName().equals(typeName) && "longValue".equals(invokedMethodName)) { //$NON-NLS-1$
			return "parseLong"; //$NON-NLS-1$
		}

		if (Double.class.getCanonicalName().equals(typeName) && "doubleValue".equals(invokedMethodName)) { //$NON-NLS-1$
			return "parseDouble"; //$NON-NLS-1$
		}

		if (Float.class.getCanonicalName().equals(typeName) && "floatValue".equals(invokedMethodName)) { //$NON-NLS-1$
			return "parseFloat"; //$NON-NLS-1$
		}

		if (Short.class.getCanonicalName().equals(typeName) && "shortValue".equals(invokedMethodName)) { //$NON-NLS-1$
			return "parseShort"; //$NON-NLS-1$
		}

		if (Byte.class.getCanonicalName().equals(typeName) && "byteValue".equals(invokedMethodName)) { //$NON-NLS-1$
			return "parseByte"; //$NON-NLS-1$
		}

		return null;
	}

	private void replaceWithTheSingleArgument(final MethodInvocation visited) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.PrimitiveWrapperCreationCleanUp_description);

		ASTNodes.replaceButKeepComment(rewrite, visited, ASTNodes.createMoveTarget(rewrite, (Expression) visited.arguments().get(0)), group);
	}

	private void replaceMethodName(final MethodInvocation visited, final String methodName) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.PrimitiveWrapperCreationCleanUp_description);

		SimpleName name= ast.newSimpleName(methodName);

		rewrite.set(visited, MethodInvocation.NAME_PROPERTY, name, group);
	}

	private void replaceInstanceCreation(final MethodInvocation visited, final ITypeBinding typeBinding, final String methodName,
			final Expression arg0) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.PrimitiveWrapperCreationCleanUp_description);

		MethodInvocation newMethodInvocation= ast.newMethodInvocation(typeBinding.getName(), methodName, ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(arg0)));
		ASTNodes.replaceButKeepComment(rewrite, visited, newMethodInvocation, group);
	}
}

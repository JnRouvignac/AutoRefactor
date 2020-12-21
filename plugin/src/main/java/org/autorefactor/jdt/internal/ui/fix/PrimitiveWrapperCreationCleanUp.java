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
	public boolean visit(final MethodInvocation visited) {
		if (visited.getExpression() == null) {
			return true;
		}

		ITypeBinding destinationTypeBinding= ASTNodes.getTargetType(visited);

		if (destinationTypeBinding != null && destinationTypeBinding.isPrimitive()
				&& "valueOf".equals(visited.getName().getIdentifier())) { //$NON-NLS-1$
			if (ASTNodes.usesGivenSignature(visited, Boolean.class.getCanonicalName(), "valueOf", boolean.class.getSimpleName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, Byte.class.getCanonicalName(), "valueOf", byte.class.getSimpleName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, Character.class.getCanonicalName(), "valueOf", char.class.getSimpleName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, Short.class.getCanonicalName(), "valueOf", short.class.getSimpleName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, Integer.class.getCanonicalName(), "valueOf", int.class.getSimpleName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, Long.class.getCanonicalName(), "valueOf", long.class.getSimpleName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, Float.class.getCanonicalName(), "valueOf", float.class.getSimpleName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(visited, Double.class.getCanonicalName(), "valueOf", double.class.getSimpleName())) { //$NON-NLS-1$
				replaceWithTheSingleArgument(visited);
				return false;
			}

			if (is(visited, Byte.class.getCanonicalName())) {
				return replaceMethodName(visited, "parseByte"); //$NON-NLS-1$
			}

			if (is(visited, Short.class.getCanonicalName())) {
				return replaceMethodName(visited, "parseShort"); //$NON-NLS-1$
			}

			if (is(visited, Integer.class.getCanonicalName())) {
				return replaceMethodName(visited, "parseInt"); //$NON-NLS-1$
			}

			if (is(visited, Long.class.getCanonicalName())) {
				return replaceMethodName(visited, "parseLong"); //$NON-NLS-1$
			}

			if (ASTNodes.usesGivenSignature(visited, Boolean.class.getCanonicalName(), "valueOf", String.class.getCanonicalName())) { //$NON-NLS-1$
				return replaceMethodName(visited, "parseBoolean"); //$NON-NLS-1$
			}

			if (is(visited, Float.class.getCanonicalName())) {
				return replaceMethodName(visited, "parseFloat"); //$NON-NLS-1$
			}

			if (is(visited, Double.class.getCanonicalName())) {
				return replaceMethodName(visited, "parseDouble"); //$NON-NLS-1$
			}
		}

		ITypeBinding typeBinding= visited.getExpression().resolveTypeBinding();
		ClassInstanceCreation classInstanceCreation= ASTNodes.as(visited.getExpression(), ClassInstanceCreation.class);

		if (typeBinding != null && classInstanceCreation != null) {
			List<?> cicArgs= classInstanceCreation.arguments();

			if (cicArgs.size() == 1) {
				Expression arg0= (Expression) cicArgs.get(0);

				if (visited.arguments().isEmpty() && ASTNodes.hasType(arg0, String.class.getCanonicalName())) {
					String methodName= getMethodName(typeBinding.getQualifiedName(),
							visited.getName().getIdentifier());

					if (methodName != null) {
						TextEditGroup group= new TextEditGroup(MultiFixMessages.PrimitiveWrapperCreationCleanUp_description);
						ASTRewrite rewrite= cuRewrite.getASTRewrite();

						ASTNodes.replaceButKeepComment(rewrite, visited,
								newMethodInvocation(typeBinding.getName(), methodName, arg0), group);
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

	private boolean replaceMethodName(final MethodInvocation visited, final String methodName) {
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		SimpleName name= ast.newSimpleName(methodName);
		TextEditGroup group= new TextEditGroup(MultiFixMessages.PrimitiveWrapperCreationCleanUp_description);
		ASTRewrite rewrite= cuRewrite.getASTRewrite();

		rewrite.set(visited, MethodInvocation.NAME_PROPERTY, name, group);
		return false;
	}

	private void replaceWithTheSingleArgument(final MethodInvocation visited) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.PrimitiveWrapperCreationCleanUp_description);
		ASTNodes.replaceButKeepComment(rewrite, visited, ASTNodes.createMoveTarget(rewrite, (Expression) visited.arguments().get(0)), group);
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
	public boolean visit(final ClassInstanceCreation visited) {
		ITypeBinding typeBinding= visited.getType().resolveBinding();
		List<Expression> args= visited.arguments();

		if (getJavaMinorVersion() >= 5 && args.size() == 1) {
			if (ASTNodes.hasType(typeBinding, Boolean.class.getCanonicalName(), Byte.class.getCanonicalName(), Character.class.getCanonicalName(), Double.class.getCanonicalName(),
					Long.class.getCanonicalName(), Short.class.getCanonicalName(), Integer.class.getCanonicalName())) {
				replaceWithValueOf(visited, typeBinding);
				return false;
			}

			if (ASTNodes.hasType(typeBinding, Float.class.getCanonicalName())) {
				replaceFloatInstanceWithValueOf(visited, typeBinding, args);
				return false;
			}
		}

		return true;
	}

	private void replaceFloatInstanceWithValueOf(final ClassInstanceCreation visited, final ITypeBinding typeBinding,
			final List<Expression> args) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.PrimitiveWrapperCreationCleanUp_description);

		Expression arg0= args.get(0);

		if (ASTNodes.isPrimitive(arg0, double.class.getSimpleName())) {
			ASTNodes.replaceButKeepComment(rewrite, visited,
					ast.newMethodInvocation(typeBinding.getName(), "valueOf", ast.newCastExpression(ast.type(float.class.getSimpleName()), ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(arg0)))), group); //$NON-NLS-1$
		} else if (ASTNodes.hasType(arg0, Double.class.getCanonicalName())) {
			ASTNodes.replaceButKeepComment(rewrite, visited, ast.newMethodInvocation(ASTNodes.createMoveTarget(rewrite, arg0), "floatValue"), group); //$NON-NLS-1$
		} else {
			replaceWithValueOf(visited, typeBinding);
		}
	}

	private void replaceWithValueOf(final ClassInstanceCreation visited, final ITypeBinding typeBinding) {
		TextEditGroup group= new TextEditGroup(MultiFixMessages.PrimitiveWrapperCreationCleanUp_description);
		ASTRewrite rewrite= cuRewrite.getASTRewrite();

		ASTNodes.replaceButKeepComment(rewrite, visited,
				newMethodInvocation(typeBinding.getName(), "valueOf", (Expression) visited.arguments().get(0)), group); //$NON-NLS-1$
	}

	private MethodInvocation newMethodInvocation(final String typeName, final String methodName, final Expression arg) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		return ast.newMethodInvocation(typeName, methodName, ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(arg)));
	}
}

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

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Bindings;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class ParsingRatherThanValueOfCleanUp extends AbstractCleanUpRule {
	private static final Class<?>[] WRAPPER_CLASSES= { Integer.class, Boolean.class, Long.class, Double.class, Character.class, Float.class, Short.class, Byte.class };

	@Override
	public String getName() {
		return MultiFixMessages.ParsingRatherThanValueOfCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ParsingRatherThanValueOfCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ParsingRatherThanValueOfCleanUp_reason;
	}

	@Override
	public boolean visit(final MethodInvocation visited) {
		if (visited.getExpression() == null) {
			return true;
		}

		ITypeBinding destinationTypeBinding= ASTNodes.getTargetType(visited);
		String methodName= visited.getName().getIdentifier();

		if (destinationTypeBinding != null
				&& destinationTypeBinding.isPrimitive()
				&& "valueOf".equals(methodName)) { //$NON-NLS-1$
			for (Class<?> wrapperClass : WRAPPER_CLASSES) {
				String canonicalName= wrapperClass.getCanonicalName();
				String primitiveType= Bindings.getUnboxedTypeName(canonicalName);

				if (ASTNodes.usesGivenSignature(visited, canonicalName, "valueOf", primitiveType)) { //$NON-NLS-1$
					replaceWithTheSingleArgument(visited);
					return false;
				}

				String parsingMethodName= getParsingMethodName(canonicalName);

				if (parsingMethodName != null
						&& isValueOfString(visited, canonicalName)) {
					replaceMethodName(visited, parsingMethodName);
					return false;
				}
			}
		}

		ITypeBinding typeBinding= visited.getExpression().resolveTypeBinding();

		if (typeBinding != null
				&& visited.arguments().isEmpty()) {
			String primitiveValueMethodName= getPrimitiveValueMethodName(typeBinding.getQualifiedName());
			String parsingMethodName= getParsingMethodName(typeBinding.getQualifiedName());

			if (primitiveValueMethodName != null
					&& primitiveValueMethodName.equals(methodName)
					&& parsingMethodName != null) {
				ClassInstanceCreation classInstanceCreation= ASTNodes.as(visited.getExpression(), ClassInstanceCreation.class);
				MethodInvocation methodInvocation= ASTNodes.as(visited.getExpression(), MethodInvocation.class);

				if (classInstanceCreation != null) {
					List<Expression> classInstanceCreationArguments= classInstanceCreation.arguments();

					if (classInstanceCreationArguments.size() == 1) {
						Expression arg0= classInstanceCreationArguments.get(0);

						if (ASTNodes.hasType(arg0, String.class.getCanonicalName())) {
							replaceByParsing(visited, typeBinding, parsingMethodName, arg0);
							return false;
						}
					}
				} else if (methodInvocation != null
						&& isValueOfString(methodInvocation, typeBinding.getQualifiedName())) {
					replaceByParsing(visited, typeBinding, parsingMethodName, (Expression) methodInvocation.arguments().get(0));
					return false;
				}
			}
		}

		return true;
	}

	private boolean isValueOfString(final MethodInvocation visited, final String declaringTypeQualifiedName) {
		return ASTNodes.usesGivenSignature(visited, declaringTypeQualifiedName, "valueOf", String.class.getCanonicalName()) //$NON-NLS-1$
				|| !Boolean.class.getCanonicalName().equals(declaringTypeQualifiedName)
				&& ASTNodes.usesGivenSignature(visited, declaringTypeQualifiedName, "valueOf", String.class.getCanonicalName(), int.class.getSimpleName()) //$NON-NLS-1$
						&& Long.valueOf(10L).equals(ASTNodes.getIntegerLiteral((Expression) visited.arguments().get(1)));
	}

	private String getPrimitiveValueMethodName(final String wrapperFullyQualifiedName) {
		String primitiveTypeName= Bindings.getUnboxedTypeName(wrapperFullyQualifiedName);

		if (primitiveTypeName != null) {
			return primitiveTypeName + "Value"; //$NON-NLS-1$
		}

		return null;
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

	private void replaceWithTheSingleArgument(final MethodInvocation visited) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ParsingRatherThanValueOfCleanUp_description);

		ASTNodes.replaceButKeepComment(rewrite, visited, ASTNodes.createMoveTarget(rewrite, (Expression) visited.arguments().get(0)), group);
	}

	private void replaceMethodName(final MethodInvocation visited, final String methodName) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ParsingRatherThanValueOfCleanUp_description);

		rewrite.set(visited, MethodInvocation.NAME_PROPERTY, ast.newSimpleName(methodName), group);
	}

	private void replaceByParsing(final MethodInvocation visited, final ITypeBinding typeBinding, final String methodName,
			final Expression arg0) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ParsingRatherThanValueOfCleanUp_description);

		MethodInvocation newMethodInvocation= ast.newMethodInvocation(typeBinding.getName(), methodName, ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(arg0)));
		ASTNodes.replaceButKeepComment(rewrite, visited, newMethodInvocation, group);
	}
}

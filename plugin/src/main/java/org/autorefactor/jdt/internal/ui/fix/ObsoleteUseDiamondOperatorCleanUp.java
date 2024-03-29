/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015-2016 Jean-Noël Rouvignac - initial API and implementation
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
import java.util.Arrays;
import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.ParameterizedType;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class ObsoleteUseDiamondOperatorCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.ObsoleteUseDiamondOperatorCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ObsoleteUseDiamondOperatorCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ObsoleteUseDiamondOperatorCleanUp_reason;
	}

	@Override
	public boolean isJavaVersionSupported(final Release javaSeRelease) {
		return javaSeRelease.getMinorVersion() >= 7;
	}

	@Override
	public boolean visit(final ClassInstanceCreation node) {
		Type type= node.getType();

		if (type.isParameterizedType()
				&& node.getAnonymousClassDeclaration() == null
				&& ASTNodes.getTargetType(node) != null
				&& canUseDiamondOperator(node, type)) {
			List<Type> typeArguments= ((ParameterizedType) type).typeArguments();

			if (!typeArguments.isEmpty()) {
				TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteUseDiamondOperatorCleanUp_description);
				ASTRewrite rewrite= cuRewrite.getASTRewrite();

				rewrite.remove(typeArguments, group);
				return false;
			}
		}

		return true;
	}

	/**
	 * Tries to rebuild the process that leads to
	 * {@link ClassInstanceCreation#isResolvedTypeInferredFromExpectedType()}.
	 */
	private boolean canUseDiamondOperator(final ClassInstanceCreation node, final Type type) {
		List<Expression> args= node.arguments();
		if (args.isEmpty()) {
			return true;
		}

		ITypeBinding typeBinding= type.resolveBinding();
		IMethodBinding ctorBinding= node.resolveConstructorBinding();

		if (typeBinding == null || ctorBinding == null) {
			return false;
		}

		List<ITypeBinding> typeArguments= new ArrayList<>(Arrays.asList(typeBinding.getTypeArguments()));
		ITypeBinding typeDecl= typeBinding.getTypeDeclaration();
		List<ITypeBinding> typeParameters= new ArrayList<>(Arrays.asList(typeDecl.getTypeParameters()));

		IMethodBinding methodDecl= ctorBinding.getMethodDeclaration();
		ITypeBinding[] actualCtorParamTypes= ctorBinding.getParameterTypes();
		ITypeBinding[] declMethodParamTypes= methodDecl.getParameterTypes();
		int limit= Math.min(declMethodParamTypes.length, args.size());

		for (int i= 0; i < limit; i++) {
			ITypeBinding declParamType= declMethodParamTypes[i];
			ITypeBinding actualParamType= actualCtorParamTypes[i];
			String actualParamTypeQName= actualParamType.getErasure().getQualifiedName();
			Expression actualArg= args.get(i);
			ITypeBinding actualArgType= ASTNodes.findImplementedType(actualArg.resolveTypeBinding(), actualParamTypeQName);

			if (actualArgType != null && declParamType.isParameterizedType()) {
				ITypeBinding[] declParamTypeArgs= declParamType.getTypeArguments();
				ITypeBinding[] actualArgTypeArgs= actualArgType.getTypeArguments();

				for (int j= 0; j < declParamTypeArgs.length; j++) {
					ITypeBinding declParamTypeArg= declParamTypeArgs[j];

					if (declParamTypeArg.isWildcardType() && actualArgTypeArgs.length != 0) {
						ITypeBinding declParamTypeArgBound= declParamTypeArg.getBound();
						int typeParamIndex= typeParameters.indexOf(declParamTypeArgBound);

						ITypeBinding actualArgTypeArg= actualArgTypeArgs[j];
						int typeArgIndex= typeArguments.indexOf(actualArgTypeArg);

						if (typeParamIndex == -1 || typeArgIndex == -1) {
							return false;
						}

						// The type parameter is matching
						typeParameters.remove(typeParamIndex);
						typeArguments.remove(typeArgIndex);
					}
				}
			}
		}

		// All the type parameters are matching
		return typeParameters.isEmpty();
	}
}

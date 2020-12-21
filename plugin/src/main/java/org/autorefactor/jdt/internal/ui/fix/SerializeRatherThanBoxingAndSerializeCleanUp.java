/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice TIERCELIN - initial API and implementation
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

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Bindings;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class SerializeRatherThanBoxingAndSerializeCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.SerializeRatherThanBoxingAndSerializeCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.SerializeRatherThanBoxingAndSerializeCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.SerializeRatherThanBoxingAndSerializeCleanUp_reason;
	}

	@Override
	public boolean visit(final MethodInvocation visited) {
		if (visited.getExpression() != null
				&& visited.arguments().isEmpty()) {
			Class<?>[] wrapperClasses= { Integer.class, Boolean.class, Long.class, Double.class, Character.class, Float.class, Short.class, Byte.class };

			for (Class<?> wrapperClass : wrapperClasses) {
				String canonicalName= wrapperClass.getCanonicalName();

				if (ASTNodes.usesGivenSignature(visited, canonicalName, "toString")) { //$NON-NLS-1$
					MethodInvocation methodInvocation = ASTNodes.as(visited.getExpression(), MethodInvocation.class);

					if (methodInvocation != null
							&& ASTNodes.usesGivenSignature(methodInvocation, canonicalName, "valueOf", Bindings.getUnboxedTypeName(canonicalName)) //$NON-NLS-1$
							&& ASTNodes.isPrimitive((Expression) methodInvocation.arguments().get(0))) {
						refactor(visited, (Expression) methodInvocation.arguments().get(0), wrapperClass);
						return false;
					}

					ClassInstanceCreation classInstanceCreation = ASTNodes.as(visited.getExpression(), ClassInstanceCreation.class);

					if (classInstanceCreation != null
							&& ASTNodes.hasType(classInstanceCreation.getType().resolveBinding(), canonicalName)
							&& ASTNodes.isPrimitive((Expression) classInstanceCreation.arguments().get(0))) {
						refactor(visited, (Expression) classInstanceCreation.arguments().get(0), wrapperClass);
						return false;
					}

					CastExpression castExpression = ASTNodes.as(visited.getExpression(), CastExpression.class);

					if (castExpression != null
							&& ASTNodes.hasType(castExpression.getType().resolveBinding(), canonicalName)
							&& ASTNodes.isPrimitive(castExpression.getExpression())) {
						refactor(visited, castExpression.getExpression(), wrapperClass);
						return false;
					}

					return true;
				}
			}
		}

		return true;
	}

	private void refactor(final MethodInvocation visited, final Expression primitiveValue, final Class<?> wrapperClass) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.SerializeRatherThanBoxingAndSerializeCleanUp_description);

		MethodInvocation newMethodInvocation= ast.newMethodInvocation(ast.newSimpleName(wrapperClass.getSimpleName()), "toString", //$NON-NLS-1$
				ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(primitiveValue)));
		ASTNodes.replaceButKeepComment(rewrite, visited, newMethodInvocation, group);
	}
}

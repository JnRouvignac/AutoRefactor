/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice Tiercelin - Initial API and implementation
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
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class ObsoleteAutoBoxingRatherThanExplicitMethodCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.ObsoleteAutoBoxingRatherThanExplicitMethodCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ObsoleteAutoBoxingRatherThanExplicitMethodCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ObsoleteAutoBoxingRatherThanExplicitMethodCleanUp_reason;
	}

	@Override
	public boolean isJavaVersionSupported(final Release javaSeRelease) {
		return javaSeRelease.getMinorVersion() >= 5;
	}

	@Override
	public boolean visit(final MethodInvocation node) {
		if ("valueOf".equals(node.getName().getIdentifier()) && node.getExpression() != null //$NON-NLS-1$
				&& node.resolveMethodBinding() != null
				&& (ASTNodes.usesGivenSignature(node, Boolean.class.getCanonicalName(), "valueOf", boolean.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, Byte.class.getCanonicalName(), "valueOf", byte.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, Character.class.getCanonicalName(), "valueOf", char.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, Short.class.getCanonicalName(), "valueOf", short.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, Integer.class.getCanonicalName(), "valueOf", int.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, Long.class.getCanonicalName(), "valueOf", long.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, Float.class.getCanonicalName(), "valueOf", float.class.getSimpleName()) //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, Double.class.getCanonicalName(), "valueOf", double.class.getSimpleName()))) { //$NON-NLS-1$
			ITypeBinding primitiveType= node.resolveMethodBinding().getParameterTypes()[0];
			ITypeBinding wrapperClass= node.resolveMethodBinding().getDeclaringClass();

			ITypeBinding actualResultType= ASTNodes.getTargetType(node);
			ITypeBinding actualParameterType= ((Expression) node.arguments().get(0)).resolveTypeBinding();

			if (actualResultType != null
					&& (actualResultType.equals(primitiveType) || actualResultType.equals(wrapperClass))
					|| actualParameterType != null && actualParameterType.equals(wrapperClass)) {
				useAutoBoxing(node, primitiveType, wrapperClass, actualParameterType, actualResultType);
				return false;
			}
		}

		return true;
	}

	private void useAutoBoxing(final MethodInvocation node, final ITypeBinding primitiveType,
			final ITypeBinding wrapperClass, final ITypeBinding actualParameterType,
			final ITypeBinding actualResultType) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteAutoBoxingRatherThanExplicitMethodCleanUp_description);

		if (primitiveType != null && !primitiveType.equals(actualParameterType)
				&& !primitiveType.equals(actualResultType)
				&& (wrapperClass == null || !wrapperClass.equals(actualParameterType))) {
			ASTNodes.replaceButKeepComment(rewrite, node, ast.newCastExpression(ast.type(primitiveType.getName()), ASTNodes.createMoveTarget(rewrite, (Expression) node.arguments().get(0))), group);
		} else {
			ASTNodes.replaceButKeepComment(rewrite, node, ASTNodes.createMoveTarget(rewrite, (Expression) node.arguments().get(0)), group);
		}
	}
}

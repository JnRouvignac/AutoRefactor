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
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class UnboxingRatherThanExplicitMethodCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_UnboxingRatherThanExplicitMethodCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_UnboxingRatherThanExplicitMethodCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_UnboxingRatherThanExplicitMethodCleanUp_reason;
	}

	@Override
	public boolean isJavaVersionSupported(final Release javaSeRelease) {
		return javaSeRelease.getMinorVersion() >= 5;
	}

	@Override
	public boolean visit(final MethodInvocation node) {
		if (node.getExpression() != null
				&& (ASTNodes.usesGivenSignature(node, Boolean.class.getCanonicalName(), "booleanValue") || ASTNodes.usesGivenSignature(node, Byte.class.getCanonicalName(), "byteValue") //$NON-NLS-1$ //$NON-NLS-2$
						|| ASTNodes.usesGivenSignature(node, Character.class.getCanonicalName(), "charValue") //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, Short.class.getCanonicalName(), "shortValue") //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, Integer.class.getCanonicalName(), "intValue") //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, Long.class.getCanonicalName(), "longValue") //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, Float.class.getCanonicalName(), "floatValue") //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(node, Double.class.getCanonicalName(), "doubleValue"))) { //$NON-NLS-1$
			ITypeBinding actualResultType= ASTNodes.getTargetType(node);

			if (actualResultType != null && actualResultType.isAssignmentCompatible(node.resolveTypeBinding())) {
				useUnboxing(node);
				return false;
			}
		}

		return true;
	}

	private void useUnboxing(final MethodInvocation node) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.CleanUpRefactoringWizard_UnboxingRatherThanExplicitMethodCleanUp_name);
		rewrite.replace(node, ASTNodes.createMoveTarget(rewrite, node.getExpression()), group);
	}
}

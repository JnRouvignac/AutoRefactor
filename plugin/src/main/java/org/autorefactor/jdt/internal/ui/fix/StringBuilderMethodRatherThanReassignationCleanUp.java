/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Initial API and implementation
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

import java.util.Arrays;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.SuperFieldAccess;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class StringBuilderMethodRatherThanReassignationCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_StringBuilderMethodRatherThanReassignationCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_StringBuilderMethodRatherThanReassignationCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_StringBuilderMethodRatherThanReassignationCleanUp_reason;
	}

	@Override
	public boolean visit(final Assignment node) {
		Expression targetVar= node.getLeftHandSide();
		Expression var= node.getRightHandSide();
		if (ASTNodes.hasOperator(node, Assignment.Operator.ASSIGN) && ASTNodes.hasType(targetVar, StringBuffer.class.getCanonicalName(), StringBuilder.class.getCanonicalName())
				&& var instanceof MethodInvocation) {
			var= getVar(var);

			if (ASTNodes.isSameVariable(targetVar, var) && ASTNodes.isPassive(targetVar)) {
				ASTRewrite rewrite= cuRewrite.getASTRewrite();
				TextEditGroup group= new TextEditGroup(MultiFixMessages.CleanUpRefactoringWizard_StringBuilderMethodRatherThanReassignationCleanUp_name);
				rewrite.replace(node, ASTNodes.createMoveTarget(rewrite, node.getRightHandSide()), group);
				return false;
			}
		}

		return true;
	}

	private Expression getVar(final Expression var) {
		if (var instanceof Name || var instanceof FieldAccess || var instanceof SuperFieldAccess) {
			return var;
		}

		MethodInvocation methodInvocation= ASTNodes.as(var, MethodInvocation.class);

		if (methodInvocation != null && ASTNodes.hasType(methodInvocation.getExpression(), StringBuffer.class.getCanonicalName(), StringBuilder.class.getCanonicalName())
				&& Arrays.asList("append", "appendCodePoint", "delete", "deleteCharAt", "insert", "replace", "reverse") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
						.contains(methodInvocation.getName().getIdentifier())) {
			return getVar(methodInvocation.getExpression());
		}

		return null;
	}
}

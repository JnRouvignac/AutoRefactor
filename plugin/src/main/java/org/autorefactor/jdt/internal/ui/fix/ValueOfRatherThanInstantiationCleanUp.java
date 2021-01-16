/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016-2021 Fabrice Tiercelin - #199 Replace unnecessary Boolean constant on boolean assignment
 *                                             #200 Compile error when Float myFloat = new Float(doubleObject);
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
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class ValueOfRatherThanInstantiationCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.ValueOfRatherThanInstantiationCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ValueOfRatherThanInstantiationCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ValueOfRatherThanInstantiationCleanUp_reason;
	}

	@Override
	public boolean visit(final ClassInstanceCreation visited) {
		ITypeBinding typeBinding= visited.getType().resolveBinding();
		List<Expression> args= visited.arguments();

		if (args.size() == 1) {
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

				if (getJavaMinorVersion() >= 5
						|| ASTNodes.hasType(typeBinding, String.class.getCanonicalName())) {
					replaceWithValueOf(visited, typeBinding, arg0);
					return false;
				}
			}
		}

		return true;
	}

	private void replaceFloatWithValueOf(final ClassInstanceCreation visited, final ITypeBinding typeBinding, final Expression arg0) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ValueOfRatherThanInstantiationCleanUp_description);

		MethodInvocation newMethodInvocation= ast.newMethodInvocation();
		newMethodInvocation.setExpression(ASTNodeFactory.newName(ast, typeBinding.getName()));
		newMethodInvocation.setName(ast.newSimpleName("valueOf")); //$NON-NLS-1$
		newMethodInvocation.arguments().add(ast.newCastExpression(ast.type(float.class.getSimpleName()), ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(arg0))));

		ASTNodes.replaceButKeepComment(rewrite, visited, newMethodInvocation, group);
	}

	private void replaceFloatWithFloatValue(final ClassInstanceCreation visited, final Expression arg0) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ValueOfRatherThanInstantiationCleanUp_description);

		MethodInvocation newMethodInvocation= ast.newMethodInvocation();
		newMethodInvocation.setExpression(ASTNodes.createMoveTarget(rewrite, arg0));
		newMethodInvocation.setName(ast.newSimpleName("floatValue")); //$NON-NLS-1$

		ASTNodes.replaceButKeepComment(rewrite, visited, newMethodInvocation, group);
	}

	private void replaceWithTheSingleArgument(final ClassInstanceCreation visited) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ValueOfRatherThanInstantiationCleanUp_description);

		ASTNodes.replaceButKeepComment(rewrite, visited, ASTNodes.createMoveTarget(rewrite, (Expression) visited.arguments().get(0)), group);
	}

	private void replaceWithValueOf(final ClassInstanceCreation visited, final ITypeBinding typeBinding, final Expression arg0) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ValueOfRatherThanInstantiationCleanUp_description);

		MethodInvocation newMethodInvocation= ast.newMethodInvocation();
		newMethodInvocation.setExpression(ASTNodeFactory.newName(ast, typeBinding.getName()));
		newMethodInvocation.setName(ast.newSimpleName("valueOf")); //$NON-NLS-1$
		newMethodInvocation.arguments().add(ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(arg0)));

		ASTNodes.replaceButKeepComment(rewrite, visited, newMethodInvocation, group);
	}
}

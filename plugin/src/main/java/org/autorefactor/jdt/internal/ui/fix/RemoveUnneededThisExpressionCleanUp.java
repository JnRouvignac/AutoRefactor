/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2016 Jean-Noël Rouvignac - initial API and implementation
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

import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.util.NotImplementedException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.AbstractTypeDeclaration;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class RemoveUnneededThisExpressionCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.RemoveUnneededThisExpressionCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.RemoveUnneededThisExpressionCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.RemoveUnneededThisExpressionCleanUp_reason;
	}

	@Override
	public boolean visit(final MethodInvocation node) {
		ThisExpression te= ASTNodes.as(node.getExpression(), ThisExpression.class);

		if (thisExpressionRefersToEnclosingType(te) && isCallingMethodDeclaredInEnclosingType(node)
				&& node.typeArguments().isEmpty()) {
			// Remove useless thisExpressions
			TextEditGroup group= new TextEditGroup(MultiFixMessages.RemoveUnneededThisExpressionCleanUp_description);
			cuRewrite.getASTRewrite().remove(node.getExpression(), group);
			return false;
		}

		return true;
	}

	private static boolean thisExpressionRefersToEnclosingType(final ThisExpression thisExpression) {
		return thisExpression != null
				&& thisExpressionRefersToEnclosingType(thisExpression.getQualifier(), thisExpression);
	}

	private static boolean thisExpressionRefersToEnclosingType(final Name thisQualifierName, final ASTNode node) {
		if (thisQualifierName == null) {
			return true;
		}

		ASTNode enclosingType= ASTNodes.getEnclosingType(node);

		if (enclosingType instanceof AnonymousClassDeclaration) {
			return false;
		}

		AbstractTypeDeclaration ancestor= (AbstractTypeDeclaration) enclosingType;

		if (thisQualifierName instanceof SimpleName) {
			return ASTNodes.isEqual((SimpleName) thisQualifierName, ancestor.getName());
		}

		if (thisQualifierName instanceof QualifiedName) {
			QualifiedName qn= (QualifiedName) thisQualifierName;
			return ASTNodes.isEqual(qn.getName(), ancestor.getName())
					&& thisExpressionRefersToEnclosingType(qn.getQualifier(), ancestor);
		}

		throw new NotImplementedException(thisQualifierName);
	}

	private boolean isCallingMethodDeclaredInEnclosingType(final MethodInvocation node) {
		ASTNode currentType= ASTNodes.getEnclosingType(node);
		IMethodBinding mb= node.resolveMethodBinding();

		if (mb == null) {
			return false;
		}

		if (currentType instanceof AnonymousClassDeclaration) {
			AnonymousClassDeclaration c= (AnonymousClassDeclaration) currentType;
			ITypeBinding enclosingTypeBinding= c.resolveBinding();
			return enclosingTypeBinding.isSubTypeCompatible(mb.getDeclaringClass());
		}

		if (currentType instanceof AbstractTypeDeclaration) {
			AbstractTypeDeclaration ed= (AbstractTypeDeclaration) currentType;
			ITypeBinding enclosingTypeBinding= ed.resolveBinding();
			return enclosingTypeBinding.isSubTypeCompatible(mb.getDeclaringClass());
		}

		throw new NotImplementedException(node, node);
	}
}

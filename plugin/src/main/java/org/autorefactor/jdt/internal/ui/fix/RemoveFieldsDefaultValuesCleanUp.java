/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2015 Jean-Noël Rouvignac - initial API and implementation
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

import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.EnumDeclaration;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.NullLiteral;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

/** See {@link #getDescription()} method. */
public class RemoveFieldsDefaultValuesCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_RemoveFieldsDefaultValuesCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_RemoveFieldsDefaultValuesCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_RemoveFieldsDefaultValuesCleanUp_reason;
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean visit(final FieldDeclaration node) {
		if (!canRemoveFieldDefaultValue(node)) {
			return true;
		}
		ITypeBinding fieldType= node.getType().resolveBinding();
		if (fieldType == null || Modifier.isFinal(node.getModifiers())) {
			return true;
		}

		boolean visitSubtree= true;
		for (VariableDeclarationFragment fragment : (List<VariableDeclarationFragment>) node.fragments()) {
			Expression initializer= fragment.getInitializer();
			if (initializer != null && (!fieldType.isPrimitive() && ASTNodes.is(initializer, NullLiteral.class)
					|| fieldType.isPrimitive() && isPrimitiveLiteral(initializer)
							&& isPrimitiveDefaultValue(initializer.resolveConstantExpressionValue()))) {
				cuRewrite.getASTRewrite().remove(initializer, null);
				visitSubtree= false;
			}
		}

		return visitSubtree;
	}

	private boolean canRemoveFieldDefaultValue(final FieldDeclaration node) {
		// Do not remove default values from interface/annotation fields
		// because they are final by default
		ASTNode parent= node.getParent();
		if (parent instanceof TypeDeclaration) {
			return !((TypeDeclaration) parent).isInterface();
		}

		return parent instanceof AnonymousClassDeclaration || parent instanceof EnumDeclaration;
	}

	private boolean isPrimitiveDefaultValue(final Object val) {
		if (val instanceof Short || val instanceof Integer || val instanceof Long) {
			return ((Number) val).longValue() == 0;
		}
		if (val instanceof Double || val instanceof Float) {
			return ((Number) val).doubleValue() == 0;
		}
		if (val instanceof Boolean) {
			return Boolean.FALSE.equals(val);
		}
		return val instanceof Character && ((Character) val).charValue() == '\u0000';
	}

	private boolean isPrimitiveLiteral(final Expression initializer) {
		switch (initializer.getNodeType()) {
		case ASTNode.BOOLEAN_LITERAL:
		case ASTNode.CHARACTER_LITERAL:
		case ASTNode.NUMBER_LITERAL:
			return true;

		default: // including string and null literal
			return false;
		}
	}
}

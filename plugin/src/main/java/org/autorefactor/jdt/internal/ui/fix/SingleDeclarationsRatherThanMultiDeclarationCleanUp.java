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

import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.IExtendedModifier;
import org.eclipse.jdt.core.dom.Javadoc;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class SingleDeclarationsRatherThanMultiDeclarationCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.SingleDeclarationsRatherThanMultiDeclarationCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.SingleDeclarationsRatherThanMultiDeclarationCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.SingleDeclarationsRatherThanMultiDeclarationCleanUp_reason;
	}

	@Override
	public boolean visit(final FieldDeclaration visited) {
		List<IExtendedModifier> modifiers= visited.modifiers();
		List<VariableDeclarationFragment> fragments= visited.fragments();
		return visitMultiDeclaration(visited, modifiers, visited.getType(), fragments, visited.getJavadoc());
	}

	@Override
	public boolean visit(final VariableDeclarationStatement visited) {
		List<IExtendedModifier> modifiers= visited.modifiers();
		List<VariableDeclarationFragment> fragments= visited.fragments();
		return visitMultiDeclaration(visited, modifiers, visited.getType(), fragments, null);
	}

	private boolean visitMultiDeclaration(final ASTNode visited, final List<IExtendedModifier> modifiers, final Type type,
			final List<VariableDeclarationFragment> fragments, final Javadoc docComment) {
		if (fragments != null && fragments.size() > 1) {
			refactorMultiDeclaration(visited, modifiers, type, fragments, docComment);
			return false;
		}

		return true;
	}

	private void refactorMultiDeclaration(final ASTNode visited, final List<IExtendedModifier> modifiers, final Type type,
			final List<VariableDeclarationFragment> fragments, final Javadoc docComment) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.SingleDeclarationsRatherThanMultiDeclarationCleanUp_description);

		for (int i= fragments.size() - 1; 0 <= i; i--) {
			VariableDeclarationFragment fragment= fragments.get(i);

			SimpleName copyOfFragment= ASTNodes.createMoveTarget(rewrite, fragment.getName());
			Type copyOfType= ast.createCopyTarget(type);

			Expression copyOfInitializer;
			if (fragment.getInitializer() != null) {
				copyOfInitializer= ASTNodes.createMoveTarget(rewrite, fragment.getInitializer());
			} else {
				copyOfInitializer= null;
			}

			VariableDeclarationFragment newFragment= ast.newVariableDeclarationFragment(copyOfFragment, copyOfInitializer);
			newFragment.extraDimensions().addAll(ASTNodes.createMoveTarget(rewrite, fragment.extraDimensions()));
			ASTNode newNode;
			if (visited instanceof VariableDeclarationStatement) {
				VariableDeclarationStatement newStatement= ast.newVariableDeclarationStatement(copyOfType, newFragment);
				updateModifiers(modifiers, newStatement.modifiers());
				newNode= newStatement;
			} else {
				FieldDeclaration newField= ast.newFieldDeclaration(copyOfType, newFragment);

				if (docComment != null) {
					newField.setJavadoc(ast.createCopyTarget(docComment));
				}

				updateModifiers(modifiers, newField.modifiers());
				newNode= newField;
			}

			if (i > 0) {
				rewrite.insertAfter(newNode, visited, group);
			} else {
				ASTNodes.replaceButKeepComment(rewrite, visited, newNode, group);
			}
		}
	}

	private void updateModifiers(final List<IExtendedModifier> modifiers, final List<ASTNode> newModifiers) {
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		newModifiers.clear();

		for (Object modifier : modifiers) {
			newModifiers.add(ast.createCopyTarget((ASTNode) modifier));
		}
	}
}

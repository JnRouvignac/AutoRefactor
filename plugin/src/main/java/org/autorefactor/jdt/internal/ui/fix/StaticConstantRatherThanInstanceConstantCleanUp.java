/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice TIERCELIN - initial API and implementation
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

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.IExtendedModifier;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class StaticConstantRatherThanInstanceConstantCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.StaticConstantRatherThanInstanceConstantCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.StaticConstantRatherThanInstanceConstantCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.StaticConstantRatherThanInstanceConstantCleanUp_reason;
	}

	@Override
	public boolean visit(final FieldDeclaration visited) {
		if (visited.getType().isPrimitiveType() || ASTNodes.hasType(visited.getType().resolveBinding(), Byte.class.getCanonicalName(), Character.class.getCanonicalName(), Short.class.getCanonicalName(),
				Integer.class.getCanonicalName(), Long.class.getCanonicalName(), Boolean.class.getCanonicalName(), Float.class.getCanonicalName(), Double.class.getCanonicalName(), String.class.getCanonicalName())) {
			Modifier finalModifier= null;
			Collection<IExtendedModifier> modifiers= visited.modifiers();

			for (Modifier modifier : getModifiersOnly(modifiers)) {
				if (modifier.isStatic()) {
					return true;
				}
				if (modifier.isFinal()) {
					finalModifier= modifier;
				}
			}

			if (finalModifier != null
					&& visited.fragments() != null
					&& visited.fragments().size() == 1) {
				Expression initializer= ((VariableDeclarationFragment) visited.fragments().get(0)).getInitializer();

				if (ASTNodes.isHardCoded(initializer)) {
					addStaticModifier(finalModifier);
					return false;
				}
			}
		}

		return true;
	}

	private void addStaticModifier(final Modifier finalModifier) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.StaticConstantRatherThanInstanceConstantCleanUp_description);

		rewrite.insertBefore(ast.static0(), finalModifier, group);
	}

	private List<Modifier> getModifiersOnly(final Collection<IExtendedModifier> modifiers) {
		List<Modifier> results= new LinkedList<>();
		for (IExtendedModifier em : modifiers) {
			if (em.isModifier()) {
				results.add((Modifier) em);
			}
		}

		return results;
	}
}

/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2021 Fabrice Tiercelin - initial API and implementation
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
import org.eclipse.jdt.core.dom.CharacterLiteral;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class ObsoleteCharacterParameterRatherThanStringCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.ObsoleteCharacterParameterRatherThanStringCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ObsoleteCharacterParameterRatherThanStringCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ObsoleteCharacterParameterRatherThanStringCleanUp_reason;
	}

	@Override
	public boolean visit(final MethodInvocation visited) {
		if (ASTNodes.usesGivenSignature(visited, String.class.getCanonicalName(), "indexOf", String.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(visited, String.class.getCanonicalName(), "lastIndexOf", String.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(visited, String.class.getCanonicalName(), "indexOf", String.class.getCanonicalName(), int.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(visited, String.class.getCanonicalName(), "lastIndexOf", String.class.getCanonicalName(), int.class.getCanonicalName())) { //$NON-NLS-1$
			StringLiteral stringLiteral= ASTNodes.as((Expression) visited.arguments().get(0), StringLiteral.class);

			if (stringLiteral != null) {
				String value= stringLiteral.getLiteralValue();

				if (value.length() == 1) {
					refactorWithCharacter(stringLiteral, value);
					return false;
				}
			}
		}

		return true;
	}

	private void refactorWithCharacter(final StringLiteral stringLiteral, final String value) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteCharacterParameterRatherThanStringCleanUp_description);

		CharacterLiteral replacement= ast.newCharacterLiteral();
		replacement.setCharValue(value.charAt(0));
		rewrite.replace(stringLiteral, replacement, group);
	}
}

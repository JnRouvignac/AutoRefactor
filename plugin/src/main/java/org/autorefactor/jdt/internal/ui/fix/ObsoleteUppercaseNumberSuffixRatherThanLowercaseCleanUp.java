/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Andrei Paikin - Initial API and implementation
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
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.text.edits.TextEditGroup;

/**
 * See {@link #getDescription()} method.
 *
 * This rule refactors the Sonar squid:LowerCaseLongSuffixCheck.
 */
public class ObsoleteUppercaseNumberSuffixRatherThanLowercaseCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.ObsoleteUppercaseNumberSuffixRatherThanLowercaseCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ObsoleteUppercaseNumberSuffixRatherThanLowercaseCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ObsoleteUppercaseNumberSuffixRatherThanLowercaseCleanUp_reason;
	}

	@Override
	public boolean visit(final NumberLiteral node) {
		String token= node.getToken();

		if (token.endsWith("l") //$NON-NLS-1$
				|| token.endsWith("f") && !token.startsWith("0x") && !token.startsWith("0X")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			useUppercase(node, token);
			return false;
		}

		return true;
	}

	private void useUppercase(final NumberLiteral node, final String token) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteUppercaseNumberSuffixRatherThanLowercaseCleanUp_description);

		String newToken= token.substring(0, token.length() - 1) + token.substring(token.length() - 1).toUpperCase();
		NumberLiteral replacement= ast.newNumberLiteral(newToken);

		ASTNodes.replaceButKeepComment(rewrite, node, replacement, group);
	}
}

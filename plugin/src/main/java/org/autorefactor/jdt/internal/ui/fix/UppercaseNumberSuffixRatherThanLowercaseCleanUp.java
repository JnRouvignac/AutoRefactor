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

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.text.edits.TextEditGroup;

/**
 * See {@link #getDescription()} method.
 *
 * This rule refactors the Sonar squid:LowerCaseLongSuffixCheck.
 */
public class UppercaseNumberSuffixRatherThanLowercaseCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_UppercaseNumberSuffixRatherThanLowercaseCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_UppercaseNumberSuffixRatherThanLowercaseCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_UppercaseNumberSuffixRatherThanLowercaseCleanUp_reason;
	}

	@Override
	public boolean visit(final NumberLiteral node) {
		String token= node.getToken();

		if (token.endsWith("l") || token.endsWith("f")) { //$NON-NLS-1$ //$NON-NLS-2$
			useUppercase(node, token);
			return false;
		}

		return true;
	}

	private void useUppercase(final NumberLiteral node, final String token) {
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		String newToken= token.substring(0, token.length() - 1) + token.substring(token.length() - 1).toUpperCase();
		NumberLiteral replacement= ast.number(newToken);
		TextEditGroup group= new TextEditGroup(MultiFixMessages.CleanUpRefactoringWizard_UppercaseNumberSuffixRatherThanLowercaseCleanUp_name);
		cuRewrite.getASTRewrite().replace(node, replacement, group);
	}
}

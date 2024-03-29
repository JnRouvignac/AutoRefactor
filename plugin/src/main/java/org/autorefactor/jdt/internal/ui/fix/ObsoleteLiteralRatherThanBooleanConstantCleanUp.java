/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016-2019 Fabrice Tiercelin - initial API and implementation
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
import org.eclipse.jdt.core.dom.BooleanLiteral;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class ObsoleteLiteralRatherThanBooleanConstantCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.ObsoleteLiteralRatherThanBooleanConstantCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ObsoleteLiteralRatherThanBooleanConstantCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ObsoleteLiteralRatherThanBooleanConstantCleanUp_reason;
	}

	@Override
	public boolean visit(final QualifiedName visited) {
		ITypeBinding typeBinding= ASTNodes.getTargetType(visited);

		if (typeBinding != null && typeBinding.isPrimitive()) {
			if (ASTNodes.isField(visited, Boolean.class.getCanonicalName(), "TRUE")) { //$NON-NLS-1$
				replaceWithBooleanLiteral(visited, true);
				return false;
			}

			if (ASTNodes.isField(visited, Boolean.class.getCanonicalName(), "FALSE")) { //$NON-NLS-1$
				replaceWithBooleanLiteral(visited, false);
				return false;
			}
		}

		return true;
	}

	private void replaceWithBooleanLiteral(final QualifiedName visited, final boolean val) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteLiteralRatherThanBooleanConstantCleanUp_description);

		BooleanLiteral booleanLiteral= ast.newBooleanLiteral(val);
		ASTNodes.replaceButKeepComment(rewrite, visited, booleanLiteral, group);
	}
}

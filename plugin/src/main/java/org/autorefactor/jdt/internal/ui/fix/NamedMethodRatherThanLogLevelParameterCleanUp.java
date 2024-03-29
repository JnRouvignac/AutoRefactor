/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2019 Fabrice Tiercelin - initial API and implementation
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
import java.util.logging.Level;
import java.util.logging.Logger;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class NamedMethodRatherThanLogLevelParameterCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.NamedMethodRatherThanLogLevelParameterCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.NamedMethodRatherThanLogLevelParameterCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.NamedMethodRatherThanLogLevelParameterCleanUp_reason;
	}

	@Override
	public boolean visit(final MethodInvocation visited) {
		if (ASTNodes.usesGivenSignature(visited, Logger.class.getCanonicalName(), "log", Level.class.getCanonicalName(), String.class.getCanonicalName())) { //$NON-NLS-1$
			List<Expression> args= visited.arguments();

			if (args != null && args.size() == 2) {
				QualifiedName levelType= ASTNodes.as(args.get(0), QualifiedName.class);

				if (levelType != null) {
					String methodName;

					if (ASTNodes.isField(levelType, Level.class.getCanonicalName(), "SEVERE")) { //$NON-NLS-1$
						methodName= "severe"; //$NON-NLS-1$
					} else if (ASTNodes.isField(levelType, Level.class.getCanonicalName(), "WARNING")) { //$NON-NLS-1$
						methodName= "warning"; //$NON-NLS-1$
					} else if (ASTNodes.isField(levelType, Level.class.getCanonicalName(), "INFO")) { //$NON-NLS-1$
						methodName= "info"; //$NON-NLS-1$
					} else if (ASTNodes.isField(levelType, Level.class.getCanonicalName(), "FINE")) { //$NON-NLS-1$
						methodName= "fine"; //$NON-NLS-1$
					} else if (ASTNodes.isField(levelType, Level.class.getCanonicalName(), "FINER")) { //$NON-NLS-1$
						methodName= "finer"; //$NON-NLS-1$
					} else if (ASTNodes.isField(levelType, Level.class.getCanonicalName(), "FINEST")) { //$NON-NLS-1$
						methodName= "finest"; //$NON-NLS-1$
					} else {
						return true;
					}

					replaceLevelByMethodName(visited, methodName);
					return false;
				}
			}
		}

		return true;
	}

	private void replaceLevelByMethodName(final MethodInvocation visited, final String methodName) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.NamedMethodRatherThanLogLevelParameterCleanUp_description);

		rewrite.replace(visited.getName(), ast.newSimpleName(methodName), group);
		rewrite.remove((Expression) visited.arguments().get(0), group);
	}
}

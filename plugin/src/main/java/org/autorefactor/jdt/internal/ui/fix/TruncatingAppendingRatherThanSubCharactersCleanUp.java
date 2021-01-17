/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2017 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Make sure we do not visit again modified nodes
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
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class TruncatingAppendingRatherThanSubCharactersCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.TruncatingAppendingRatherThanSubCharactersCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.TruncatingAppendingRatherThanSubCharactersCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.TruncatingAppendingRatherThanSubCharactersCleanUp_reason;
	}

	@Override
	public boolean visit(final MethodInvocation visited) {
		if (visited.getExpression() != null
				&& "append".equals(visited.getName().getIdentifier()) //$NON-NLS-1$
				&& visited.arguments().size() == 1
				// Most expensive check comes last
				&& ASTNodes.hasType(visited.getExpression(), StringBuilder.class.getCanonicalName(), StringBuffer.class.getCanonicalName())) {
			MethodInvocation truncatingMethod= ASTNodes.as((Expression) visited.arguments().get(0), MethodInvocation.class);

			if (ASTNodes.usesGivenSignature(truncatingMethod, String.class.getCanonicalName(), "substring", int.class.getSimpleName(), int.class.getSimpleName()) //$NON-NLS-1$
					|| getJavaMinorVersion() >= 5 && ASTNodes.usesGivenSignature(truncatingMethod, CharSequence.class.getCanonicalName(), "subSequence", int.class.getSimpleName(), int.class.getSimpleName())) { //$NON-NLS-1$
				replaceWithAppendSubstring(visited, truncatingMethod);
				return false;
			}
		}

		return true;
	}

	private void replaceWithAppendSubstring(final MethodInvocation visited, final MethodInvocation truncatingMethod) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.TruncatingAppendingRatherThanSubCharactersCleanUp_description);

		Expression builder= ASTNodes.createMoveTarget(rewrite, visited.getExpression());
		Expression stringVar= ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(truncatingMethod.getExpression()));
		List<Expression> args= truncatingMethod.arguments();
		Expression arg0= ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(args.get(0)));
		Expression arg1= ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(args.get(1)));

		MethodInvocation appendMethod= ast.newMethodInvocation();
		appendMethod.setExpression(builder);
		appendMethod.setName(ast.newSimpleName("append")); //$NON-NLS-1$
		appendMethod.arguments().add(stringVar);
		appendMethod.arguments().add(arg0);
		appendMethod.arguments().add(arg1);

		ASTNodes.replaceButKeepComment(rewrite, visited, appendMethod, group);
	}
}

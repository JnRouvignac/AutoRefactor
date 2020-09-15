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

import org.eclipse.jdt.core.dom.SuperConstructorInvocation;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class RemoveEmptySuperConstrInvocationCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.RemoveEmptySuperConstrInvocationCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.RemoveEmptySuperConstrInvocationCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.RemoveEmptySuperConstrInvocationCleanUp_reason;
	}

	@Override
	public boolean visit(final SuperConstructorInvocation node) {
		if (node.arguments().isEmpty()) {
			TextEditGroup group= new TextEditGroup(MultiFixMessages.RemoveEmptySuperConstrInvocationCleanUp_description);
			cuRewrite.getASTRewrite().removeButKeepComment(node, group);
			return false;
		}

		return true;
	}
}

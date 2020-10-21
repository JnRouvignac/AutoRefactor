/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - initial API and implementation
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
package org.autorefactor.jdt.internal.corext.dom;

import java.util.Set;

import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.SimpleName;

/**
 * The variable occurrence visitor.
 */
public class VarOccurrenceVisitor extends InterruptibleVisitor {
	private final Set<SimpleName> localVarIds;
	private boolean varUsed;
	private ASTNode startNode;
	private final boolean includeInnerScopes;

	/**
	 * Returns true if at least one variable is used.
	 *
	 * @return True if at least one variable is used
	 */
	public boolean isVarUsed() {
		return varUsed;
	}

	/**
	 * The constructor.
	 *
	 * @param localVarIds The ids of the variable to search
	 * @param includeInnerScopes True if the sub blocks should be analyzed
	 */
	public VarOccurrenceVisitor(final Set<SimpleName> localVarIds, final boolean includeInnerScopes) {
		this.localVarIds= localVarIds;
		this.includeInnerScopes= includeInnerScopes;
	}

	@Override
	public void traverseNodeInterruptibly(final ASTNode startNode) {
		this.startNode= startNode;
		super.traverseNodeInterruptibly(this.startNode);
	}

	@Override
	public boolean visit(final SimpleName aVariable) {
		for (SimpleName localVarId : localVarIds) {
			if (Utils.equalNotNull(localVarId.getIdentifier(), aVariable.getIdentifier())) {
				varUsed= true;
				return interruptVisit();
			}
		}

		return true;
	}

	@Override
	public boolean visit(final Block node) {
		return startNode == node || includeInnerScopes;
	}
}

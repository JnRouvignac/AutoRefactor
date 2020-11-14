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

import java.util.HashSet;
import java.util.Set;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.SimpleName;

/**
 * The variable conflict visitor.
 */
public class VarConflictVisitor extends InterruptibleVisitor {
	private final Set<String> localVariableIds;
	private boolean varConflicting;
	private ASTNode startNode;
	private final boolean includeInnerScopes;

	/**
	 * The constructor.
	 *
	 * @param localVariables The variables that may have the same name as others
	 * @param includeInnerScopes True if the sub blocks should be analyzed
	 */
	public VarConflictVisitor(final Set<SimpleName> localVariables, final boolean includeInnerScopes) {
		this.includeInnerScopes= includeInnerScopes;
		this.localVariableIds= new HashSet<>(localVariables.size());

		for (SimpleName localVariable : localVariables) {
			this.localVariableIds.add(localVariable.getIdentifier());
		}
	}

	/**
	 * Returns true if at least one variable is used.
	 *
	 * @return True if at least one variable is used
	 */
	public boolean isVarConflicting() {
		return varConflicting;
	}

	@Override
	public void traverseNodeInterruptibly(final ASTNode aStartNode) {
		this.startNode= aStartNode;
		super.traverseNodeInterruptibly(this.startNode);
	}

	@Override
	public boolean visit(final SimpleName concurrentVariable) {
		if (concurrentVariable.resolveBinding() == null || concurrentVariable.resolveBinding().getKind() == IBinding.VARIABLE) {
			if (localVariableIds.contains(concurrentVariable.getIdentifier())) {
				varConflicting= true;
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

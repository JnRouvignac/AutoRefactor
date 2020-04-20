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
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

/**
 * Visitor collecting all definitions of any variable.
 */
public class VarDeclarationIdentifierVisitor extends ASTVisitor {
	private final Set<SimpleName> variableNames= new HashSet<>();
	private final ASTNode startNode;
	private final boolean includeInnerScopes;

	/**
	 * The constructor.
	 *
	 * @param startNode       the {@link ASTNode} which is the scope of the search
	 * @param includeInnerScopes True if the sub blocks should be analyzed
	 */
	public VarDeclarationIdentifierVisitor(final ASTNode startNode, final boolean includeInnerScopes) {
		this.startNode= startNode;
		this.includeInnerScopes= includeInnerScopes;
	}

	/**
	 * Get the variable names.
	 *
	 * @return the variable names.
	 */
	public Set<SimpleName> getVariableNames() {
		return variableNames;
	}

	@Override
	public boolean visit(final SingleVariableDeclaration node) {
		variableNames.add(node.getName());
		return true;
	}

	@Override
	public boolean visit(final VariableDeclarationFragment node) {
		variableNames.add(node.getName());
		return true;
	}

	@Override
	public boolean visit(final Block node) {
		return startNode == node || includeInnerScopes;
	}
}

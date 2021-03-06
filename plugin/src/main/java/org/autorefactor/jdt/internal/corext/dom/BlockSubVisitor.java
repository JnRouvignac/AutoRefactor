/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Jean-Noël Rouvignac - initial API and implementation
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

import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Block;

/**
 * A visitor to refactor several sibling nodes and flag the parent block to
 * visited.
 */
public class BlockSubVisitor extends ASTVisitor {
	/**
	 * The first node to visit.
	 */
	protected Block startNode;

	/**
	 * The result.
	 */
	public boolean result= true;

	/**
	 * Should be used to visit a node.
	 *
	 * @param node The node to visit
	 */
	public void visitNode(final Block node) {
		this.startNode= node;
		node.accept(this);
	}

	@Override
	public boolean visit(final Block node) {
		return startNode == node;
	}
}

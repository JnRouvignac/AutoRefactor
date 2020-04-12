/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice TIERCELIN - Initial API and implementation
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

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;

/**
 * An interruptible visitor.
 */
public class InterruptibleVisitor extends ASTVisitor {
	/**
	 * Visit the node.
	 *
	 * @param node The visited node.
	 */
	public void visitNode(final ASTNode node) {
		try {
			node.accept(this);
		} catch (AbortSearchException e) {
			return;
		}
	}

	/**
	 * Interrupt the visit of a tree.
	 *
	 * @return nothing
	 */
	public boolean interruptVisit() {
		throw new AbortSearchException();
	}
}

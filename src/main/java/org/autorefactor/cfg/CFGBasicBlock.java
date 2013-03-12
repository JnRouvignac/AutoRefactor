/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.cfg;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.CompilationUnit;

/**
 * Control Flow Graph Basic Block. Basic blocks here are a little different from
 * the normal definition of "all adjacent statements not separated by a jump".
 * Here, things like for statement initializers, if conditions, while conditions
 * all receive their own basic block in order to be able to link variable use to
 * one basic block.
 * 
 * @author jnrouvignac
 * @see <a href="http://en.wikipedia.org/wiki/Control_flow_graph">Control flow
 *      graph on wikipedia</a>
 * @see <a href="http://en.wikipedia.org/wiki/Basic_block">Basic block on
 *      wikipedia</a>
 */
public class CFGBasicBlock {

	/** The ASTNode owning this basic block */
	private ASTNode node;
	private final Collection<CFGEdge> incomingEdges = new LinkedList<CFGEdge>();
	private final Collection<CFGEdge> outgoingEdges = new LinkedList<CFGEdge>();
	private final List<VariableAccess> variableAccesses = new ArrayList<VariableAccess>();
	public boolean isEntryBlock;
	public boolean isExitBlock;

	public CFGBasicBlock() {
		this.isExitBlock = true;
	}

	public CFGBasicBlock(ASTNode node) {
		this.node = node;
	}

	/** @return the ASTNode owning this basic block */
	public ASTNode getASTNode() {
		return this.node;
	}

	public void addIncomingEdge(CFGEdge edge) {
		this.incomingEdges.add(edge);
	}

	public void addOutgoingEdge(CFGEdge edge) {
		this.outgoingEdges.add(edge);
	}

	public void addVariableAccess(VariableAccess varAccess) {
		this.variableAccesses.add(varAccess);
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		toStringNoOutgoingEdges(sb);

		for (CFGEdge outgoing : this.outgoingEdges) {
			sb.append("\n\t");
			CFGEdge.arcToString(sb, outgoing.getCondition());
			outgoing.getTargetBlock().toStringNoOutgoingEdges(sb);
		}

		for (CFGEdge outgoing : this.outgoingEdges) {
			sb.append("\n");
			sb.append("\n");
			sb.append(outgoing.getTargetBlock());
		}

		return sb.toString();
	}

	private void toStringNoOutgoingEdges(final StringBuilder sb) {
		if (this.node != null) {
			if (this.node.getRoot() instanceof CompilationUnit) {
				CompilationUnit cu = (CompilationUnit) this.node.getRoot();
				sb.append(cu.getTypeRoot().getElementName()).append(":");
			}
			sb.append(this.node.getStartPosition());
			final String s = this.node.toString();
			sb.append(" - ").append(s.substring(0, s.indexOf('\n')));
		}
		if (this.isExitBlock) {
			sb.append("EXIT");
		}
	}
}

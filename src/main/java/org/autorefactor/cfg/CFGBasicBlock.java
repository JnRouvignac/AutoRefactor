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

import java.util.Collection;
import java.util.LinkedList;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.MethodDeclaration;

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
	private int line;
	private int column;
	private final Collection<CFGEdge> incomingEdges = new LinkedList<CFGEdge>();
	private final Collection<Object> outgoingEdgesAndVariableAccesses = new LinkedList<Object>();
	private Boolean isEntryBlock;

	private CFGBasicBlock(boolean isEntry, MethodDeclaration node, int line, int column) {
		this.isEntryBlock = isEntry;
		this.node = node;
		this.line = line;
		this.column = column;
	}

	public CFGBasicBlock(ASTNode node) {
		this.node = node;
	}

	public CFGBasicBlock(ASTNode node, int lineNumber, int column) {
		this.node = node;
		this.line = lineNumber;
		this.column = column;
	}

	public static CFGBasicBlock buildEntryBlock(MethodDeclaration node) {
		return new CFGBasicBlock(true, node, 1, 1);
	}

	public static CFGBasicBlock buildExitBlock(MethodDeclaration node, int line, int column) {
		return new CFGBasicBlock(false, node, line, column);
	}

	/** @return the ASTNode owning this basic block */
	public ASTNode getASTNode() {
		return this.node;
	}

	public int getColumn() {
		return column;
	}

	public int getLine() {
		return line;
	}

	public boolean isEntryBlock() {
		return Boolean.TRUE.equals(this.isEntryBlock);
	}

	public boolean isExitBlock() {
		return Boolean.FALSE.equals(this.isEntryBlock);
	}

	public Collection<Object> getOutgoingEdgesAndVariableAccesses() {
		return outgoingEdgesAndVariableAccesses;
	}

	public void addIncomingEdge(CFGEdge edge) {
		if (edge.getTargetBlock() != this) {
			throw new IllegalArgumentException(
					"Error: the target block of this incoming edge is not the current block: "
							+ edge);
		}
		if (!this.incomingEdges.add(edge)) {
			throw new IllegalArgumentException(
					"Error: duplicate incoming edge:" + edge);
		}
	}

	public void addOutgoingEdge(CFGEdge edge) {
		if (edge.getSourceBlock() != this) {
			throw new IllegalArgumentException(
					"Error: the source block of this outgoing edge is not the current block");
		}
		if (!this.outgoingEdgesAndVariableAccesses.add(edge)) {
			throw new IllegalArgumentException(
					"Error: duplicate outgoing edge:" + edge);
		}
	}

	public void addVariableAccess(VariableAccess varAccess) {
		this.outgoingEdgesAndVariableAccesses.add(varAccess);
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		toString(sb);
		return sb.toString();
	}

	private void toString(final StringBuilder sb) {
		if (this.node == null) {
			return;
		}
		// append this node's identity
		String fileName = getFileName();
		if (fileName != null) {
			sb.append(fileName).append(":");
			sb.append(this.node.getStartPosition()).append(" - ");
			appendSummary(sb);
		}

		// append the sub nodes identity
		for (Object obj : this.outgoingEdgesAndVariableAccesses) {
			sb.append('\n');
			if (obj instanceof CFGEdge) {
				((CFGEdge) obj).getTargetBlock().toString(sb);
			} else if (obj instanceof VariableAccess) {
				((VariableAccess) obj).toString(sb);
			} else {
				throw new RuntimeException(
						"Not implemented for " + obj != null ? obj.getClass()
								.getSimpleName() : null);
			}
		}
		sb.append("\n");
		if (fileName != null) {
			sb.append(fileName).append(":");
			sb.append(this.node.getStartPosition() + this.node.getLength());
			sb.append(" - ");
		}
		sb.append("}");
	}

	void appendSummary(final StringBuilder sb) {
		final String s = this.node.toString();
		final int idx = s.indexOf('\n');
		if (idx != -1) {
			sb.append(s.substring(0, idx));
		} else {
			sb.append(s);
		}
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + column;
		result = prime * result
				+ ((isEntryBlock == null) ? 0 : isEntryBlock.hashCode());
		result = prime * result + line;
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		CFGBasicBlock other = (CFGBasicBlock) obj;
		if (column != other.column)
			return false;
		if (isEntryBlock == null) {
			if (other.isEntryBlock != null)
				return false;
		} else if (!isEntryBlock.equals(other.isEntryBlock))
			return false;
		if (line != other.line)
			return false;
		return true;
	}

	String getFileName() {
		if (this.node.getRoot() instanceof CompilationUnit) {
			CompilationUnit cu = (CompilationUnit) this.node.getRoot();
			return cu.getTypeRoot().getElementName();
		}
		return null;
	}
}

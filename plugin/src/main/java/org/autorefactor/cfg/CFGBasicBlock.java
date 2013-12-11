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

/**
 * Control Flow Graph Basic Block. Basic blocks here are a little different from
 * the normal definition of "all adjacent statements not separated by a jump".
 * Here, things like for statement initializers, if conditions, while conditions
 * all receive their own basic block in order to be able to link variable uses to
 * one basic block. It also mixes in lexical scoping.
 *
 * @author jnrouvignac
 * @see <a href="http://en.wikipedia.org/wiki/Control_flow_graph">Control flow
 *      graph on wikipedia</a>
 * @see <a href="http://en.wikipedia.org/wiki/Basic_block">Basic block on
 *      wikipedia</a>
 */
public class CFGBasicBlock implements Comparable<CFGBasicBlock> {

	private final ASTNode node;
	private final String fileName;
	private final String codeExcerpt;
	private final boolean isDecision;
	private final Boolean isEntryBlock;
	private final LineAndColumn lineAndColumn;
	private final Collection<CFGEdge> incomingEdges = new LinkedList<CFGEdge>();
	private final Collection<Object> outgoingEdgesAndVariableAccesses = new LinkedList<Object>();

	private CFGBasicBlock(ASTNode node, String fileName, String codeExcerpt, boolean isDecision, Boolean isEntryBlock,
			LineAndColumn lineAndColumn) {
		this.node = node;
		this.fileName = fileName;
		this.codeExcerpt = codeExcerpt;
		this.isDecision = isDecision;
		this.isEntryBlock = isEntryBlock;
		this.lineAndColumn = lineAndColumn;
	}

	public CFGBasicBlock(ASTNode node, String fileName, String codeExcerpt, boolean isDecision, LineAndColumn lineAndColumn) {
		this(node, fileName, codeExcerpt, isDecision, null, lineAndColumn);
	}

	public static CFGBasicBlock buildEntryBlock(ASTNode node, String fileName, String codeExcerpt) {
		return new CFGBasicBlock(node, fileName, codeExcerpt, false, true, new LineAndColumn(0, 1, 1));
	}

	public static CFGBasicBlock buildExitBlock(ASTNode node, String fileName, String codeExcerpt, LineAndColumn lineAndColumn) {
		return new CFGBasicBlock(node, fileName, codeExcerpt, false, false, lineAndColumn);
	}

	public LineAndColumn getLineAndColumn() {
		return lineAndColumn;
	}

	public ASTNode getNode() {
		return node;
	}

	public boolean isDecision() {
		return this.isDecision;
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
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((fileName == null) ? 0 : fileName.hashCode());
		result = prime * result + ((lineAndColumn == null) ? 0 : lineAndColumn.hashCode());;
		result = prime * result
				+ ((isEntryBlock == null) ? 0 : isEntryBlock.hashCode());
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
		if (fileName == null) {
			if (other.fileName != null)
				return false;
		} else if (!fileName.equals(other.fileName))
			return false;
		if (lineAndColumn == null) {
			if (other.lineAndColumn != null)
				return false;
		} else if (!lineAndColumn.equals(other.lineAndColumn))
			return false;
		if (isEntryBlock == null) {
			if (other.isEntryBlock != null)
				return false;
		} else if (!isEntryBlock.equals(other.isEntryBlock))
			return false;
		return true;
	}

	/** {@inheritDoc} */
	public int compareTo(CFGBasicBlock o)
	{
		final Integer startPosition = lineAndColumn.getStartPosition();
		return startPosition.compareTo(o.lineAndColumn.getStartPosition());
	}

	public String getFileName() {
		return this.fileName;
	}

	public String getCodeExcerpt() {
		return codeExcerpt;
	}

	String getDotNodeLabel() {
		final StringBuilder sb = new StringBuilder();
		appendDotNodeLabel(sb);
		return sb.toString();
	}

	StringBuilder appendDotNodeId(StringBuilder sb) {
		if (isEntryBlock()) {
			sb.append("Entry");
		} else if (isExitBlock()) {
			sb.append("Exit");
		} else {
			LineAndColumn lal = this.lineAndColumn;
			sb.append("_").append(lal.getLine()).append("_").append(lal.getColumn());
		}
		return sb;
	}

	StringBuilder appendDotNodeLabel(StringBuilder sb) {
		sb.append(this.codeExcerpt).append("\\n(");
		LineAndColumn lal = this.lineAndColumn;
		sb.append(lal.getLine()).append(",").append(lal.getColumn()).append(")");
		return sb;
	}

	StringBuilder appendDotNodeSourcePosition(StringBuilder sb) {
//		LineAndColumn lal = this.lineAndColumn;
//		sb.append("(").append(lal.getLine()).append(",").append(lal.getColumn()).append(")");
		return sb;
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder("BLOCK[");
		toString(sb);
		return sb.append("]").toString();
	}

	private void toString(final StringBuilder sb) {
		if (this.codeExcerpt == null) {
			return;
		}
		appendDotNodeLabel(sb);
	}

}

/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2014 Jean-Noël Rouvignac - initial API and implementation
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
import java.util.Objects;

import org.autorefactor.util.IllegalArgumentException;
import org.eclipse.jdt.core.dom.ASTNode;

/**
 * Control Flow Graph Basic Block. Basic blocks here are a little different from
 * the normal definition of "all adjacent statements not separated by a jump".
 * Here, things like for statement initializers, if conditions, while conditions
 * all receive their own basic block in order to be able to link variable uses
 * to one basic block. It also mixes in lexical scoping.
 *
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
	/**
	 * true means entry block, false means exit block, null means neither entry nor
	 * exit block.
	 */
	private final Boolean isEntryBlock;
	private final LineAndColumn lineAndColumn;
	private final Collection<CFGEdge> incomingEdges= new LinkedList<>();
	private final Collection<Object> outgoingEdgesAndVariableAccesses= new LinkedList<>();

	private CFGBasicBlock(final ASTNode node, final String fileName, final String codeExcerpt, final boolean isDecision, final Boolean isEntryBlock,
			final LineAndColumn lineAndColumn) {
		this.node= node;
		this.fileName= fileName;
		this.codeExcerpt= codeExcerpt;
		this.isDecision= isDecision;
		this.isEntryBlock= isEntryBlock;
		this.lineAndColumn= lineAndColumn;
	}

	/**
	 * Constructor for a new block.
	 *
	 * @param node          the AST node that led to the creation of this block
	 * @param fileName      the file name where this block is coming from
	 * @param codeExcerpt   a code excerpt to display for this block
	 * @param isDecision    whether this block is a decision block
	 * @param lineAndColumn the line and column information for this block
	 */
	public CFGBasicBlock(final ASTNode node, final String fileName, final String codeExcerpt, final boolean isDecision,
			final LineAndColumn lineAndColumn) {
		this(node, fileName, codeExcerpt, isDecision, null, lineAndColumn);
	}

	/**
	 * Builds and returns a new entry block.
	 *
	 * @param node        the AST node that led to the creation of this entry block
	 * @param fileName    the file name where this entry block is coming from
	 * @param codeExcerpt a code excerpt to display for this block
	 * @return a new entry block
	 */
	public static CFGBasicBlock buildEntryBlock(final ASTNode node, final String fileName, final String codeExcerpt) {
		return new CFGBasicBlock(node, fileName, codeExcerpt, false, true, new LineAndColumn(0, 1, 1));
	}

	/**
	 * Builds and returns a new exit block.
	 *
	 * @param node          the AST node that led to the creation of this exit block
	 * @param fileName      the file name where this exit block is coming from
	 * @param codeExcerpt   a code excerpt to display for this block
	 * @param lineAndColumn the line and column information for this exit block
	 * @return a new exit block
	 */
	public static CFGBasicBlock buildExitBlock(final ASTNode node, final String fileName, final String codeExcerpt,
			final LineAndColumn lineAndColumn) {
		return new CFGBasicBlock(node, fileName, codeExcerpt, false, false, lineAndColumn);
	}

	/**
	 * Returns the line and column information of this block.
	 *
	 * @return the line and column information of this block
	 */
	public LineAndColumn getLineAndColumn() {
		return lineAndColumn;
	}

	/**
	 * Returns the AST node represented by this basic block.
	 *
	 * @return the AST node represented by this basic block
	 */
	public ASTNode getNode() {
		return node;
	}

	/**
	 * Returns whether this block is a decision block.
	 *
	 * @return true if this block is a decision block, false otherwise
	 */
	public boolean isDecision() {
		return this.isDecision;
	}

	/**
	 * Returns whether this basic block is the entry block of a method.
	 *
	 * @return true if this basic block is an entry block, false otherwise
	 */
	public boolean isEntryBlock() {
		return Boolean.TRUE.equals(this.isEntryBlock);
	}

	/**
	 * Returns whether this basic block is the exit block of a method.
	 *
	 * @return true if this basic block is an exit block, false otherwise
	 */
	public boolean isExitBlock() {
		return Boolean.FALSE.equals(this.isEntryBlock);
	}

	/**
	 * Returns a collection of the outgoing edges and variable accesses of this
	 * basic block.
	 *
	 * @return a collection of the outgoing edges and variable accesses of this
	 *         basic block.
	 */
	public Collection<Object> getOutgoingEdgesAndVariableAccesses() {
		return outgoingEdgesAndVariableAccesses;
	}

	/**
	 * Adds an incoming edge to this basic block.
	 *
	 * @param edge an incoming edge to this basic block
	 */
	public void addIncomingEdge(final CFGEdge edge) {
		if (edge.getTargetBlock() != this) {
			throw new IllegalArgumentException(null,
					"Error: the target block of this incoming edge is not the current block: " + edge); //$NON-NLS-1$
		}
		if (!this.incomingEdges.add(edge)) {
			throw new IllegalArgumentException(null, "Error: duplicate incoming edge:" + edge); //$NON-NLS-1$
		}
	}

	/**
	 * Adds an outgoing edge to this basic block.
	 *
	 * @param edge an outgoing edge from this basic block
	 */
	public void addOutgoingEdge(final CFGEdge edge) {
		if (edge.getSourceBlock() != this) {
			throw new IllegalArgumentException(null,
					"Error: the source block of this outgoing edge is not the current block"); //$NON-NLS-1$
		}
		if (!this.outgoingEdgesAndVariableAccesses.add(edge)) {
			throw new IllegalArgumentException(null, "Error: duplicate outgoing edge:" + edge); //$NON-NLS-1$
		}
	}

	/**
	 * Adds a variable access to this basic block.
	 *
	 * @param varAccess the variable access to add to this basic block
	 */
	public void addVariableAccess(final VariableAccess varAccess) {
		this.outgoingEdgesAndVariableAccesses.add(varAccess);
	}

	@Override
	public int hashCode() {
		return Objects.hash(fileName, lineAndColumn, isEntryBlock);
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}
		CFGBasicBlock other= (CFGBasicBlock) obj;
		return Objects.equals(fileName, other.fileName) && Objects.equals(lineAndColumn, other.lineAndColumn)
				&& Objects.equals(isEntryBlock, other.isEntryBlock);
	}

	@Override
	public int compareTo(final CFGBasicBlock o) {
		Integer startPosition= lineAndColumn.getStartPosition();
		return startPosition.compareTo(o.lineAndColumn.getStartPosition());
	}

	/**
	 * Returns the file name containing this basic block.
	 *
	 * @return the file name containing this basic block
	 */
	public String getFileName() {
		return this.fileName;
	}

	/**
	 * Returns a code excerpt for this basic block.
	 *
	 * @return a code excerpt for this basic block
	 */
	public String getCodeExcerpt() {
		return codeExcerpt;
	}

	/**
	 * Returns the node label in the DOT format.
	 *
	 * @return the node label in the DOT format
	 */
	String getDotNodeLabel() {
		StringBuilder sb= new StringBuilder();
		appendDotNodeLabel(sb);
		return sb.toString();
	}

	/**
	 * Appends the node id in the DOT format and returns the provided string
	 * builder.
	 *
	 * @param sb the string builder where to append the node id
	 * @return the provided string builder
	 */
	StringBuilder appendDotNodeId(final StringBuilder sb) {
		if (isEntryBlock()) {
			sb.append("Entry"); //$NON-NLS-1$
		} else if (isExitBlock()) {
			sb.append("Exit"); //$NON-NLS-1$
		} else {
			LineAndColumn lal= this.lineAndColumn;
			sb.append("_").append(lal.getLine()).append("_").append(lal.getColumn()); //$NON-NLS-1$ //$NON-NLS-2$
		}

		return sb;
	}

	/**
	 * Appends the node label in the DOT format and returns the provided string
	 * builder.
	 *
	 * @param sb the string builder where to append the node label
	 * @return the provided string builder
	 */
	StringBuilder appendDotNodeLabel(final StringBuilder sb) {
		sb.append(this.codeExcerpt).append("\\n("); //$NON-NLS-1$
		LineAndColumn lal= this.lineAndColumn;
		sb.append(lal.getLine()).append(",").append(lal.getColumn()).append(")"); //$NON-NLS-1$ //$NON-NLS-2$
		return sb;
	}

	@Override
	public String toString() {
		StringBuilder sb= new StringBuilder("BLOCK["); //$NON-NLS-1$
		toString(sb);
		return sb.append("]").toString(); //$NON-NLS-1$
	}

	private void toString(final StringBuilder sb) {
		if (this.codeExcerpt == null) {
			return;
		}
		appendDotNodeLabel(sb);
	}
}

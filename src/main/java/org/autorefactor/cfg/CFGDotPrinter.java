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

import java.util.Comparator;
import java.util.Set;
import java.util.TreeSet;

import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.IfStatement;

/**
 * Outputs a string representing the CFG in the dot format.
 * <p>
 * Example command to output an image:
 * <pre>dot IfElseIfSample.dot -Tpng > IfElseIfSample.png</pre>
 * </p>
 */
public class CFGDotPrinter {

	// TODO JNR
	// - Add variable access nodes
	// - link them with "--" and/or inside a box
	// - Display expr in the box?
	// - node label of the form: var name + line, column + R, W, Decl
	// - use separate colors?

	// TODO JNR add one subgraph for each complex statements 
	// that deserve one (if, for, while, switch, etc.)

	// TODO JNR CFGBasicBlock: replace the ASTNode member with a 
	// String label (useful for for statement initializers + updaters)
	// TODO JNR CFGBasicBlock + CFGEdge + CFGEdgeBuilder
	// + VariableAccess: change toString() impl. 

	private final class CFGBasicBlockComparator implements
			Comparator<CFGBasicBlock> {
		public int compare(CFGBasicBlock o1, CFGBasicBlock o2) {
			if (o1.getLine() < o2.getLine()) {
				return -1;
			} else if (o1.getLine() > o2.getLine()) {
				return 1;
			}
			if (o1.getColumn() < o2.getColumn()) {
				return -1;
			} else if (o1.getColumn() > o2.getColumn()) {
				return 1;
			}
			return 0;
		}
	}

	private final class CFGEdgeComparator implements Comparator<CFGEdge> {

		private CFGBasicBlockComparator c = new CFGBasicBlockComparator();

		public int compare(CFGEdge e1, CFGEdge e2) {
			final int cmp = c.compare(e1.getSourceBlock(), e2.getSourceBlock());
			if (cmp != 0) {
				return cmp;
			}
			return c.compare(e1.getTargetBlock(), e2.getTargetBlock());
		}
	}

	/**
	 * Returns a String representing the CFG in the dot format.
	 * 
	 * @param startblock
	 *            the block from where to start printing
	 * @return a String representing the CFG in the dot format.
	 */
	public String toDot(final CFGBasicBlock startblock) {
		final StringBuilder sb = new StringBuilder();
		appendDigraph(startblock, sb);
		appendSubgraph(startblock, sb);

		final Pair<Set<CFGBasicBlock>, Set<CFGEdge>> nodesAndEdges = collect(startblock);
		for (CFGBasicBlock block : nodesAndEdges.getFirst()) {
			appendDotNode(block, sb);
		}
		sb.append("\n");
		for (CFGEdge edge : nodesAndEdges.getSecond()) {
			appendDotEdge(edge, sb);
		}

		sb.append("}\n}\n");
		return sb.toString();
	}

	private Pair<Set<CFGBasicBlock>, Set<CFGEdge>> collect(CFGBasicBlock block) {
		final Set<CFGBasicBlock> blockSet = new TreeSet<CFGBasicBlock>(
				new CFGBasicBlockComparator());
		final Set<CFGEdge> edgeSet = new TreeSet<CFGEdge>(
				new CFGEdgeComparator());
		final Pair<Set<CFGBasicBlock>, Set<CFGEdge>> results = Pair.of(
				blockSet, edgeSet);
		collect(block, results);
		return results;
	}

	private void collect(CFGBasicBlock block,
			final Pair<Set<CFGBasicBlock>, Set<CFGEdge>> results) {
		if (!results.getFirst().add(block)) {
			// node was already added. No need to go through this path again
			return;
		}

		for (Object obj : block.getOutgoingEdgesAndVariableAccesses()) {
			if (obj instanceof CFGEdge) {
				final CFGEdge edge = (CFGEdge) obj;
				results.getSecond().add(edge);
				collect(edge.getTargetBlock(), results);
			}
		}
	}

	private StringBuilder appendDigraph(final CFGBasicBlock block,
			final StringBuilder sb) {
		final String fileName = block.getFileName();
		final String className = fileName.substring(0, fileName.indexOf('.'));
		sb.append("digraph ").append(className).append(" {\n");
		sb.append("label=\"").append(className).append("\";\n");
		return sb;
	}

	private StringBuilder appendSubgraph(final CFGBasicBlock block,
			final StringBuilder sb) {
		final String methodCodeExcerpt = codeExcerpt(block);
		String methodSignature = methodCodeExcerpt.replaceAll("\\W", "_");
		sb.append("subgraph cluster_").append(methodSignature).append(" {\n");
		sb.append("label=\"").append(methodCodeExcerpt).append("\";\n");
		return sb;
	}

	private boolean appendDotEdge(final CFGEdge edge, final StringBuilder sb) {
		appendDotNodeName(edge.getSourceBlock(), sb).append(" -> ");
		appendDotNodeName(edge.getTargetBlock(), sb);
		if (edge.getCondition() != null) {
			sb.append(" [label=\"").append(edge.getEvaluationResult())
					.append("\"];");
		}
		sb.append("\n");
		return true;
	}

	private void appendDotNode(CFGBasicBlock block, StringBuilder sb) {
		if (block.isEntryBlock()) {
			sb.append("Entry [style=\"filled\" fillcolor=\"red\"   fontcolor=\"white\"];\n");
		} else if (block.isExitBlock()) {
			sb.append("Exit  [style=\"filled\" fillcolor=\"black\" fontcolor=\"white\"];\n");
		} else {
			appendDotNodeName(block, sb);
			sb.append(" [label=\"").append(codeExcerpt(block));
			sb.append("\\n(").append(block.getLine()).append(",")
					.append(block.getColumn()).append(")").append("\"");
			if (block.getASTNode() instanceof IfStatement) {
				sb.append(",shape=\"triangle\"");
			}
			sb.append("];\n");
		}
	}

	private String codeExcerpt(CFGBasicBlock block) {
		final String nodeString = block.getASTNode().toString();
		final String[] nodeLines = nodeString.split("\n");
		final String codeExcerpt;
		if (nodeLines[0].matches("\\s*\\{\\s*")) {
			codeExcerpt = nodeLines[0] + " " + nodeLines[1] + " ...";
		} else {
			codeExcerpt = nodeLines[0];
		}
		return escape(codeExcerpt.replaceAll("\\s+", " "));
	}

	private String escape(String s) {
		return s != null ? s.replaceAll("\"", "\\\"") : null;
	}

	private StringBuilder appendDotNodeName(CFGBasicBlock block,
			StringBuilder sb) {
		if (block.isEntryBlock()) {
			sb.append("Entry");
		} else if (block.isExitBlock()) {
			sb.append("Exit");
		} else {
			sb.append("_").append(block.getLine()).append("_")
					.append(block.getColumn());
		}
		return sb;
	}

}

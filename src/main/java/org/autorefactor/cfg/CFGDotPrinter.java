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

import java.util.HashSet;
import java.util.Set;

import org.eclipse.jdt.core.dom.IfStatement;

public class CFGDotPrinter {

	private Set<Object> alreadyPrinted = new HashSet<Object>();

	public String toDot(final CFGBasicBlock block) {
		final String fileName = block.getFileName();
		final String className = fileName.substring(0, fileName.indexOf('.'));
		final StringBuilder sb = new StringBuilder();
		sb.append("digraph ").append(className).append(" {\n");
		sb.append("label=\"").append(className).append("\";\n");
		final String methodCodeExcerpt = codeExcerpt(block);
		String methodSignature = methodCodeExcerpt.replaceAll("\\W", "_");
		sb.append("subgraph cluster_").append(methodSignature).append(" {\n");
		sb.append("label=\"").append(methodCodeExcerpt).append("\";\n");
		toDot(block, sb);
		sb.append("}\n}\n");
		return sb.toString();
	}

	private void toDot(final CFGBasicBlock block, final StringBuilder sb) {
		appendDotNode(block, sb);

		// append the sub nodes identity
		for (Object obj : block.getOutgoingEdgesAndVariableAccesses()) {
			sb.append('\n');
			if (obj instanceof CFGEdge) {
				final CFGEdge cfgEdge = (CFGEdge) obj;
				if (appendDotEdge(cfgEdge, sb)) {
					// We already printed the source block
					// at the start of the current method
					toDot(cfgEdge.getTargetBlock(), sb);
				}
				// else we already printed the edge,
				// so the target block was also printed.
			}
		}
	}

	private boolean appendDotEdge(final CFGEdge edge, final StringBuilder sb) {
		if (!alreadyPrinted.add(edge)) {
			// do not print again already printed edge
			return false;
		}

		appendDotNodeName(edge.getSourceBlock(), sb).append(" -> ");
		appendDotNodeName(edge.getTargetBlock(), sb);
		if (edge.getCondition() != null) {
			// String condition =
			// escape(cfgEdge.getCondition().toString());
			// sb.append(" [label=\"").append(condition).append("\\n-> ");
			sb.append(" [label=\"").append(edge.getEvaluationResult())
					.append("\"];");
		}
		sb.append("\n");
		return true;
	}

	private void appendDotNode(CFGBasicBlock block, StringBuilder sb) {
		if (!alreadyPrinted.add(block)) {
			// do not print again already printed node
			return;
		}

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
			sb.append("];");
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

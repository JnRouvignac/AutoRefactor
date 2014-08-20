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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.autorefactor.refactoring.ASTHelper;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.CatchClause;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.SwitchStatement;
import org.eclipse.jdt.core.dom.TryStatement;

/**
 * Outputs a string representing the CFG in the dot format.
 * <p>
 * Example command to output an image:
 *
 * <pre>
 * dot IfElseIfSample.dot -Tpng > IfElseIfSample.png
 * </pre>
 * </p>
 */
public class CFGDotPrinter {

    // TODO JNR
    // - Add variable access nodes
    // - link them with "--" and/or inside a box
    // - Display expr in the box?
    // - node label of the form: var name + line, column + R, W, Decl
    // - use separate colors?

    private static final class CFGSubGraph {

        private final String codeExcerpt;
        private final int startPosition;
        final Set<CFGBasicBlock> blocks = new TreeSet<CFGBasicBlock>();
        final List<CFGSubGraph> subGraphs = new ArrayList<CFGDotPrinter.CFGSubGraph>();

        public CFGSubGraph(String codeExcerpt, int startPosition) {
            this.codeExcerpt = codeExcerpt;
            this.startPosition = startPosition;
        }
    }

    private static final class CFGEdgeComparator implements Comparator<CFGEdge> {

        public int compare(CFGEdge e1, CFGEdge e2) {
            final int cmp = e1.getSourceBlock().compareTo(e2.getSourceBlock());
            if (cmp != 0) {
                return cmp;
            }
            return e1.getTargetBlock().compareTo(e2.getTargetBlock());
        }
    }

    /**
     * Returns a String representing the CFG in the dot format.
     *
     * @param startBlock
     *            the block from where to start printing
     * @return a String representing the CFG in the dot format.
     */
    public String toDot(final CFGBasicBlock startBlock) {
        final Map<ASTNode, CFGSubGraph> subGraphs = new HashMap<ASTNode, CFGSubGraph>();
        final Set<CFGEdge> edges = new TreeSet<CFGEdge>(new CFGEdgeComparator());
        collect(startBlock, subGraphs, edges);
        final CFGSubGraph subGraph = subGraphs.get(startBlock.getNode());

        final StringBuilder sb = new StringBuilder();
        appendGraph(startBlock, subGraph, edges, sb);
        return sb.toString();
    }

    private void appendGraph(final CFGBasicBlock startblock,
            final CFGSubGraph graph, Set<CFGEdge> edges, final StringBuilder sb) {
        final boolean needDigraph = sb.length() == 0;
        if (needDigraph) {
            appendDigraph(startblock, sb);
            sb.append("\n");
        }
        if (!edges.isEmpty()) {
            for (CFGEdge edge : edges) {
                appendDotEdge(edge, sb);
            }
            sb.append("\n");
        }

        appendSubgraph(graph, sb);

        for (CFGBasicBlock block : graph.blocks) {
            appendDotNode(block, sb);
        }
        if (!graph.subGraphs.isEmpty()) {
            sb.append("\n");
            for (CFGSubGraph subGraph : graph.subGraphs) {
                appendGraph(startblock, subGraph, Collections.EMPTY_SET, sb);
            }
        }

        sb.append("}\n");
        if (needDigraph) {
            sb.append("}\n");
        }
    }

    private void collect(CFGBasicBlock block,
            Map<ASTNode, CFGSubGraph> subGraphs, Set<CFGEdge> edges) {
        CFGSubGraph blockSubGraph = getSubGraph(subGraphs, block.getNode());
        if (!blockSubGraph.blocks.add(block)) {
            // node was already added.
            // Avoid cycles: do not go through this path again
            return;
        }

        for (Object obj : block.getOutgoingEdgesAndVariableAccesses()) {
            if (obj instanceof CFGEdge) {
                final CFGEdge edge = (CFGEdge) obj;
                edges.add(edge);
                collect(edge.getTargetBlock(), subGraphs, edges);
            }
        }
    }

    private CFGSubGraph getSubGraph(Map<ASTNode, CFGSubGraph> subGraphs,
            ASTNode node) {
        if (node == null) {
            return null;
        }
        CFGSubGraph subGraph = subGraphs.get(node);
        if (subGraph == null) {
            if (ASTHelper.isLoop(node)
                    || node instanceof IfStatement
                    || node instanceof SwitchStatement
                    || node instanceof MethodDeclaration
                    || node instanceof TryStatement
                    || node instanceof CatchClause) {
                // such statements need their own subgraph to ease reading the CFG
                subGraph = new CFGSubGraph(ASTPrintHelper.codeExcerpt(node),
                        node.getStartPosition());
                subGraphs.put(node, subGraph);
                // builds all sub graphs all the way to the top node
                CFGSubGraph parentSubGraph = getSubGraph(subGraphs,
                        node.getParent());
                if (parentSubGraph != null) {
                    parentSubGraph.subGraphs.add(subGraph);
                }
            } else {
                return getSubGraph(subGraphs, node.getParent());
            }
        }
        return subGraph;
    }

    private StringBuilder appendDigraph(final CFGBasicBlock block,
            final StringBuilder sb) {
        final String fileName = block.getFileName();
        final String className = fileName.substring(0, fileName.indexOf('.'));
        sb.append("digraph ").append(className).append(" {\n");
        sb.append("label=\"").append(className).append("\";\n");
        return sb;
    }

    private StringBuilder appendSubgraph(final CFGSubGraph graph,
            final StringBuilder sb) {
        final String blockCodeExcerpt = escape(graph.codeExcerpt);
        String clusterName = blockCodeExcerpt.replaceAll("\\W", "_");
        sb.append("subgraph cluster_").append(graph.startPosition).append("_")
                .append(clusterName).append(" {\n");
        sb.append("label=\"").append(blockCodeExcerpt).append("\";\n");
        return sb;
    }

    private boolean appendDotEdge(final CFGEdge edge, final StringBuilder sb) {
        edge.getSourceBlock().appendDotNodeId(sb).append(" -> ");
        edge.getTargetBlock().appendDotNodeId(sb);
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
            block.appendDotNodeId(sb);
            sb.append(" [label=\"").append(escape(block.getDotNodeLabel())).append("\"");
            if (block.isDecision()) {
                sb.append(",shape=\"triangle\"");
            }
            sb.append("];\n");
            // block.appendDotNodeSourcePosition(sb);
        }
    }

    private String escape(String s) {
        return s != null ? s.replaceAll("\"", "\\\"") : null;
    }

}

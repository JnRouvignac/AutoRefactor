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

import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
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
    // - Display expression in the box?
    // - node label of the form: var name + line, column + R, W, Decl
    // - use separate colors?

    private static final class CFGSubGraph {
        private final String codeExcerpt;
        private final int startPosition;
        final Set<CFGBasicBlock> blocks= new TreeSet<>();
        final List<CFGSubGraph> subGraphs= new ArrayList<>();

        public CFGSubGraph(final String codeExcerpt, final int startPosition) {
            this.codeExcerpt= codeExcerpt;
            this.startPosition= startPosition;
        }
    }

    private static final class CFGEdgeComparator implements Comparator<CFGEdge> {
        @Override
        public int compare(final CFGEdge e1, final CFGEdge e2) {
            int cmp= e1.getSourceBlock().compareTo(e2.getSourceBlock());
            if (cmp != 0) {
                return cmp;
            }

            return e1.getTargetBlock().compareTo(e2.getTargetBlock());
        }
    }

    /**
     * Returns a String representing the CFG in the dot format.
     *
     * @param startBlock the block from where to start printing
     * @return a String representing the CFG in the dot format.
     */
    public String toDot(final CFGBasicBlock startBlock) {
        Map<ASTNode, CFGSubGraph> subGraphs= new HashMap<>();
        Set<CFGEdge> edges= new TreeSet<>(new CFGEdgeComparator());
        collect(startBlock, subGraphs, edges);
        CFGSubGraph subGraph= subGraphs.get(startBlock.getNode());

        StringBuilder sb= new StringBuilder();
        appendGraph(startBlock, subGraph, edges, sb);
        return sb.toString();
    }

    private void appendGraph(final CFGBasicBlock startblock, final CFGSubGraph graph, final Set<CFGEdge> edges,
            final StringBuilder sb) {
        boolean needDigraph= sb.length() == 0;
        if (needDigraph) {
            appendDigraph(startblock, sb);
            sb.append("\n"); //$NON-NLS-1$
        }
        if (!edges.isEmpty()) {
            for (CFGEdge edge : edges) {
                appendDotEdge(edge, sb);
            }
            sb.append("\n"); //$NON-NLS-1$
        }

        appendSubgraph(graph, sb);

        for (CFGBasicBlock block : graph.blocks) {
            appendDotNode(block, sb);
        }
        if (!graph.subGraphs.isEmpty()) {
            sb.append("\n"); //$NON-NLS-1$
            for (CFGSubGraph subGraph : graph.subGraphs) {
                appendGraph(startblock, subGraph, Collections.EMPTY_SET, sb);
            }
        }

        sb.append("}\n"); //$NON-NLS-1$
        if (needDigraph) {
            sb.append("}\n"); //$NON-NLS-1$
        }
    }

    private void collect(final CFGBasicBlock block, final Map<ASTNode, CFGSubGraph> subGraphs, final Set<CFGEdge> edges) {
        CFGSubGraph blockSubGraph= getSubGraph(subGraphs, block.getNode());
        if (!blockSubGraph.blocks.add(block)) {
            // Node was already added.
            // Avoid cycles: do not go through this path again
            return;
        }

        for (Object obj : block.getOutgoingEdgesAndVariableAccesses()) {
            if (obj instanceof CFGEdge) {
                CFGEdge edge= (CFGEdge) obj;
                edges.add(edge);
                collect(edge.getTargetBlock(), subGraphs, edges);
            }
        }
    }

    private CFGSubGraph getSubGraph(final Map<ASTNode, CFGSubGraph> subGraphs, final ASTNode node) {
        if (node == null) {
            return null;
        }
        CFGSubGraph subGraph= subGraphs.get(node);
        if (subGraph == null) {
            if (!ASTNodes.isLoop(node) && !(node instanceof IfStatement) && !(node instanceof SwitchStatement)
                    && !(node instanceof MethodDeclaration) && !(node instanceof TryStatement) && !(node instanceof CatchClause)) {
                return getSubGraph(subGraphs, node.getParent());
            }
            // Such statements need their own subgraph to ease reading the CFG
            subGraph= new CFGSubGraph(ASTPrintHelper.codeExcerpt(node), node.getStartPosition());
            subGraphs.put(node, subGraph);
            // Builds all sub graphs all the way to the top node
            CFGSubGraph parentSubGraph= getSubGraph(subGraphs, node.getParent());
            if (parentSubGraph != null) {
                parentSubGraph.subGraphs.add(subGraph);
            }
        }

        return subGraph;
    }

    private StringBuilder appendDigraph(final CFGBasicBlock block, final StringBuilder sb) {
        String fileName= block.getFileName();
        String className= fileName.substring(0, fileName.indexOf('.'));
        sb.append("digraph ").append(className).append(" {\n"); //$NON-NLS-1$ //$NON-NLS-2$
        sb.append("label=\"").append(className).append("\";\n"); //$NON-NLS-1$ //$NON-NLS-2$
        return sb;
    }

    private StringBuilder appendSubgraph(final CFGSubGraph graph, final StringBuilder sb) {
        String blockCodeExcerpt= escape(graph.codeExcerpt);
        String clusterName= blockCodeExcerpt.replaceAll("\\W", "_"); //$NON-NLS-1$ //$NON-NLS-2$
        sb.append("subgraph cluster_").append(graph.startPosition).append("_").append(clusterName).append(" {\n"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        sb.append("label=\"").append(blockCodeExcerpt).append("\";\n"); //$NON-NLS-1$ //$NON-NLS-2$
        return sb;
    }

    private boolean appendDotEdge(final CFGEdge edge, final StringBuilder sb) {
        edge.getSourceBlock().appendDotNodeId(sb).append(" -> "); //$NON-NLS-1$
        edge.getTargetBlock().appendDotNodeId(sb);
        if (edge.getCondition() != null) {
            sb.append(" [label=\"").append(edge.getEvaluationResult()).append("\"];"); //$NON-NLS-1$ //$NON-NLS-2$
        }
        sb.append("\n"); //$NON-NLS-1$
        return true;
    }

    private void appendDotNode(final CFGBasicBlock block, final StringBuilder sb) {
        if (block.isEntryBlock()) {
            sb.append("Entry [style=\"filled\" fillcolor=\"red\"   fontcolor=\"white\"];\n"); //$NON-NLS-1$
        } else if (block.isExitBlock()) {
            sb.append("Exit  [style=\"filled\" fillcolor=\"black\" fontcolor=\"white\"];\n"); //$NON-NLS-1$
        } else {
            block.appendDotNodeId(sb);
            sb.append(" [label=\"").append(escape(block.getDotNodeLabel())).append("\""); //$NON-NLS-1$ //$NON-NLS-2$
            if (block.isDecision()) {
                sb.append(",shape=\"triangle\""); //$NON-NLS-1$
            }
            sb.append("];\n"); //$NON-NLS-1$
            // block.appendDotNodeSourcePosition(sb);
        }
    }

    private String escape(final String s) {
        return s != null ? s.replaceAll("\"", "\\\"") : null; //$NON-NLS-1$ //$NON-NLS-2$
    }
}

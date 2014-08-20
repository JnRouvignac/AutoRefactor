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

import org.eclipse.jdt.core.dom.Expression;

import static org.autorefactor.util.Utils.*;

/**
 * This class represents an edge of the control flow graph.
 */
public class CFGEdge {

    private final Expression condition;
    /** TODO JNR rename. */
    private final boolean evaluationResult;
    private final CFGBasicBlock sourceBlock;
    private final CFGBasicBlock targetBlock;

    /**
     * Builds an instance of this class.
     *
     * @param condition the decision condition that led to the creation of this edge.
     * @param evaluationResult the condition evaluation result
     * @param sourceBlock the source block
     * @param targetBlock the target block
     */
    public CFGEdge(Expression condition, boolean evaluationResult,
            CFGBasicBlock sourceBlock, CFGBasicBlock targetBlock) {
        this.condition = condition;
        this.evaluationResult = evaluationResult;
        this.sourceBlock = sourceBlock;
        this.targetBlock = targetBlock;
    }

    /**
     * Returns the decision condition that led to the creation of this edge.
     *
     * @return the decision condition that led to the creation of this edge
     */
    public Expression getCondition() {
        return this.condition;
    }

    /**
     * Returns the source block of the edge.
     *
     * @return the source block of the edge
     */
    public CFGBasicBlock getSourceBlock() {
        return this.sourceBlock;
    }

    /**
     * Returns the target block of the edge.
     *
     * @return the target block of the edge
     */
    public CFGBasicBlock getTargetBlock() {
        return this.targetBlock;
    }

    /**
     * Returns the condition evaluation result.
     *
     * @return the condition evaluation result
     */
    public boolean getEvaluationResult() {
        return evaluationResult;
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("EDGE[");
        this.sourceBlock.appendDotNodeLabel(sb);
        sb.append(" -> ");
        this.targetBlock.appendDotNodeLabel(sb);
        return sb.append("]").toString();
    }

    /** {@inheritDoc} */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((condition == null) ? 0 : condition.hashCode());
        result = prime * result + (evaluationResult ? 1231 : 1237);
        result = prime * result
                + ((sourceBlock == null) ? 0 : sourceBlock.hashCode());
        result = prime * result
                + ((targetBlock == null) ? 0 : targetBlock.hashCode());
        return result;
    }

    /** {@inheritDoc} */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        final CFGEdge other = (CFGEdge) obj;
        return equal(condition, other.condition)
                && equal(evaluationResult, other.evaluationResult)
                && equal(sourceBlock, other.sourceBlock)
                && equal(targetBlock, other.targetBlock);
    }

}

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

import org.autorefactor.util.IllegalStateException;
import org.eclipse.jdt.core.dom.Expression;

import static org.autorefactor.util.Utils.*;

/**
 * Builder for a {@link CFGEdge}.
 */
public class CFGEdgeBuilder {

    private Expression condition;
    /** TODO JNR rename. */
    private boolean evaluationResult;
    private final CFGBasicBlock sourceBlock;
    private CFGBasicBlock targetBlock;
    /** Marks a "jumping" edge: and edge built because of an exception escaping a try statement. */
    private boolean jumping;
    /** Prevents building twice. */
    private CFGEdge built;

    /**
     * Builds an instance of this class.
     *
     * @param sourceBlock the edge source block
     */
    public CFGEdgeBuilder(CFGBasicBlock sourceBlock) {
        this(null, false, sourceBlock);
    }

    /**
     * Builds an instance of this class.
     *
     * @param condition the decision condition that led to the creation of this edge.
     * @param evaluationResult the condition evaluation result
     * @param sourceBlock the edge source block
     */
    public CFGEdgeBuilder(Expression condition, boolean evaluationResult, CFGBasicBlock sourceBlock) {
        this.condition = condition;
        this.evaluationResult = evaluationResult;
        this.sourceBlock = sourceBlock;
    }

    /**
     * Builds an instance of this class.
     *
     * @param throwingBlock a block throwing an exception
     * @param jumping marks a "jumping" edge: and edge built because of an exception escaping a try statement
     */
    public CFGEdgeBuilder(CFGBasicBlock throwingBlock, boolean jumping) {
        this.sourceBlock = throwingBlock;
        this.jumping = jumping;
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
        final CFGEdgeBuilder other = (CFGEdgeBuilder) obj;
        return equal(condition, other.condition)
                && equal(evaluationResult, other.evaluationResult)
                && equal(sourceBlock, other.sourceBlock)
                && equal(targetBlock, other.targetBlock);
    }

    /**
     * Sets the target block of the edge.
     *
     * @param targetBlock  the target block of the edge
     * @return the current object
     */
    public CFGEdgeBuilder withTarget(CFGBasicBlock targetBlock) {
        this.targetBlock = targetBlock;
        return this;
    }

    /**
     * Validates the content of this builder and then builds a {@link CFGEdge} with it.
     * This method can only be invoked once.
     *
     * @return a new {@link CFGEdge} with the content from this builder.
     */
    public CFGEdge build() {
        if (sourceBlock == null) {
            throw new IllegalStateException(this.condition, "sourceBlock is mandatory");
        }
        if (targetBlock == null) {
            throw new IllegalStateException(this.condition, "targetBlock is mandatory");
        }
        if (built != null) {
            throw new IllegalStateException(this.condition, "CFGEdgeBuilder " + this + " has already been built");
        }
        if (condition != null) {
            built = buildEdge(condition, evaluationResult, sourceBlock, targetBlock);
        } else {
            built = buildEdge(sourceBlock, targetBlock);
        }
        return built;
    }

    /**
     * Factory method building a CFGEdge with the provided arguments.
     *
     * @param sourceBlock the source block of the edge
     * @param targetBlock the target block of the edge
     * @return a new {@link CFGEdge}
     */
    public static CFGEdge buildEdge(CFGBasicBlock sourceBlock, CFGBasicBlock targetBlock) {
        return buildEdge(null, true, sourceBlock, targetBlock);
    }

    /**
     * Factory method building a CFGEdge with the provided arguments.
     *
     * @param condition the decision condition that led to the creation of this edge.
     * @param sourceBlock the source block of the edge
     * @param targetBlock the target block of the edge
     * @return a new {@link CFGEdge}
     */
    public static CFGEdge buildEdge(Expression condition, CFGBasicBlock sourceBlock, CFGBasicBlock targetBlock) {
        return buildEdge(condition, true, sourceBlock, targetBlock);
    }


    /**
     * Factory method building a CFGEdge with the provided arguments.
     *
     * @param condition the decision condition that led to the creation of this edge.
     * @param evaluationResult the condition evaluation result
     * @param sourceBlock the source block of the edge
     * @param targetBlock the target block of the edge
     * @return a new {@link CFGEdge}
     */
    public static CFGEdge buildEdge(Expression condition,
            boolean evaluationResult, CFGBasicBlock sourceBlock, CFGBasicBlock targetBlock) {
        final CFGEdge edge = new CFGEdge(condition, evaluationResult, sourceBlock, targetBlock);
        sourceBlock.addOutgoingEdge(edge);
        targetBlock.addIncomingEdge(edge);
        return edge;
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("EDGE_BUILDER[");
        this.sourceBlock.appendDotNodeLabel(sb);
        sb.append(" -> ");
        if (this.targetBlock != null) {
            this.targetBlock.appendDotNodeLabel(sb);
        } else {
            sb.append("?");
        }
        return sb.append("]").toString();
    }
}

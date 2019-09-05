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

import java.util.Objects;

import org.eclipse.jdt.core.dom.Expression;

/** This class represents an edge of the control flow graph. */
public class CFGEdge {
    private final Expression condition;
    /** TODO JNR rename. */
    private final boolean evaluationResult;
    private final CFGBasicBlock sourceBlock;
    private final CFGBasicBlock targetBlock;

    /**
     * Builds an instance of this class.
     *
     * @param condition        the decision condition that led to the creation of
     *                         this edge.
     * @param evaluationResult the condition evaluation result
     * @param sourceBlock      the source block
     * @param targetBlock      the target block
     */
    public CFGEdge(Expression condition, boolean evaluationResult, CFGBasicBlock sourceBlock,
            CFGBasicBlock targetBlock) {
        this.condition= condition;
        this.evaluationResult= evaluationResult;
        this.sourceBlock= sourceBlock;
        this.targetBlock= targetBlock;
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

    @Override
    public String toString() {
        final StringBuilder sb= new StringBuilder("EDGE["); //$NON-NLS-1$
        this.sourceBlock.appendDotNodeLabel(sb);
        sb.append(" -> "); //$NON-NLS-1$
        this.targetBlock.appendDotNodeLabel(sb);
        return sb.append("]").toString(); //$NON-NLS-1$
    }

    @Override
    public int hashCode() {
        return Objects.hash(condition, evaluationResult, sourceBlock, targetBlock);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        final CFGEdge other= (CFGEdge) obj;
        return Objects.equals(condition, other.condition) && Objects.equals(evaluationResult, other.evaluationResult)
                && Objects.equals(sourceBlock, other.sourceBlock) && Objects.equals(targetBlock, other.targetBlock);
    }
}

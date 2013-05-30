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

import org.eclipse.jdt.core.dom.Expression;

public class CFGEdge {

	private final Expression condition;
	/** TODO JNR rename */
	private final boolean evaluationResult;
	private final CFGBasicBlock sourceBlock;
	private final CFGBasicBlock targetBlock;

	public CFGEdge(Expression condition, boolean evaluationResult,
			CFGBasicBlock source, CFGBasicBlock target) {
		this.condition = condition;
		this.evaluationResult = evaluationResult;
		this.sourceBlock = source;
		this.targetBlock = target;
	}

	public Expression getCondition() {
		return this.condition;
	}

	public CFGBasicBlock getSourceBlock() {
		return this.sourceBlock;
	}

	public CFGBasicBlock getTargetBlock() {
		return this.targetBlock;
	}

	public boolean getEvaluationResult() {
		return evaluationResult;
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder("EDGE[");
		this.sourceBlock.appendDotNodeLabel(sb);
		sb.append(" -> ");
		this.targetBlock.appendDotNodeLabel(sb);
		return sb.append("]").toString();
	}

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

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		CFGEdge other = (CFGEdge) obj;
		if (condition == null) {
			if (other.condition != null)
				return false;
		} else if (!condition.equals(other.condition))
			return false;
		if (evaluationResult != other.evaluationResult)
			return false;
		if (sourceBlock == null) {
			if (other.sourceBlock != null)
				return false;
		} else if (!sourceBlock.equals(other.sourceBlock))
			return false;
		if (targetBlock == null) {
			if (other.targetBlock != null)
				return false;
		} else if (!targetBlock.equals(other.targetBlock))
			return false;
		return true;
	}

}

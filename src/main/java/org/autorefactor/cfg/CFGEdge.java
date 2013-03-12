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
	private final CFGBasicBlock sourceBlock;
	private final CFGBasicBlock targetBlock;

	private CFGEdge(Expression condition, CFGBasicBlock source,
			CFGBasicBlock target) {
		this.condition = condition;
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

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(this.sourceBlock);
		arcToString(sb, this.condition);
		sb.append(this.targetBlock);
		return sb.toString();
	}

	public static void arcToString(final StringBuilder sb,
			Expression condition) {
		sb.append(" +---{");
		sb.append(condition != null ? condition : "true");
		sb.append("}---> ");
	}

	public static CFGEdge build(CFGBasicBlock source, CFGBasicBlock target) {
		return build(null, source, target);
	}

	public static CFGEdge build(Expression condition, CFGBasicBlock source,
			CFGBasicBlock target) {
		final CFGEdge edge = new CFGEdge(condition, source, target);
		source.addOutgoingEdge(edge);
		target.addIncomingEdge(edge);
		return edge;
	}

}

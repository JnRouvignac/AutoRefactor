/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - initial API and implementation
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
package org.autorefactor.jdt.internal.corext.dom;

import java.util.List;

import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.Statement;

/**
 * Represents an expected workflow.
 */
public interface ControlWorkflowMatcherRunnable extends ControlWorkflowMatcherCreable {
	/**
	 * Is matching.
	 *
	 * @param actualStatement The actual statement
	 * @return is matching
	 */
	boolean isMatching(Statement actualStatement);

	/**
	 * Is matching.
	 *
	 * @param actualStatements The actual statements
	 * @return is matching
	 */
	boolean isMatching(List<Statement> actualStatements);

	/**
	 * Add a new workflow. A workflow is started by a list of mandatory conditions that are either passive or unordered, a list of statements to encounter and it's ended by an optional returned value.
	 *
	 * @param expectedCondition The expected condition
	 * @return The matcher
	 */
	ControlWorkflowMatcherCompletable addWorkflow(NodeMatcher<Expression> expectedCondition);
}

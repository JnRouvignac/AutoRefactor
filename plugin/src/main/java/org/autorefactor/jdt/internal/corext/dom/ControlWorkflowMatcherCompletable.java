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

import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.Statement;

/**
 * Represents an expected workflow.
 */
public interface ControlWorkflowMatcherCompletable extends ControlWorkflowMatcherCreable {
	/**
	 * Add a condition to the existing ones on the current workflow. All the conditions form an AND expression.
	 *
	 * @param expectedCondition The new condition.
	 * @return The matcher
	 */
	ControlWorkflowMatcherCompletable condition(NodeMatcher<Expression> expectedCondition);

	/**
	 * Add a statement that should be found following the workflow conditions.
	 *
	 * @param expectedStatement The statement
	 * @return The matcher
	 */
	ControlWorkflowMatcherCompletable statement(NodeMatcher<Statement> expectedStatement);

	/**
	 * Add a returned value that should be found following the workflow conditions.
	 *
	 * @param expectedExpression The returned value
	 * @return The matcher
	 */
	ControlWorkflowMatcherRunnable returnedValue(NodeMatcher<Expression> expectedExpression);
}

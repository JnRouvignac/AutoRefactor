/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice Tiercelin - Initial API and implementation
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
package org.autorefactor.jdt.internal.ui.fix;

import java.util.ArrayList;
import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.InterruptibleVisitor;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.BreakStatement;
import org.eclipse.jdt.core.dom.ContinueStatement;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.LambdaExpression;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SwitchStatement;
import org.eclipse.jdt.core.dom.WhileStatement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class IfRatherThanWhileAndFallsThroughCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.IfRatherThanWhileAndFallsThroughCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.IfRatherThanWhileAndFallsThroughCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.IfRatherThanWhileAndFallsThroughCleanUp_reason;
	}

	@Override
	public boolean visit(final WhileStatement node) {
		if (ASTNodes.fallsThrough(node.getBody())) {
			ContinueVisitor continueVisitor= new ContinueVisitor(node);
			continueVisitor.visitNode(node);

			if (continueVisitor.canBeRefactored()) {
				BreakVisitor breakVisitor= new BreakVisitor(node);
				breakVisitor.visitNode(node);

				if (breakVisitor.canBeRefactored()) {
					replaceByIf(node, breakVisitor);
					return false;
				}
			}
		}

		return true;
	}

	private void replaceByIf(final WhileStatement node, final BreakVisitor breakVisitor) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.IfRatherThanWhileAndFallsThroughCleanUp_description);

		for (BreakStatement breakStatement : breakVisitor.getBreaks()) {
			if (ASTNodes.canHaveSiblings(breakStatement) || breakStatement.getLocationInParent() == IfStatement.ELSE_STATEMENT_PROPERTY) {
				rewrite.remove(breakStatement, group);
			} else {
				rewrite.replace(breakStatement, ast.block(), group);
			}
		}

		rewrite.replace(node, ast.if0(ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(node.getExpression())), ASTNodes.createMoveTarget(rewrite, node.getBody())), group);
	}

	private static class BreakVisitor extends InterruptibleVisitor {
		private final WhileStatement root;
		private final List<BreakStatement> breaks= new ArrayList<>();
		private boolean canBeRefactored= true;

		public BreakVisitor(final WhileStatement root) {
			this.root= root;
		}

		public List<BreakStatement> getBreaks() {
			return breaks;
		}

		public boolean canBeRefactored() {
			return canBeRefactored;
		}

		@Override
		public boolean visit(final BreakStatement aBreak) {
			if (aBreak.getLabel() != null) {
				canBeRefactored= false;
				return interruptVisit();
			}

			Statement parent= aBreak;
			do {
				parent= ASTNodes.getTypedAncestor(parent, Statement.class);
			} while (parent != root && Utils.isEmpty(ASTNodes.getNextSiblings(parent)));

			if (parent != root) {
				canBeRefactored= false;
				return interruptVisit();
			}

			breaks.add(aBreak);

			return true;
		}

		@Override
		public boolean visit(final WhileStatement node) {
			return root.equals(node);
		}

		@Override
		public boolean visit(final DoStatement node) {
			return false;
		}

		@Override
		public boolean visit(final ForStatement node) {
			return false;
		}

		@Override
		public boolean visit(final EnhancedForStatement node) {
			return false;
		}

		@Override
		public boolean visit(final SwitchStatement node) {
			return false;
		}

		@Override
		public boolean visit(final AnonymousClassDeclaration node) {
			return false;
		}

		@Override
		public boolean visit(final LambdaExpression node) {
			return false;
		}
	}

	private static class ContinueVisitor extends InterruptibleVisitor {
		private final WhileStatement root;
		private boolean canBeRefactored= true;

		public ContinueVisitor(final WhileStatement root) {
			this.root= root;
		}

		public boolean canBeRefactored() {
			return canBeRefactored;
		}

		@Override
		public boolean visit(final ContinueStatement node) {
			canBeRefactored= false;
			return interruptVisit();
		}

		@Override
		public boolean visit(final WhileStatement node) {
			return root.equals(node);
		}

		@Override
		public boolean visit(final DoStatement node) {
			return false;
		}

		@Override
		public boolean visit(final ForStatement node) {
			return false;
		}

		@Override
		public boolean visit(final EnhancedForStatement node) {
			return false;
		}

		@Override
		public boolean visit(final AnonymousClassDeclaration node) {
			return false;
		}

		@Override
		public boolean visit(final LambdaExpression node) {
			return false;
		}
	}
}

/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.refactoring.rules;

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.*;

import static org.autorefactor.refactoring.ASTHelper.*;

public class HotSpotIntrinsicedAPIsRefactoring extends ASTVisitor implements
		IJavaRefactoring {

	private class SystemArrayCopyParams {
		private IVariableBinding indexVarBinding;
		private Expression indexStartPos;
		private Expression srcArrayExpr;
		private Expression srcPos;
		private Expression destArrayExpr;
		private Expression destPos;
		private Expression endPos;
	}

	private RefactoringContext ctx;

	public HotSpotIntrinsicedAPIsRefactoring() {
		super();
	}

	public void setRefactoringContext(RefactoringContext ctx) {
		this.ctx = ctx;
	}

	/** {@inheritDoc} */
	@Override
	public boolean visit(ForStatement node) {
		final SystemArrayCopyParams params = new SystemArrayCopyParams();
		collectUniqueIndex(node, params);
		final IVariableBinding incrementedIdx = getUniqueIncrementedVariable(node);
		final List<Statement> stmts = asList(node.getBody());
		if (equalsNotNull(params.indexVarBinding, incrementedIdx)
				&& stmts.size() == 1) {
			collectLength(node.getExpression(), incrementedIdx, params);

			final Statement stmt = stmts.get(0);
			if (stmt instanceof ExpressionStatement) {
				final Expression e = ((ExpressionStatement) stmt).getExpression();
				if (e instanceof Assignment) {
					final Assignment as = (Assignment) e;
					final Expression lhs = as.getLeftHandSide();
					final Expression rhs = as.getRightHandSide();
					if (lhs instanceof ArrayAccess && rhs instanceof ArrayAccess) {
						final ArrayAccess aaLHS = (ArrayAccess) lhs;
						params.destArrayExpr = aaLHS.getArray();
						params.destPos = pp(aaLHS.getIndex(), params);

						final ArrayAccess aaRHS = (ArrayAccess) rhs;
						params.srcArrayExpr = aaRHS.getArray();
						params.srcPos = pp(aaRHS.getIndex(), params);
						return replaceWithSystemArrayCopyCloneAll(node, params);
					}
				}
			}
		}
		return VISIT_SUBTREE;
	}

	private Expression pp(final Expression index,
			final SystemArrayCopyParams params) {
		if (index instanceof SimpleName) {
			final IVariableBinding idxVar = getVariableBinding(index);
			if (equalsNotNull(params.indexVarBinding, idxVar)) {
				return params.indexStartPos;
			}
		}
		else if (index instanceof InfixExpression) {
			final InfixExpression ie = (InfixExpression) index;
			if (InfixExpression.Operator.PLUS.equals(ie.getOperator())) {
				final Expression leftOp = ie.getLeftOperand();
				final Expression rightOp = ie.getRightOperand();
				if (leftOp instanceof SimpleName) {
					final IVariableBinding idxVar = getVariableBinding(leftOp);
					if (equalsNotNull(params.indexVarBinding, idxVar)) {
						return plus(rightOp, params.indexStartPos);
					}
				}
				if (rightOp instanceof SimpleName) {
					final IVariableBinding idxVar = getVariableBinding(rightOp);
					if (equalsNotNull(params.indexVarBinding, idxVar)) {
						return plus(leftOp, params.indexStartPos);
					}
				}
			}
		}
		return null;
	}

	private Expression plus(Expression expr1, Expression expr2) {
		final ASTBuilder b = this.ctx.getASTBuilder();

		final Integer expr1Value = intValue(expr1);
		final Integer expr2Value = intValue(expr2);
		if (expr1Value != null && expr2Value != null) {
			return b.int0(expr1Value + expr2Value);
		}
		else if (expr1Value != null && expr1Value == 0) {
			return b.copyExpr(expr2);
		}
		else if (expr2Value != null && expr2Value == 0) {
			return b.copyExpr(expr1);
		}
		return b.infixExpr(
				b.copyExpr(expr1),
				InfixExpression.Operator.PLUS,
				b.copyExpr(expr2));
	}

	private Integer intValue(Expression expr) {
		if (expr instanceof NumberLiteral && isPrimitive(expr, "int")) {
			return Integer.parseInt(((NumberLiteral) expr).getToken());
		}
		return null;
	}

	private void collectLength(final Expression condition,
			final IVariableBinding incrementedIdx, final SystemArrayCopyParams params) {
		if (condition instanceof InfixExpression) {
			InfixExpression ie = (InfixExpression) condition;
			if (InfixExpression.Operator.LESS.equals(ie.getOperator())) {
				IVariableBinding conditionIdx = getVariableBinding(ie.getLeftOperand());
				if (equalsNotNull(incrementedIdx, conditionIdx)) {
					params.endPos = ie.getRightOperand();
				}
			}
		}
	}

	private boolean equalsNotNull(final Object o1, final Object o2) {
		return o1 != null && o1.equals(o2);
	}

	private boolean replaceWithSystemArrayCopyCloneAll(ForStatement node,
			SystemArrayCopyParams params) {
		if (params.srcArrayExpr == null
				|| params.srcPos == null
				|| params.destArrayExpr == null
				|| params.destPos == null
				|| params.endPos == null) {
			return DO_NOT_VISIT_SUBTREE;
		}
		final ASTBuilder b = this.ctx.getASTBuilder();
		return replaceWithSystemArrayCopy(node,
				b.copyExpr(params.srcArrayExpr),
				b.copyExpr(params.srcPos),
				b.copyExpr(params.destArrayExpr),
				b.copyExpr(params.destPos),
				b.copyExpr(params.endPos));
	}

	private boolean replaceWithSystemArrayCopy(ForStatement node,
			Expression srcArrayExpr, Expression srcPos,
			Expression destArrayExpr, Expression destPos,
			Expression length) {
		final ASTBuilder b = this.ctx.getASTBuilder();
		final TryStatement tryS = b.try0(
				b.body(
						b.toStmt(
								b.invoke("System", "arraycopy",
										srcArrayExpr, srcPos, destArrayExpr, destPos, length))),
				b.catch0("IndexOutOfBoundsException", "e",
						b.throw0(
								b.new0("ArrayIndexOutOfBoundsException",
										b.invoke("e", "getMessage")))));

		this.ctx.getRefactorings().replace(node, tryS);
		return DO_NOT_VISIT_SUBTREE;
	}

	private void collectUniqueIndex(ForStatement node,
			SystemArrayCopyParams params) {
		if (node.initializers().size() != 1) {
			return;
		}
		final Expression initializer = (Expression) node.initializers().get(0);
		if (initializer instanceof VariableDeclarationExpression) {
			final VariableDeclarationExpression vde =
					(VariableDeclarationExpression) initializer;
			if (isPrimitive(vde, "int") && vde.fragments().size() == 1) {
				// this must be the array index
				VariableDeclarationFragment vdf = (VariableDeclarationFragment) vde.fragments().get(0);
				if (vdf.getExtraDimensions() == 0) {
					params.indexStartPos = vdf.getInitializer();
					params.indexVarBinding = vdf.resolveBinding();
					return;
				}
			}
		}
		else if (initializer instanceof Assignment) {
			final Assignment as = (Assignment) initializer;
			if (Assignment.Operator.ASSIGN.equals(as.getOperator())
					&& isPrimitive(as.resolveTypeBinding(), "int")) {
				// this must be the array index
				params.indexStartPos = as.getRightHandSide();
				final Expression lhs = as.getLeftHandSide();
				if (lhs instanceof SimpleName) {
					final IBinding binding = ((SimpleName) lhs).resolveBinding();
					if (binding instanceof IVariableBinding) {
						params.indexVarBinding = (IVariableBinding) binding;
						return;
					}
				}
			}
		}
	}

	private IVariableBinding getUniqueIncrementedVariable(ForStatement node) {
		if (node.updaters().size() != 1) {
			return null;
		}
		final Expression updater = (Expression) node.updaters().get(0);
		if (updater instanceof PostfixExpression) {
			final PostfixExpression pe = (PostfixExpression) updater;
			if (PostfixExpression.Operator.INCREMENT.equals(pe.getOperator())) {
				return getVariableBinding(pe.getOperand());
			}
		}
		else if (updater instanceof PrefixExpression) {
			final PrefixExpression pe = (PrefixExpression) updater;
			if (PrefixExpression.Operator.INCREMENT.equals(pe.getOperator())) {
				return getVariableBinding(pe.getOperand());
			}
		}
		return null;
	}

	// TODO JNR verify that client code null checks the result of calling this method
	private IVariableBinding getVariableBinding(final Expression e) {
		if (e instanceof SimpleName) {
			final SimpleName sn = (SimpleName) e;
			final IBinding binding = sn.resolveBinding();
			if (binding instanceof IVariableBinding) { // this is a local variable or a field
				return (IVariableBinding) binding;
			}
		}
		return null;
	}

	public Refactorings getRefactorings(CompilationUnit astRoot) {
		astRoot.accept(this);
		return this.ctx.getRefactorings();
	}
}

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
package org.autorefactor.refactoring.rules;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.autorefactor.refactoring.ASTHelper;
import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.Release;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.PackageDeclaration;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.jdt.core.dom.WhileStatement;

/**
 * Reduces the scope of local variables.
 * <p>
 * TODO JNR can we also transform singular fields into local variables?
 * </p>
 */
public class ReduceVariableScopeRefactoring extends ASTVisitor implements
		IJavaRefactoring {

	private static final int DECL = 1 << 0, READ = 1 << 1, WRITE = 1 << 2;

	private static final class VariableName {

		private final Name name;

		public VariableName(Name name) {
			this.name = name;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj instanceof VariableName) {
				final VariableName other = (VariableName) obj;
				if (this.name instanceof SimpleName
						&& other.name instanceof SimpleName) {
					return ASTHelper.isEqual((SimpleName) this.name,
							(SimpleName) other.name);
				}
				// if (this.name instanceof QualifiedName
				// && other.name instanceof QualifiedName) {
				// throw new IllegalStateException();
				// }
			}
			// return false;
			throw new IllegalStateException();
		}

		@Override
		public int hashCode() {
			if (this.name instanceof SimpleName) {
				return ((SimpleName) this.name).getIdentifier().hashCode();
			}
			// if (this.name instanceof QualifiedName) {
			// throw new IllegalStateException();
			// }
			throw new IllegalStateException();
		}

		@Override
		public String toString() {
			return this.name.toString();
		}
	}

	private static final class VariableAccess {

		private final Name variableName;
		private final int accessType;
		private final ASTNode scope;

		public VariableAccess(Name variableName, int accessType, ASTNode scope) {
			if (accessType == 0) {
				throw new IllegalArgumentException(
						"accessType must not be null");
			}
			this.variableName = variableName;
			this.accessType = accessType;
			this.scope = scope;
		}

		public Name getVariableName() {
			return variableName;
		}

		public int getAccessType() {
			return accessType;
		}

		public ASTNode getScope() {
			return scope;
		}

		@Override
		public String toString() {
			final StringBuilder sb = new StringBuilder();
			if ((this.accessType & DECL) != 0) {
				sb.append("DECL");
			}
			if ((this.accessType & READ) != 0) {
				if (sb.length() > 0) {
					sb.append(" | ");
				}
				sb.append("READ");
			}
			if ((this.accessType & WRITE) != 0) {
				if (sb.length() > 0) {
					sb.append(" | ");
				}
				sb.append("WRITE");
			}
			sb.append("\n");
			sb.append(scope);
			return sb.toString();
		}
	}

	private final Refactorings refactorings = new Refactorings();
	private AST ast;
	private Release javaSERelease;
	private final Map<VariableName, List<VariableAccess>> allVariableAccesses = new HashMap<VariableName, List<VariableAccess>>();
	private static final Pair<Integer, ASTNode> nullPair = Pair.of(0, null);

	public ReduceVariableScopeRefactoring() {
		super();
	}

	public void setAST(final AST ast) {
		this.ast = ast;
	}

	public void setJavaSERelease(Release javaSERelease) {
		this.javaSERelease = javaSERelease;
	}

	@Override
	public boolean visit(SimpleName node) {
		findVariableAccesses(node);
		return true;
	}

	@Override
	public boolean visit(QualifiedName node) {
		findVariableAccesses(node);
		return false;
	}

	public void findVariableAccesses(Name node) {
		final Pair<Integer, ASTNode> accessTypeAndScope = getAccessTypeAndScope(node);
		if (accessTypeAndScope.getFirst().intValue() != 0) {
			final VariableName varName = new VariableName(node);
			List<VariableAccess> list = this.allVariableAccesses.get(varName);
			if (list == null) {
				list = new ArrayList<VariableAccess>();
				this.allVariableAccesses.put(varName, list);
			}
			if (list.size() == 0
					|| !list.get(list.size() - 1).getScope()
							.equals(accessTypeAndScope.getSecond())) {
				// only keep first write in scope
				list.add(new VariableAccess(node,
						accessTypeAndScope.getFirst(), accessTypeAndScope
								.getSecond()));
			}
		}
	}

	private Pair<Integer, ASTNode> getAccessTypeAndScope(ASTNode node) {
		final ASTNode parent = node.getParent();
		if (parent instanceof Block || parent instanceof InfixExpression
				|| parent instanceof EnhancedForStatement
				|| parent instanceof ExpressionStatement
				|| parent instanceof ForStatement || parent instanceof Name
				|| parent instanceof WhileStatement) {
			return getAccessTypeAndScope(parent);
		} else if (parent instanceof ImportDeclaration
				|| parent instanceof MethodDeclaration
				|| parent instanceof MethodInvocation
				|| parent instanceof PackageDeclaration
				|| parent instanceof Type || parent instanceof TypeDeclaration) {
			return nullPair;
		} else if (parent instanceof SingleVariableDeclaration) {
			final SingleVariableDeclaration var = (SingleVariableDeclaration) parent;
			return getAccessTypeAndScope(var.getParent());
		} else if (parent instanceof VariableDeclarationFragment) {
			final VariableDeclarationFragment var = (VariableDeclarationFragment) parent;
			return Pair.of(var.getInitializer() != null ? WRITE | DECL : DECL,
					getScope(var));
		} else if (parent instanceof Assignment) {
			return Pair.of(WRITE, getScope(parent.getParent()));
		} else if (parent instanceof InfixExpression) {
			return Pair.of(READ, getScope(parent.getParent()));
		} else if (parent instanceof PostfixExpression) {
			return Pair.of(READ | WRITE, getScope(parent.getParent()));
		}
		throw p(parent);
	}

	private ASTNode getScope(ASTNode node) {
		if (node instanceof Block || node instanceof EnhancedForStatement
				|| node instanceof ForStatement || node instanceof IfStatement
				|| node instanceof WhileStatement) {
			return node;
		} else if (node instanceof Expression || node instanceof Statement
				|| node instanceof VariableDeclaration) {
			return getScope(node.getParent());
		}
		throw p(node);
	}

	private IllegalStateException p(final ASTNode node) {
		return new IllegalStateException("Not implemented for "
				+ node.getClass().getName());
	}

	public Refactorings getRefactorings(CompilationUnit astRoot) {
		astRoot.accept(this);

		for (Entry<VariableName, List<VariableAccess>> entry : this.allVariableAccesses
				.entrySet()) {
			final List<VariableAccess> variableAccesses = entry.getValue();
			if (canReduceVariableScope(variableAccesses)) {
				final VariableAccess varDecl = variableAccesses.get(0);
				remove(varDecl.getVariableName());

				for (VariableAccess varAccess : variableAccesses) {
					if (varAccess.getAccessType() == WRITE) {
						replace(varDecl, varAccess);
					}// TODO JNR if (varAccess.getAccessType() & WRITE) {
				}
			}
		}

		// TODO JNR remove writes when there are no reads after
		// TODO JNR remove double writes when there are no reads after

		return this.refactorings;
	}

	private void replace(VariableAccess varDecl, VariableAccess varAccess) {
		final ASTNode scope = varAccess.getScope();
		final Name varName = varAccess.getVariableName();
		final Type varType = getType(varDecl.getVariableName().getParent());
		if (scope instanceof Block) {
			final Block b = (Block) scope;
			final List<Statement> statements = b.statements();
			for (int i = 0; i < statements.size(); i++) {
				final Statement stmt = statements.get(i);
				final Expression parentExpr = getParentOfType(varName,
						Expression.class);// FIXME i=0
				final Statement parentStmt = getParentOfType(parentExpr,
						Statement.class);// FIXME i=0;
				if (stmt.equals(parentStmt)) {
					final VariableDeclarationFragment vdf = getVariableDeclarationFragment(
							parentExpr, varName);

					final VariableDeclarationStatement vds = this.ast
							.newVariableDeclarationStatement(vdf);
					vds.setType(varType);
					this.refactorings.replace(stmt, vds);
					break;
				}
			}
		} else if (scope instanceof EnhancedForStatement) {
			final EnhancedForStatement efs = (EnhancedForStatement) scope;
			final EnhancedForStatement newEfs = ASTHelper.copySubtree(this.ast,
					efs);
			newEfs.setParameter(ASTHelper.copySubtree(this.ast,
					efs.getParameter()));
			newEfs.setExpression(ASTHelper.copySubtree(this.ast,
					efs.getExpression()));
			final Statement parentStmt = getParentOfType(varName,
					Statement.class);
			if (efs.getBody() != null && efs.getBody().equals(parentStmt)) {
				newEfs.setBody(copy(efs.getBody(), varName));
			}
			this.refactorings.replace(efs, newEfs);
		} else if (scope instanceof ForStatement) {
			final ForStatement fs = (ForStatement) scope;
			final ForStatement newFs = ASTHelper.copySubtree(this.ast, fs);
			final List<Expression> initializers = newFs.initializers();
			if (initializers.size() == 1) {
				final Expression init = initializers.remove(0);
				final VariableDeclarationFragment vdf = getVariableDeclarationFragment(
						init, varName);
				final VariableDeclarationExpression vde = this.ast
						.newVariableDeclarationExpression(vdf);
				vde.setType(varType);
				initializers.add(vde);
				this.refactorings.replace(fs, newFs);
				// TODO JNR
				// if (fs.getBody() != null && fs.getBody().equals(parentStmt))
				// {
				// newFs.setBody(copy(fs.getBody()));
				// }
			} else {
				throw new IllegalStateException(
						"Unhandled case: more than one initializer in for loop.");
			}
		} else if (scope instanceof WhileStatement) {
			final WhileStatement ws = (WhileStatement) scope;
			final WhileStatement newWs = this.ast.newWhileStatement();
			newWs.setExpression(ASTHelper.copySubtree(this.ast,
					ws.getExpression()));
			final Statement parentStmt = getParentOfType(varName,
					Statement.class);
			if (ws.getBody() != null && ws.getBody().equals(parentStmt)) {
				newWs.setBody(copy(ws.getBody(), varName));
			}
			this.refactorings.replace(ws, newWs);
		} else if (scope instanceof IfStatement) {
			final IfStatement is = (IfStatement) scope;
			final IfStatement newIs = this.ast.newIfStatement();
			newIs.setExpression(ASTHelper.copySubtree(this.ast,
					is.getExpression()));
			final Statement parentStmt = getParentOfType(varName,
					Statement.class);
			if (is.getThenStatement() != null
					&& is.getThenStatement().equals(parentStmt)) {
				newIs.setThenStatement(copy(is.getThenStatement(), varName));
				if (is.getElseStatement() != null) {
					newIs.setElseStatement(ASTHelper.copySubtree(this.ast,
							is.getElseStatement()));
				}
				this.refactorings.replace(is, newIs);
			} else if (is.getElseStatement() != null
					&& is.getElseStatement().equals(parentStmt)) {
				if (is.getThenStatement() != null) {
					newIs.setThenStatement(ASTHelper.copySubtree(this.ast,
							is.getThenStatement()));
				}
				newIs.setElseStatement(copy(is.getElseStatement(), varName));
				this.refactorings.replace(is, newIs);
			} else {
				throw new IllegalStateException(
						"Parent statement should be inside the then or else statement of this if statement: "
								+ is);
			}
		} else {
			throw p(scope);
		}
	}

	private Block copy(Statement stmtToCopy, Name varName) {
		if (stmtToCopy != null && !(stmtToCopy instanceof Block)) {
			final Block b = this.ast.newBlock();
			if (stmtToCopy instanceof ExpressionStatement) {
				final ExpressionStatement es = (ExpressionStatement) stmtToCopy;
				final VariableDeclarationFragment vdf = getVariableDeclarationFragment(
						es.getExpression(), varName);
				b.statements().add(
						this.ast.newVariableDeclarationStatement(vdf));
			} else {
				throw p(stmtToCopy);
			}
			return b;
		}
		// We should never come here if we had a Block statement, see the
		// replace() method
		throw new IllegalStateException("Not implemented for " + stmtToCopy);
	}

	@SuppressWarnings("unchecked")
	private <T extends ASTNode> T getParentOfType(ASTNode node, Class<T> clazz) {
		final ASTNode parent = node.getParent();
		if (node != null && clazz.isAssignableFrom(parent.getClass())) {
			return (T) parent;
		}
		return getParentOfType(parent, clazz);
	}

	private Type getType(ASTNode node) {
		if (node instanceof VariableDeclarationStatement) {
			final VariableDeclarationStatement vds = (VariableDeclarationStatement) node;
			return ASTHelper.copySubtree(this.ast, vds.getType());
		}
		return getType(node.getParent());
	}

	private VariableDeclarationFragment getVariableDeclarationFragment(
			final Expression exprToReplace, final Name varName) {
		if (exprToReplace instanceof Assignment) {
			final Assignment a = (Assignment) exprToReplace;
			if (a.getLeftHandSide() instanceof SimpleName) {
				final SimpleName sn = (SimpleName) a.getLeftHandSide();
				if (sn.getFullyQualifiedName().equals(
						varName.getFullyQualifiedName())) {
					final VariableDeclarationFragment vdf = this.ast
							.newVariableDeclarationFragment();
					vdf.setInitializer(ASTHelper.copySubtree(this.ast,
							a.getRightHandSide()));
					vdf.setName(ASTHelper.copySubtree(this.ast, sn));
					return vdf;
				}
			}
			throw p(a.getLeftHandSide());
		}
		throw p(exprToReplace);
	}

	private void remove(ASTNode node) {
		if (node instanceof VariableDeclarationFragment) {
			this.refactorings.remove(node.getParent());
		} else {
			remove(node.getParent());
		}
	}

	private boolean canReduceVariableScope(
			final List<VariableAccess> variableAccesses) {
		final VariableAccess varDecl = variableAccesses.get(0);

		VariableAccess lastWrite = null;
		for (VariableAccess varAccess : variableAccesses) {
			if (varAccess.getAccessType() == WRITE) {
				// is only write
				lastWrite = varAccess;
			} else if ((varAccess.getAccessType() & READ) != 0) {
				// is read
				if (lastWrite != null
						&& !isReadDominatedByWriteInScopeMoreReducedThanVariableScope(
								varAccess.getScope(), lastWrite.getScope(),
								varDecl.getScope())) {
					// TODO JNR return sublist of reduceable scope
					return false;
				}
			}
		}
		return true;
	}

	private boolean isReadDominatedByWriteInScopeMoreReducedThanVariableScope(
			ASTNode readScope, ASTNode writeScope, ASTNode varScope) {
		if (varScope.equals(readScope) || varScope.equals(writeScope)) {
			return false;
		}
		if (readScope.equals(writeScope)) {
			return true;
		}
		return isReadDominatedByWriteInScopeMoreReducedThanVariableScope(
				readScope.getParent(), writeScope, varScope);
	}
}

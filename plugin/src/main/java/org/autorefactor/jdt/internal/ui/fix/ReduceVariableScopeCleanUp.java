/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2016 Jean-Noël Rouvignac - initial API and implementation
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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.util.IllegalArgumentException;
import org.autorefactor.util.IllegalStateException;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.Pair;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.ASTNode;
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
 * TODO JNR can we also transform singular fields into local variables?
 *
 * @see {@link #getDescription()}
 */
public class ReduceVariableScopeCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return "Reduce scope of variable"; //$NON-NLS-1$
	}

	@Override
	public String getDescription() {
		return "Reduces the scope of local variables."; //$NON-NLS-1$
	}

	@Override
	public String getReason() {
		return "It reduces the reading and debugging cost."; //$NON-NLS-1$
	}

	private static final int DECL= 1 << 0;
	private static final int READ= 1 << 1;
	private static final int WRITE= 1 << 2;

	private static final class VariableName {
		private final Name name;

		public VariableName(final Name name) {
			this.name= name;
		}

		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj instanceof VariableName) {
				VariableName other= (VariableName) obj;
				if (this.name instanceof SimpleName && other.name instanceof SimpleName) {
					return ASTNodes.isEqual((SimpleName) this.name, (SimpleName) other.name);
				}
				// if (this.name instanceof QualifiedName
				// && other.name instanceof QualifiedName) {
				// throw new IllegalStateException();
				// }
			}
			// Return false;
			throw new NotImplementedException(name, name);
		}

		@Override
		public int hashCode() {
			if (this.name instanceof SimpleName) {
				return ((SimpleName) this.name).getIdentifier().hashCode();
			}
			// if (this.name instanceof QualifiedName) {
			// throw new IllegalStateException();
			// }
			throw new NotImplementedException(name, name);
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

		public VariableAccess(final Name variableName, final int accessType, final ASTNode scope) {
			if (accessType == 0) {
				throw new IllegalArgumentException(null, "accessType must not be null"); //$NON-NLS-1$
			}
			this.variableName= variableName;
			this.accessType= accessType;
			this.scope= scope;
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
			StringBuilder sb= new StringBuilder();
			if ((this.accessType & DECL) != 0) {
				sb.append("DECL"); //$NON-NLS-1$
			}
			if ((this.accessType & READ) != 0) {
				if (sb.length() > 0) {
					sb.append(" | "); //$NON-NLS-1$
				}
				sb.append("READ"); //$NON-NLS-1$
			}
			if ((this.accessType & WRITE) != 0) {
				if (sb.length() > 0) {
					sb.append(" | "); //$NON-NLS-1$
				}
				sb.append("WRITE"); //$NON-NLS-1$
			}
			sb.append("\n"); //$NON-NLS-1$
			sb.append(scope);
			return sb.toString();
		}
	}

	private final Map<VariableName, List<VariableAccess>> allVariableAccesses= new HashMap<>();
	private static final Pair<Integer, ASTNode> NULL_PAIR= Pair.of(0, null);

	@Override
	public boolean visit(final SimpleName node) {
		findVariableAccesses(node);
		return true;
	}

	@Override
	public boolean visit(final QualifiedName node) {
		findVariableAccesses(node);
		return true;
	}

	private void findVariableAccesses(final Name node) {
		Pair<Integer, ASTNode> accessTypeAndScope= getAccessTypeAndScope(node);
		if (accessTypeAndScope.getFirst().intValue() != 0) {
			VariableName varName= new VariableName(node);
			List<VariableAccess> list= this.allVariableAccesses.get(varName);
			if (list == null) {
				list= new ArrayList<>();
				this.allVariableAccesses.put(varName, list);
			}
			if (list.isEmpty() || !Utils.getLast(list).getScope().equals(accessTypeAndScope.getSecond())) {
				// Only keep first write in scope
				list.add(new VariableAccess(node, accessTypeAndScope.getFirst(), accessTypeAndScope.getSecond()));
			}
		}
	}

	private Pair<Integer, ASTNode> getAccessTypeAndScope(final ASTNode node) {
		ASTNode parent= node.getParent();
		if (parent instanceof Block || parent instanceof InfixExpression || parent instanceof EnhancedForStatement
				|| parent instanceof ExpressionStatement || parent instanceof ForStatement || parent instanceof Name
				|| parent instanceof WhileStatement) {
			return getAccessTypeAndScope(parent);
		}
		if (parent instanceof ImportDeclaration || parent instanceof MethodDeclaration
				|| parent instanceof MethodInvocation || parent instanceof PackageDeclaration || parent instanceof Type
				|| parent instanceof TypeDeclaration) {
			return NULL_PAIR;
		}
		if (parent instanceof SingleVariableDeclaration) {
			SingleVariableDeclaration var= (SingleVariableDeclaration) parent;
			return getAccessTypeAndScope(var.getParent());
		}
		if (parent instanceof VariableDeclarationFragment) {
			VariableDeclarationFragment var= (VariableDeclarationFragment) parent;
			return Pair.of(var.getInitializer() != null ? WRITE | DECL : DECL, getScope(var));
		}
		if (parent instanceof Assignment) {
			return Pair.of(WRITE, getScope(parent.getParent()));
		}
		if (parent instanceof InfixExpression) {
			return Pair.of(READ, getScope(parent.getParent()));
		}
		if (parent instanceof PostfixExpression) {
			return Pair.of(READ | WRITE, getScope(parent.getParent()));
		}
		throw new NotImplementedException(parent);
	}

	private ASTNode getScope(final ASTNode node) {
		if (node instanceof Block || node instanceof EnhancedForStatement || node instanceof ForStatement
				|| node instanceof IfStatement || node instanceof WhileStatement) {
			return node;
		}
		if (node instanceof Expression || node instanceof Statement || node instanceof VariableDeclaration) {
			return getScope(node.getParent());
		}
		throw new NotImplementedException(node);
	}

	@Override
	public ASTRewrite getRefactorings(final CompilationUnit astRoot) {
		astRoot.accept(this);

		for (Entry<VariableName, List<VariableAccess>> entry : this.allVariableAccesses.entrySet()) {
			List<VariableAccess> variableAccesses= entry.getValue();
			if (canReduceVariableScope(variableAccesses)) {
				VariableAccess varDecl= variableAccesses.get(0);
				remove(varDecl.getVariableName());

				for (VariableAccess varAccess : variableAccesses) {
					if (varAccess.getAccessType() == WRITE) {
						replace(varDecl, varAccess);
					} // TODO JNR if (varAccess.getAccessType() & WRITE) {
				}
			}
		}

		// TODO JNR remove writes when there are no reads after
		// TODO JNR remove double writes when there are no reads after

		return cuRewrite.getASTRewrite();
	}

	private void replace(final VariableAccess varDecl, final VariableAccess varAccess) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		ASTNode scope= varAccess.getScope();
		Name varName= varAccess.getVariableName();
		Type varType= getType(varDecl.getVariableName().getParent());
		if (scope instanceof Block) {
			@SuppressWarnings("unchecked")
			List<Statement> statements= ((Block) scope).statements();
			for (Statement statement : statements) {
				Expression parentExpression= ASTNodes.getTypedAncestorOrCrash(varName, Expression.class); // FIXME i=0
				Statement parentStatement= ASTNodes.getTypedAncestorOrCrash(parentExpression, Statement.class); // FIXME i=0
				if (statement.equals(parentStatement)) {
					VariableDeclarationFragment fragment= getVariableDeclarationFragment(parentExpression, varName);
					VariableDeclarationStatement vds= ast.getAST().newVariableDeclarationStatement(fragment);
					vds.setType(varType);
					rewrite.replace(statement, vds, null);
					break;
				}
			}
		} else if (scope instanceof EnhancedForStatement) {
			EnhancedForStatement efs= (EnhancedForStatement) scope;
			EnhancedForStatement newEfs= ast.createCopyTarget(efs);
			newEfs.setParameter(ast.createCopyTarget(efs.getParameter()));
			newEfs.setExpression(ast.createCopyTarget(efs.getExpression()));
			Statement parentStatement= ASTNodes.getTypedAncestorOrCrash(varName, Statement.class);
			if (Utils.equalNotNull(efs.getBody(), parentStatement)) {
				newEfs.setBody(copy(efs.getBody(), varName));
			}
			rewrite.replace(efs, newEfs, null);
		} else if (scope instanceof ForStatement) {
			ForStatement fs= (ForStatement) scope;
			ForStatement newFs= ast.createCopyTarget(fs);
			@SuppressWarnings("unchecked")
			List<Expression> initializers= newFs.initializers();
			if (initializers.size() != 1) {
				throw new NotImplementedException(scope, "for more than one initializer in for loop."); //$NON-NLS-1$
			}
			Expression init= initializers.remove(0);
			VariableDeclarationFragment fragment= getVariableDeclarationFragment(init, varName);
			VariableDeclarationExpression variableDeclarationExpression= ast.getAST().newVariableDeclarationExpression(fragment);
			variableDeclarationExpression.setType(varType);
			initializers.add(variableDeclarationExpression);
			rewrite.replace(fs, newFs, null);
			// TODO JNR
			// if (equalNotNull(fs.getBody(), parentStatement)) {
			// newFs.setBody(copy(fs.getBody()));
			// }
		} else if (scope instanceof WhileStatement) {
			WhileStatement ws= (WhileStatement) scope;
			WhileStatement newWs= ast.getAST().newWhileStatement();
			newWs.setExpression(ast.createCopyTarget(ws.getExpression()));
			Statement parentStatement= ASTNodes.getTypedAncestorOrCrash(varName, Statement.class);
			if (Utils.equalNotNull(ws.getBody(), parentStatement)) {
				newWs.setBody(copy(ws.getBody(), varName));
			}
			rewrite.replace(ws, newWs, null);
		} else if (scope instanceof IfStatement) {
			IfStatement is= (IfStatement) scope;
			IfStatement newIs= ast.getAST().newIfStatement();
			newIs.setExpression(ast.createCopyTarget(is.getExpression()));
			Statement parentStatement= ASTNodes.getTypedAncestorOrCrash(varName, Statement.class);
			if (Utils.equalNotNull(is.getThenStatement(), parentStatement)) {
				newIs.setThenStatement(copy(is.getThenStatement(), varName));
				if (is.getElseStatement() != null) {
					newIs.setElseStatement(ast.createCopyTarget(is.getElseStatement()));
				}
			} else if (Utils.equalNotNull(is.getElseStatement(), parentStatement)) {
				if (is.getThenStatement() != null) {
					newIs.setThenStatement(ast.createCopyTarget(is.getThenStatement()));
				}
				newIs.setElseStatement(copy(is.getElseStatement(), varName));
			} else {
				throw new IllegalStateException(is,
						"Parent statement should be inside the then or else statement of this if statement: " + is); //$NON-NLS-1$
			}
			rewrite.replace(is, newIs, null);
		} else {
			throw new NotImplementedException(scope);
		}
	}

	@SuppressWarnings("unchecked")
	private Block copy(final Statement stmtToCopy, final Name varName) {
		if (stmtToCopy != null && !(stmtToCopy instanceof Block)) {
			Block block= cuRewrite.getAST().newBlock();
			Assignment a= ASTNodes.asExpression(stmtToCopy, Assignment.class);
			if (a == null) {
				throw new NotImplementedException(stmtToCopy);
			}
			VariableDeclarationFragment fragment= getVariableDeclarationFragment(a, varName);
			((List<Statement>) block.statements()).add(cuRewrite.getAST().newVariableDeclarationStatement(fragment));
			return block;
		}
		// We should never come here if we had a Block statement, see the replace()
		// method
		throw new NotImplementedException(stmtToCopy);
	}

	private Type getType(final ASTNode node) {
		if (node instanceof VariableDeclarationStatement) {
			VariableDeclarationStatement vds= (VariableDeclarationStatement) node;
			return cuRewrite.getASTBuilder().createCopyTarget(vds.getType());
		}

		return getType(node.getParent());
	}

	private VariableDeclarationFragment getVariableDeclarationFragment(final Expression exprToReplace, final Name varName) {
		if (exprToReplace instanceof Assignment) {
			Assignment a= (Assignment) exprToReplace;
			if (a.getLeftHandSide() instanceof SimpleName) {
				SimpleName sn= (SimpleName) a.getLeftHandSide();
				if (sn.getFullyQualifiedName().equals(varName.getFullyQualifiedName())) {
					ASTNodeFactory ast= cuRewrite.getASTBuilder();

					VariableDeclarationFragment fragment= ast.getAST().newVariableDeclarationFragment();
					fragment.setInitializer(ast.createCopyTarget(a.getRightHandSide()));
					fragment.setName(ast.createCopyTarget(sn));
					return fragment;
				}
			}
			throw new NotImplementedException(a.getLeftHandSide());
		}
		throw new NotImplementedException(exprToReplace);
	}

	private void remove(final ASTNode node) {
		if (node instanceof VariableDeclarationFragment) {
			cuRewrite.getASTRewrite().remove(node.getParent(), null);
		} else {
			remove(node.getParent());
		}
	}

	private boolean canReduceVariableScope(final List<VariableAccess> variableAccesses) {
		VariableAccess varDecl= variableAccesses.get(0);

		VariableAccess lastWrite= null;
		for (VariableAccess varAccess : variableAccesses) {
			if (varAccess.getAccessType() == WRITE) {
				// Is only write
				lastWrite= varAccess;
			} else if ((varAccess.getAccessType() & READ) != 0 && lastWrite != null
					&& !isReadDominatedByWriteInScopeMoreReducedThanVariableScope(varAccess.getScope(),
							lastWrite.getScope(), varDecl.getScope())) {
				// TODO JNR return sublist of reduceable scope
				return false;
			}
		}

		return true;
	}

	private boolean isReadDominatedByWriteInScopeMoreReducedThanVariableScope(final ASTNode readScope, final ASTNode writeScope,
			final ASTNode varScope) {
		return !varScope.equals(readScope) && !varScope.equals(writeScope)
				&& (readScope.equals(writeScope) || isReadDominatedByWriteInScopeMoreReducedThanVariableScope(
						readScope.getParent(), writeScope, varScope));
	}
}

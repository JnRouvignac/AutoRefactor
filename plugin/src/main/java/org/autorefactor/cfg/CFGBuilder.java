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

import java.lang.reflect.Method;
import java.util.*;
import java.util.Map.Entry;
import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.autorefactor.refactoring.ASTHelper;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.UnhandledException;
import org.eclipse.jdt.core.dom.*;

import static org.autorefactor.cfg.ASTPrintHelper.*;
import static org.autorefactor.cfg.CFGEdgeBuilder.*;
import static org.autorefactor.cfg.VariableAccess.*;
import static org.autorefactor.refactoring.ASTHelper.*;

/**
 * Builds a CFG.
 * <p>
 * Look at {@link #buildCFG(IfStatement, LivenessState)} for a javadoc for
 * all the buildCFG(*Statement, LivenessState) methods.
 * <p>
 * TODO JNR detect dead code by looking for empty live blocks list when visiting a node
 * + looking at if / while / etc. conditions and see if they resolve to a constant
 */
public class CFGBuilder {

	private static final class LivenessState {

		/**
		 * The currently live CFGBasicBlock. It can be null which means that further
		 * analysis will first have to create a live CFGBasicBlock.
		 */
		private final CFGBasicBlock liveBasicBlock;
		/**
		 * The edges that are live on entering or after finishing analyzing a
		 * statement.
		 */
		private final List<CFGEdgeBuilder> liveEdges = new LinkedList<CFGEdgeBuilder>();

		private LivenessState() {
			this.liveBasicBlock = null;
		}

		private LivenessState(CFGBasicBlock liveBasicBlock) {
			this.liveBasicBlock = liveBasicBlock;
		}

		private LivenessState(CFGBasicBlock liveBasicBlock, CFGEdgeBuilder liveEdge) {
			this.liveBasicBlock = liveBasicBlock;
			this.liveEdges.add(liveEdge);
		}

		private static LivenessState of(List<CFGEdgeBuilder> liveEdges) {
			LivenessState result = new LivenessState();
			result.addAll(liveEdges);
			return result;
		}

		private static LivenessState of(CFGEdgeBuilder liveEdge) {
			LivenessState result = new LivenessState();
			result.add(liveEdge);
			return result;
		}

		private LivenessState copyLiveBasicBlock() {
			return new LivenessState(liveBasicBlock);
		}

		private LivenessState copyLiveEdges() {
			return LivenessState.of(liveEdges);
		}

		private LivenessState nextStmtWillCreateNewBlock() {
			return copyLiveEdges();
		}

		private LivenessState nextStmtsAreDeadCode() {
			return copyLiveEdges();
		}

		private boolean requireNewBlock() {
			return liveBasicBlock == null;
		}

		private void add(CFGEdgeBuilder edge) {
			liveEdges.add(edge);
		}

		private void addAll(Collection<CFGEdgeBuilder> edges) {
			liveEdges.addAll(edges);
		}

		private void addAll(LivenessState state) {
			liveEdges.addAll(state.liveEdges);
		}

		/** {@inheritDoc} */
		@Override
		public String toString() {
			return "LivenessState [liveBasicBlock=" + liveBasicBlock
					+ ", liveEdges=" + liveEdges + "]";
		}
	}

	private static final Pattern NEWLINE = Pattern.compile("\r\n|\r|\n");

	private String source;
	private int tabSize;
	/**
	 * Edges to be built after visiting the statement used as the key.
	 * <p>
	 * After a statement is visited, code checks whether there are edges to
	 * build and creates them.
	 * </p>
	 * <p>
	 * This is only useful when labels are used with break or continue
	 * statements which can send control flow back to any parent statement.
	 * </p>
	 */
	private final Map<Statement, Map<CFGEdgeBuilder, Boolean>> edgesToBuild =
			new HashMap<Statement, Map<CFGEdgeBuilder, Boolean>>();
	/** The exit block for the CFG being built */
	private CFGBasicBlock exitBlock;

	public CFGBuilder(String source, int tabSize) {
		this.source = source;
		this.tabSize = tabSize;
	}

	@SuppressWarnings("unchecked")
	private void addVariableAccess(CFGBasicBlock basicBlock, Expression node,
			int flags) {
		if (node == null) {
			return;
		} else if (node instanceof ArrayAccess) {
			ArrayAccess aa = (ArrayAccess) node;
			addVariableAccess(basicBlock, aa.getArray(), flags);
			addVariableAccess(basicBlock, aa.getIndex(), flags);
		} else if (node instanceof ArrayCreation) {
			ArrayCreation ac = (ArrayCreation) node;
			addVariableAccess(basicBlock, ac.getInitializer(), flags);
			addVariableAccesses(basicBlock, ac.dimensions(), flags);
		} else if (node instanceof ArrayInitializer) {
			ArrayInitializer ai = (ArrayInitializer) node;
			addVariableAccesses(basicBlock, ai.expressions(), flags);
		} else if (node instanceof Assignment) {
			Assignment a = (Assignment) node;
			addVariableAccess(basicBlock, a.getLeftHandSide(), WRITE);
			addVariableAccess(basicBlock, a.getRightHandSide(), READ);
		} else if (node instanceof BooleanLiteral
				|| node instanceof CharacterLiteral
				|| node instanceof NullLiteral || node instanceof NumberLiteral
				|| node instanceof StringLiteral || node instanceof TypeLiteral) {
			// nothing to do
		} else if (node instanceof CastExpression) {
			CastExpression ce = (CastExpression) node;
			addVariableAccess(basicBlock, ce.getExpression(), flags);
		} else if (node instanceof ClassInstanceCreation) {
			ClassInstanceCreation cic = (ClassInstanceCreation) node;
			addVariableAccess(basicBlock, cic.getExpression(), flags);
			addVariableAccesses(basicBlock, cic.arguments(), flags);
		} else if (node instanceof ConditionalExpression) {
			ConditionalExpression ce = (ConditionalExpression) node;
			addVariableAccess(basicBlock, ce.getExpression(), flags);
			addVariableAccess(basicBlock, ce.getThenExpression(), flags);
			addVariableAccess(basicBlock, ce.getElseExpression(), flags);
		} else if (node instanceof FieldAccess) {
			FieldAccess fa = (FieldAccess) node;
			basicBlock.addVariableAccess(new VariableAccess(fa, flags));
		} else if (node instanceof InfixExpression) {
			InfixExpression ie = (InfixExpression) node;
			addVariableAccess(basicBlock, ie.getLeftOperand(), flags);
			addVariableAccess(basicBlock, ie.getRightOperand(), flags);
		} else if (node instanceof InstanceofExpression) {
			InstanceofExpression ie = (InstanceofExpression) node;
			addVariableAccess(basicBlock, ie.getLeftOperand(), flags);
		} else if (node instanceof MethodInvocation) {
			MethodInvocation mi = (MethodInvocation) node;
			addVariableAccess(basicBlock, mi.getExpression(), flags);
			addVariableAccesses(basicBlock, mi.arguments(), flags);
		} else if (node instanceof SimpleName) {
			SimpleName sn = (SimpleName) node;
			basicBlock.addVariableAccess(new VariableAccess(sn, flags));
		} else if (node instanceof QualifiedName) {
			QualifiedName qn = (QualifiedName) node;
			basicBlock.addVariableAccess(new VariableAccess(qn, flags));
		} else if (node instanceof ParenthesizedExpression) {
			ParenthesizedExpression pe = (ParenthesizedExpression) node;
			addVariableAccess(basicBlock, pe.getExpression(), flags);
		} else if (node instanceof PostfixExpression) {
			PostfixExpression pe = (PostfixExpression) node;
			addVariableAccess(basicBlock, pe.getOperand(), flags);
		} else if (node instanceof PrefixExpression) {
			PrefixExpression pe = (PrefixExpression) node;
			addVariableAccess(basicBlock, pe.getOperand(), flags);
		} else if (node instanceof SuperFieldAccess) {
			SuperFieldAccess sfa = (SuperFieldAccess) node;

			addVariableAccess(basicBlock, sfa.getQualifier(), flags);
			addVariableAccess(basicBlock, sfa.getName(), flags);
		} else if (node instanceof SuperMethodInvocation) {
			SuperMethodInvocation smi = (SuperMethodInvocation) node;
			addVariableAccess(basicBlock, smi.getQualifier(), flags);
			addVariableAccess(basicBlock, smi.getName(), flags);
		} else if (node instanceof ThisExpression) {
			ThisExpression te = (ThisExpression) node;
			// TODO JNR remember use of "this" here
			addVariableAccess(basicBlock, te.getQualifier(), flags);
		} else if (node instanceof VariableDeclarationExpression) {
			addDeclarations(basicBlock, (VariableDeclarationExpression) node);
		} else {
			throw new NotImplementedException(node);
		}
	}

	private void addVariableAccesses(CFGBasicBlock basicBlock,
			List<Expression> expressions, int flags) {
		for (Expression exor : expressions) {
			addVariableAccess(basicBlock, exor, flags);
		}
	}

	private void addDeclarations(CFGBasicBlock basicBlock,
			List<VariableDeclarationFragment> fragments, Type type) {
		for (VariableDeclarationFragment vdf : fragments) {
			addDeclaration(basicBlock, vdf, type);
		}
	}

	private void addDeclaration(final CFGBasicBlock basicBlock,
			VariableDeclarationFragment vdf, Type type) {
		final int accessType = vdf.getInitializer() == null ? DECL_UNINIT
				: DECL_INIT | WRITE;
		basicBlock.addVariableAccess(new VariableAccess(vdf, vdf.getName(),
				type, accessType));
	}

	private void addDeclarations(CFGBasicBlock basicBlock,
			final VariableDeclarationExpression vde) {
		addDeclarations(basicBlock, vde.fragments(), vde.getType());

	}

	private void addDeclarations(CFGBasicBlock basicBlock,
			List<SingleVariableDeclaration> varDecls) {
		for (SingleVariableDeclaration varDecl : varDecls) {
			addDeclaration(basicBlock, varDecl);
		}
	}

	private void addDeclaration(final CFGBasicBlock basicBlock,
			final SingleVariableDeclaration varDecl) {
		addDeclaration(basicBlock, varDecl, DECL_INIT);
	}

	private void addDeclaration(CFGBasicBlock basicBlock,
			SingleVariableDeclaration varDecl, int flags) {
		basicBlock.addVariableAccess(new VariableAccess(varDecl, varDecl
				.getName(), varDecl.getType(), flags));
	}

	private LivenessState buildCFG(Statement node, LivenessState state) {
		if (node == null) {
			return state.nextStmtWillCreateNewBlock();
		}
		try {
			final Method m = getClass().getMethod("buildCFG", node.getClass(), LivenessState.class);
			return (LivenessState) m.invoke(this, node, state);
		} catch (Exception e) {
			throw new UnhandledException(e);
		}
	}

	public void buildCFG(QualifiedName node) {
		throw new NotImplementedException();
	}

	public void buildCFG(PrimitiveType node) {
		throw new NotImplementedException();
	}

	public void buildCFG(QualifiedType node) {
		throw new NotImplementedException();
	}

	public void buildCFG(PrefixExpression node) {
		throw new NotImplementedException();
	}

	public void buildCFG(PostfixExpression node) {
		throw new NotImplementedException();
	}

	public void buildCFG(ParenthesizedExpression node) {
		throw new NotImplementedException();
	}

	public void buildCFG(SingleVariableDeclaration node) {
		throw new NotImplementedException();
	}

	public void buildCFG(SimpleType node) {
		throw new NotImplementedException();
	}

	public void buildCFG(SimpleName node) {
		throw new NotImplementedException();
	}

	public LivenessState buildCFG(ReturnStatement node, LivenessState state) {
		final CFGBasicBlock basicBlock = getCFGBasicBlock(node, state);
		if (node.getExpression() != null) {
			addVariableAccess(basicBlock, node.getExpression(), READ);
		}
		buildEdge(basicBlock, this.exitBlock);
		return state.nextStmtsAreDeadCode();
	}

	public void buildCFG(Modifier node) {
		throw new NotImplementedException();
	}

	public void buildCFG(MethodInvocation node) {
		// TODO JNR add variable access to "this"
		throw new NotImplementedException();
	}

	public CFGBasicBlock buildCFG(MethodDeclaration node) {
		final CFGBasicBlock entryBlock = newEntryBlock(node);
		this.exitBlock = newExitBlock(node);

		addDeclarations(entryBlock, node.parameters());

		try {
			final CFGEdgeBuilder liveEdge = new CFGEdgeBuilder(entryBlock);
			final LivenessState liveAfterBody = buildCFG(node.getBody(), LivenessState.of(liveEdge));
			if (!liveAfterBody.liveEdges.isEmpty()) {
				if (node.getReturnType2() == null
						|| node.getReturnType2().resolveBinding() == null // added for unit tests
						|| "void".equals(node.getReturnType2().resolveBinding().getName())) {
					buildEdges(liveAfterBody, exitBlock);
				} else {
					throw new IllegalStateException("Did not expect to find any edges to build for a constructor or a non void method return type.");
				}
			}
			if (!this.edgesToBuild.isEmpty()) {
				throw new IllegalStateException(
						"At this point, there should not be any edges left to build. Left edges: " + this.edgesToBuild);
			}
			// new CFGDotPrinter().toDot(entryBlock);
			// new CodePathCollector().getPaths(entryBlock);
			return entryBlock;
		} finally {
			this.exitBlock = null;
		}
	}

	public void buildCFG(MethodRefParameter node) {
		throw new NotImplementedException();
	}

	public void buildCFG(MethodRef node) {
		throw new NotImplementedException();
	}

	public void buildCFG(MemberValuePair node) {
		throw new NotImplementedException();
	}

	public void buildCFG(ParameterizedType node) {
		throw new NotImplementedException();
	}

	public void buildCFG(NumberLiteral node) {
		throw new NotImplementedException();
	}

	public void buildCFG(NullLiteral node) {
		throw new NotImplementedException();
	}

	public void buildCFG(UnionType node) {
		throw new NotImplementedException();
	}

	public void buildCFG(TypeParameter node) {
		throw new NotImplementedException();
	}

	public void buildCFG(TypeLiteral node) {
		throw new NotImplementedException();
	}

	public void buildCFG(TypeDeclarationStatement node) {
		throw new NotImplementedException();
	}

	public List<CFGBasicBlock> buildCFG(TypeDeclaration node) {
		if (!node.isInterface()) {
			List<CFGBasicBlock> results = new LinkedList<CFGBasicBlock>();
			for (FieldDeclaration fieldDecl : node.getFields()) {
				buildCFG(fieldDecl);
			}
			for (MethodDeclaration methodDecl : node.getMethods()) {
				results.add(buildCFG(methodDecl));
			}
			for (TypeDeclaration typeDeclaration : node.getTypes()) {
				buildCFG(typeDeclaration);
			}
			// for (BodyDeclaration bodyDeclaration : (List<BodyDeclaration>)
			// node.bodyDeclarations()) {
			// buildCFG(bodyDeclaration);
			// }
			return results;
		}
		return Collections.emptyList();
	}

	public void buildCFG(TryStatement node) {
		throw new NotImplementedException();
	}

	public void buildCFG(WildcardType node) {
		throw new NotImplementedException();
	}

	public LivenessState buildCFG(WhileStatement node, LivenessState state) {
		final CFGBasicBlock conditionBlock = getCFGBasicBlock(node.getExpression(), state.nextStmtWillCreateNewBlock());
		addVariableAccess(conditionBlock, node.getExpression(), READ);

		final CFGEdgeBuilder liveEdge = new CFGEdgeBuilder(node.getExpression(), true, conditionBlock);
		final LivenessState liveAfterStmt = buildCFG(node.getBody(), LivenessState.of(liveEdge));
		liveAfterStmt.add(new CFGEdgeBuilder(node.getExpression(), false, conditionBlock));
		buildEdgesAfterBranchableStmt(node, liveAfterStmt, conditionBlock);
		return liveAfterStmt.nextStmtWillCreateNewBlock();
	}

	public void buildCFG(VariableDeclarationFragment node) {
		throw new NotImplementedException();
	}

	public LivenessState buildCFG(VariableDeclarationStatement node, LivenessState state) {
		final CFGBasicBlock basicBlock = getCFGBasicBlock(node, state);
		addDeclarations(basicBlock, node.fragments(), node.getType());
		return getInBlockStmtResult(state, basicBlock);
	}

	public void buildCFG(VariableDeclarationExpression node) {
		throw new NotImplementedException();
	}

	public LivenessState buildCFG(SwitchStatement node, LivenessState state) {
		final CFGBasicBlock basicBlock = getCFGBasicBlock(node, state);
		final LivenessState liveBeforeBody = new LivenessState(basicBlock, new CFGEdgeBuilder(basicBlock));
		final LivenessState liveAfterBody = buildCFG(node.statements(), liveBeforeBody);
		liveAfterBody.add(new CFGEdgeBuilder(basicBlock));

		buildEdgesAfterBranchableStmt(node, liveAfterBody, basicBlock);
		return liveAfterBody.nextStmtWillCreateNewBlock();
	}

	public LivenessState buildCFG(SwitchCase node, CFGBasicBlock switchConditionBasicBlock, LivenessState state) {
		// the current live blocks will be empty if there was a break,
		// or populated in case of fall-through.

		// add an edge going from the condition of the switch
		// (state.liveBasicBlock is the condition of the switch)
		state.add(new CFGEdgeBuilder(node.getExpression(), true, switchConditionBasicBlock));
		return state.nextStmtWillCreateNewBlock();
	}

	public void buildCFG(SuperMethodInvocation node) {
		throw new NotImplementedException();
	}

	public void buildCFG(SuperFieldAccess node) {
		throw new NotImplementedException();
	}

	public LivenessState buildCFG(SuperConstructorInvocation node, LivenessState state) {
		final CFGBasicBlock basicBlock = getCFGBasicBlock(node, state);
		addVariableAccesses(basicBlock, node.arguments(), READ);
		return getInBlockStmtResult(state, basicBlock);
	}

	public void buildCFG(StringLiteral node) {
		throw new NotImplementedException();
	}

	public void buildCFG(ThrowStatement node) {
		throw new NotImplementedException();
	}

	public void buildCFG(ThisExpression node) {
		throw new NotImplementedException();
	}

	public void buildCFG(TextElement node) {
		throw new NotImplementedException();
	}

	public void buildCFG(TagElement node) {
		throw new NotImplementedException();
	}

	public LivenessState buildCFG(SynchronizedStatement node, LivenessState state) {
		CFGBasicBlock basicBlock = getCFGBasicBlock(node, state.nextStmtWillCreateNewBlock());
		addVariableAccess(basicBlock, node.getExpression(), READ);
		CFGEdgeBuilder liveEdge = new CFGEdgeBuilder(basicBlock);
		LivenessState result = buildCFG(node.getBody(), LivenessState.of(liveEdge));
		return result.nextStmtWillCreateNewBlock();
	}

	public void buildCFG(CatchClause node) {
		throw new NotImplementedException();
	}

	public void buildCFG(CastExpression node) {
		throw new NotImplementedException();
	}

	public LivenessState buildCFG(BreakStatement node, LivenessState state) {
		final CFGBasicBlock basicBlock = getCFGBasicBlock(node, state);
		final Statement targetStmt;
		if (node.getLabel() != null) {
			targetStmt = findLabeledParentStmt(node);
		} else {
			targetStmt = findBreakableParentStmt(node);
		}
		addEdgeToBuild(targetStmt, new CFGEdgeBuilder(basicBlock), true);
		return state.copyLiveBasicBlock();
	}

	private Statement findLabeledParentStmt(ASTNode node) {
		ASTNode n = node;
		while (n != null && !(n instanceof LabeledStatement)) {
			n = n.getParent();
		}
		if (n != null) {
			return ((LabeledStatement) n).getBody();
		}
		return null;
	}

	private Statement findBreakableParentStmt(ASTNode node) {
		ASTNode n = node;
		while (n != null && !ASTHelper.isBreakable(n)) {
			n = n.getParent();
		}
		if (n != null) {
			return (Statement) n;
		}
		return null;
	}

	public void buildCFG(BooleanLiteral node) {
		throw new NotImplementedException();
	}

	public LivenessState buildCFG(ConstructorInvocation node, LivenessState state) {
		final CFGBasicBlock basicBlock = getCFGBasicBlock(node, state);
		addVariableAccesses(basicBlock, node.arguments(), READ);
		return getInBlockStmtResult(state, basicBlock);
	}

	public void buildCFG(ConditionalExpression node) {
		throw new NotImplementedException();
	}

	public List<CFGBasicBlock> buildCFG(CompilationUnit node) {
		List<CFGBasicBlock> results = new LinkedList<CFGBasicBlock>();
		for (AbstractTypeDeclaration decl : (List<AbstractTypeDeclaration>) node.types()) {
			if (decl instanceof TypeDeclaration) {
				results.addAll(buildCFG((TypeDeclaration) decl));
			} else {
				throw new NotImplementedException(node);
			}
		}
		return results;
	}

	public void buildCFG(ClassInstanceCreation node) {
		throw new NotImplementedException();
	}

	public void buildCFG(CharacterLiteral node) {
		throw new NotImplementedException();
	}

	public void buildCFG(ArrayCreation node) {
		throw new NotImplementedException();
	}

	public void buildCFG(ArrayAccess node) {
		throw new NotImplementedException();
	}

	public void buildCFG(AnonymousClassDeclaration node) {
		throw new NotImplementedException();
	}

	public LivenessState buildCFG(Block node, LivenessState state) {
		LivenessState liveState = state;
		try {
			liveState = buildCFG(node.statements(), state);
		} finally {
			moveAllEdgesToBuild(node, liveState);
		}
		return liveState;
	}

	private LivenessState buildCFG(List<Statement> stmts, final LivenessState startState) {
		LivenessState liveState = startState;
		for (Statement stmt : stmts) {
			if (stmt instanceof AssertStatement) {
				liveState = buildCFG((AssertStatement) stmt, liveState);
			// } else if (stmt instanceof Block) {
			// buildCFG((Block) stmt, liveState);
			} else if (stmt instanceof BreakStatement) {
				liveState = buildCFG((BreakStatement) stmt, liveState);
			} else if (stmt instanceof ConstructorInvocation) {
				liveState = buildCFG(stmt, liveState);
			} else if (stmt instanceof ContinueStatement) {
				liveState = buildCFG((ContinueStatement) stmt, liveState);
			} else if (stmt instanceof DoStatement) {
				liveState = buildCFG((DoStatement) stmt, liveState);
			} else if (stmt instanceof EmptyStatement) {
				liveState = buildCFG((EmptyStatement) stmt, liveState);
			} else if (stmt instanceof EnhancedForStatement) {
				liveState = buildCFG((EnhancedForStatement) stmt, liveState);
			} else if (stmt instanceof ExpressionStatement) {
				liveState = buildCFG((ExpressionStatement) stmt, liveState);
			} else if (stmt instanceof ForStatement) {
				liveState = buildCFG((ForStatement) stmt, liveState);
			} else if (stmt instanceof IfStatement) {
				liveState = buildCFG((IfStatement) stmt, liveState);
			} else if (stmt instanceof LabeledStatement) {
				liveState = buildCFG((LabeledStatement) stmt, liveState);
			} else if (stmt instanceof ReturnStatement) {
				liveState = buildCFG((ReturnStatement) stmt, liveState);
			} else if (stmt instanceof SuperConstructorInvocation) {
				liveState = buildCFG(stmt, liveState);
			} else if (stmt instanceof SwitchCase) {
				// Here, use startState.liveBasicBlock to build an edge
				// from the switch condition to the case statement
				liveState = buildCFG((SwitchCase) stmt, startState.liveBasicBlock, liveState);
				// next statement will always create a new basicBlock
			} else if (stmt instanceof SwitchStatement) {
				liveState = buildCFG((SwitchStatement) stmt, liveState);
			} else if (stmt instanceof SynchronizedStatement) {
				liveState = buildCFG((SynchronizedStatement) stmt, liveState);
				// } else if (stmt instanceof ThrowStatement) {
				// buildCFG((ThrowStatement) stmt, liveState);
				// } else if (stmt instanceof TryStatement) {
				// buildCFG((TryStatement) stmt, liveState);
				// } else if (stmt instanceof TypeDeclarationStatement) {
				// buildCFG((TypeDeclarationStatement) stmt, liveState);
			} else if (stmt instanceof VariableDeclarationStatement) {
				liveState = buildCFG((VariableDeclarationStatement) stmt, liveState);
			} else if (stmt instanceof WhileStatement) {
				liveState = buildCFG((WhileStatement) stmt, liveState);
			} else {
				throw new NotImplementedException(stmt);
			}
		}
		return liveState;
	}

	public void buildCFG(Assignment node) {
		throw new NotImplementedException();
	}

	public LivenessState buildCFG(AssertStatement node, LivenessState state) {
		CFGBasicBlock basicBlock = getCFGBasicBlock(node, state);
		addVariableAccess(basicBlock, node.getExpression(), READ);
		addVariableAccess(basicBlock, node.getMessage(), READ);
		return getInBlockStmtResult(state, basicBlock);
	}

	public void buildCFG(ArrayType node) {
		throw new NotImplementedException();
	}

	public void buildCFG(ArrayInitializer node) {
		throw new NotImplementedException();
	}

	public void buildCFG(Initializer node) {
		throw new NotImplementedException();
	}

	public void buildCFG(InstanceofExpression node) {
		throw new NotImplementedException();
	}

	public void buildCFG(InfixExpression node) {
		throw new NotImplementedException();
	}

	/**
	 * Builds a CFG for the passed in statement.
	 *
	 * @param node
	 *          the statement for which to build a CFG
	 * @param state
	 *          the liveness state before the current statement.
	 *          It contains: the live edges before the current statement,
	 *          the live basic block to which the current statement might be
	 *          added.
	 *          If null, then the new basic block must be created for the
	 *          current statement.
	 * @return the new live state after the current statement
	 */
	public LivenessState buildCFG(IfStatement node, LivenessState state) {
		final CFGBasicBlock exprBlock = getCFGBasicBlock(node, state.nextStmtWillCreateNewBlock(), true);
		try {
			addVariableAccess(exprBlock, node.getExpression(), READ);

			final LivenessState result = new LivenessState();
			CFGEdgeBuilder thenEdge = new CFGEdgeBuilder(node.getExpression(), true, exprBlock);
			result.addAll(buildCFG(node.getThenStatement(), LivenessState.of(thenEdge)));

			final Statement elseStmt = node.getElseStatement();
			CFGEdgeBuilder elseEdge = new CFGEdgeBuilder(node.getExpression(), false, exprBlock);
			if (elseStmt != null) {
				result.addAll(buildCFG(elseStmt, LivenessState.of(elseEdge)));
			} else {
				result.add(elseEdge);
			}
			return result.nextStmtWillCreateNewBlock();
		} finally {
			moveAllEdgesToBuild(node, state);
		}
	}

	private void addEdgeToBuild(final Statement node, CFGEdgeBuilder builder, boolean isBreakStmt) {
		if (builder != null) {
			Map<CFGEdgeBuilder, Boolean> builders = this.edgesToBuild.get(node);
			if (builders == null) {
				builders = new HashMap<CFGEdgeBuilder, Boolean>();
				this.edgesToBuild.put(node, builders);
			}
			builders.put(builder, isBreakStmt);
		}
	}

	private void moveAllEdgesToBuild(Statement node, LivenessState state) {
		final Map<CFGEdgeBuilder, Boolean> toBuild = this.edgesToBuild.remove(node);
		if (toBuild != null) {
			state.addAll(toBuild.keySet());
		}
	}

	private void buildEdges(final LivenessState toBuild, final CFGBasicBlock targetBlock) {
		if (isNotEmpty(toBuild.liveEdges)) {
			for (CFGEdgeBuilder builder : toBuild.liveEdges) {
				builder.withTarget(targetBlock).build();
			}
			toBuild.liveEdges.clear();
		}
	}

	private void buildEdgesAfterBranchableStmt(Statement node,
			final LivenessState liveAfterBranchableStmt, final CFGBasicBlock whereToBranchBlock) {
		final Map<CFGEdgeBuilder, Boolean> toBuild = this.edgesToBuild.remove(node);
		if (isNotEmpty(toBuild)) {
			Set<Entry<CFGEdgeBuilder, Boolean>> set = toBuild.entrySet();
			for (Iterator iter = set.iterator(); iter.hasNext();) {
				Entry<CFGEdgeBuilder, Boolean> entry =
						(Entry<CFGEdgeBuilder, Boolean>) iter.next();
				final CFGEdgeBuilder builder = entry.getKey();
				final boolean isBreakStmt = entry.getValue();
				if (isBreakStmt) {
					liveAfterBranchableStmt.add(builder);
				} else { // this is a continue statement
					builder.withTarget(whereToBranchBlock).build();
					iter.remove();
				}
			}
		}
	}

	public void buildCFG(MemberRef node) {
		throw new NotImplementedException();
	}

	public LivenessState buildCFG(LabeledStatement node, LivenessState state) {
		// does not count as an executable node, so do not get a basic block for it
		return buildCFG(node.getBody(), state);
	}

	public LivenessState buildCFG(EnhancedForStatement node, LivenessState state) {
		final CFGBasicBlock basicBlock = getCFGBasicBlock(node, state.nextStmtWillCreateNewBlock());

		addDeclaration(basicBlock, node.getParameter(), DECL_INIT | WRITE);

		final LivenessState newLiveState = LivenessState.of(new CFGEdgeBuilder(basicBlock));
		final LivenessState liveAfterBody = buildCFG(node.getBody(), newLiveState);
		buildEdges(liveAfterBody, basicBlock);

		final LivenessState liveAfterStmt = LivenessState.of(new CFGEdgeBuilder(basicBlock));
		buildEdgesAfterBranchableStmt(node, liveAfterStmt, basicBlock);
		return liveAfterStmt.nextStmtWillCreateNewBlock();
	}

	public LivenessState buildCFG(EmptyStatement node, LivenessState state) {
		CFGBasicBlock basicBlock = getCFGBasicBlock(node, state);
		return getInBlockStmtResult(state, basicBlock);
	}

	private LivenessState getInBlockStmtResult(LivenessState state, CFGBasicBlock basicBlock) {
		if (state.liveBasicBlock == null) {
			// new block was created for current node
			return new LivenessState(basicBlock, new CFGEdgeBuilder(basicBlock));
		}
		return state;
	}

	public LivenessState buildCFG(DoStatement node, LivenessState state) {
		final CFGBasicBlock basicBlock = getCFGBasicBlock(node, state.nextStmtWillCreateNewBlock());
		final LivenessState newLiveState = new LivenessState(basicBlock, new CFGEdgeBuilder(basicBlock));
		final LivenessState liveAfterLoop = buildCFG(node.getBody(), newLiveState);
		CFGBasicBlock conditionBlock = getCFGBasicBlock(node.getExpression(), liveAfterLoop.nextStmtWillCreateNewBlock());
		addVariableAccess(conditionBlock, node.getExpression(), READ);

		buildEdge(node.getExpression(), true, conditionBlock, basicBlock);

		LivenessState liveAfterStmt = LivenessState.of(new CFGEdgeBuilder(node.getExpression(), false, conditionBlock));
		buildEdgesAfterBranchableStmt(node, liveAfterStmt, basicBlock);
		return liveAfterStmt;
	}

	public LivenessState buildCFG(ContinueStatement node, LivenessState state) {
		final CFGBasicBlock basicBlock = getCFGBasicBlock(node, state);
		final Statement targetStmt;
		if (node.getLabel() != null) {
			targetStmt = findLabeledParentStmt(node);
		} else {
			targetStmt = findContinuableParentStmt(node);
		}
		addEdgeToBuild(targetStmt, new CFGEdgeBuilder(basicBlock), false);
		return state.copyLiveBasicBlock();
	}

	private Statement findContinuableParentStmt(ASTNode node) {
		ASTNode n = node;
		while (n != null && !ASTHelper.isLoop(n)) {
			n = n.getParent();
		}
		if (n != null) {
			return (Statement) n;
		}
		return null;
	}

	public LivenessState buildCFG(ForStatement node, LivenessState state) {
		final CFGBasicBlock initBlock = getCFGBasicBlock(node.initializers(), state);
		final LivenessState initLiveBlock = LivenessState.of(new CFGEdgeBuilder(initBlock));
		final CFGBasicBlock exprBlock = getCFGBasicBlock(node.getExpression(), initLiveBlock, true);
		final CFGBasicBlock updatersBlock = getCFGBasicBlock(node.updaters(), new LivenessState());
		buildEdge(updatersBlock, exprBlock);

		for (Expression expression : (List<Expression>) node.initializers()) {
			if (expression instanceof VariableDeclarationExpression) {
				addDeclarations(initBlock, (VariableDeclarationExpression) expression);
			}
		}
		addVariableAccess(exprBlock, node.getExpression(), READ);
		addVariableAccesses(updatersBlock, node.updaters(), WRITE);

		CFGEdgeBuilder liveBlock = new CFGEdgeBuilder(node.getExpression(), true, exprBlock);
		final LivenessState liveAfterBody = buildCFG(node.getBody(), LivenessState.of(liveBlock));
		buildEdges(liveAfterBody, updatersBlock);

		final LivenessState liveAfterStmt = LivenessState.of(new CFGEdgeBuilder(
				node.getExpression(), false, exprBlock));
		buildEdgesAfterBranchableStmt(node, liveAfterStmt, updatersBlock);
		return liveAfterStmt.nextStmtWillCreateNewBlock();
	}

	public void buildCFG(FieldDeclaration node) {
		throw new NotImplementedException();
	}

	public void buildCFG(FieldAccess node) {
		throw new NotImplementedException();
	}

	public LivenessState buildCFG(ExpressionStatement node, LivenessState state) {
		final CFGBasicBlock basicBlock = getCFGBasicBlock(node, state);
		addVariableAccess(basicBlock, node.getExpression(), READ);
		return getInBlockStmtResult(state, basicBlock);
	}

	private CFGBasicBlock getCFGBasicBlock(ASTNode node, LivenessState state) {
		return getCFGBasicBlock(node, state, false);
	}

	/**
	 * Will create and return a new CFGBasicBlock for the passed in node, if the liveBasicBlock is null, otherwise
	 * it will return the liveBasicBlock.
	 *
	 * @param node
	 * @param state the liveness state the current statement will be added to.
	 *        A null liveBasicBlock forces the creation of a new CFGBasicBlock.
	 *        liveEdges are the edges that are live before getting the CFGBasicBlock
	 * @param isDecision used for building the associated CFGEdge
	 * @return
	 */
	private CFGBasicBlock getCFGBasicBlock(ASTNode node, LivenessState state, boolean isDecision) {
		final Map<CFGEdgeBuilder, Boolean> toBuild = this.edgesToBuild.remove(node);
		if (isNotEmpty(toBuild)) {
			throw new RuntimeException("No edges to build should exist for node \"" + node
				+ "\" before a CFGBasicBlock is created for it. Found the following edges to build " + toBuild);
		}
		if (!state.requireNewBlock()) {
			final CFGBasicBlock basicBlock = state.liveBasicBlock;
			// TODO JNR add nodes to the basicBlock they belong to
			// and adapt the CFGDotPrinter to display "..." after the first node
			// basicBlock.addNode(node);
			return basicBlock;
		}
		final LineAndColumn lineCol = getLineAndColumn(node);
		final CFGBasicBlock basicBlock = new CFGBasicBlock(node,
				getFileName(node), codeExcerpt(node), isDecision, lineCol);
		buildEdges(state, basicBlock);
		return basicBlock;
	}

	private CFGBasicBlock getCFGBasicBlock(List<Expression> expressions, LivenessState state) {
		if (isNotEmpty(expressions)) {
			final Expression firstExpr = expressions.get(0);
			final LineAndColumn lineCol = getLineAndColumn(firstExpr.getStartPosition());
			final CFGBasicBlock basicBlock = new CFGBasicBlock(expressions.get(0),
					getFileName(firstExpr), codeExcerpt(expressions), false, lineCol);
			buildEdges(state, basicBlock);
			return basicBlock;
		}
		throw new NotImplementedException("for empty expressions list");
	}

	private CFGBasicBlock newEntryBlock(MethodDeclaration node) {
		return CFGBasicBlock.buildEntryBlock(node, getFileName(node),
				codeExcerpt(node));
	}

	private CFGBasicBlock newExitBlock(MethodDeclaration node) {
		final LineAndColumn lineCol = getLineAndColumn(node
				.getStartPosition() + node.getLength());
		return CFGBasicBlock.buildExitBlock(node, getFileName(node),
				codeExcerpt(node), lineCol);
	}

	private LineAndColumn getLineAndColumn(ASTNode node) {
		return getLineAndColumn(node.getStartPosition());
	}

	private LineAndColumn getLineAndColumn(final int position) {
		// TODO Use CompilationUnit.getLineNumber() and CompilationUnit.getColumnNumber()
		// Return SourceLocation class with also startNodePosition to be used for graph node names
		// line number and column number are then used as comments for the node
		// file starts with line 1
		int lineNo = 1;
		int lastMatchPosition = 0;
		final Matcher matcher = NEWLINE.matcher(source);
		while (matcher.find()) {
			final MatchResult matchResult = matcher.toMatchResult();
			if (matchResult.end() >= position) {
				final String startOfLine = this.source.substring(
						lastMatchPosition, position);
				final int nbChars = countCharacters(startOfLine, tabSize);
				// + 1 because line starts with column 1
				return new LineAndColumn(position, lineNo, nbChars + 1);
			}
			lastMatchPosition = matchResult.end();
			++lineNo;
		}
		throw new IllegalStateException(
				"A line and column number should have been found");
	}

	private int countCharacters(String s, int tabSize) {
		int result = 0;
		for (int i = 0; i < s.length(); i++) {
			if (s.charAt(i) == '\t') {
				result += tabSize - (i % tabSize);
			} else {
				result++;
			}
		}
		return result;
	}

	private boolean isNotEmpty(final Collection<?> col) {
		return col != null && !col.isEmpty();
	}

	private boolean isNotEmpty(final Map<?, ?> col) {
		return col != null && !col.isEmpty();
	}

}

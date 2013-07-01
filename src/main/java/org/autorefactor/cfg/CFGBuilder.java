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

import static org.autorefactor.cfg.ASTPrintHelper.*;
import static org.autorefactor.cfg.VariableAccess.*;

import java.lang.reflect.Method;
import java.util.*;
import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.autorefactor.refactoring.ASTHelper;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.*;

/**
 * Builds a CFG.
 * <p>
 * Look at {@link #buildCFG(IfStatement, List, CFGBasicBlock)} for a javadoc for
 * all the buildCFG(*Statement, List<CFGEdgeBuilder>, CFGBasicBlock) methods.
 */
public class CFGBuilder {

	private String source;
	private int tabSize;
	/**
	 * Edges to be built before or after visiting the statement used as the key.
	 * When the statement is visited, code checks whether there are edges to
	 * build and creates them. After the statement was visited, code checks
	 * whether what edges where added for build and decides whether to build
	 * them now (loops) or to forward them to the next statement for building
	 * them (decision statement).
	 * <p>
	 * Will probably need to revisit this Map to only use it with labels, so
	 * only check its content after visiting a node.
	 */
	private final Map<Statement, Set<CFGEdgeBuilder>> edgesToBuild = new HashMap<Statement, Set<CFGEdgeBuilder>>();
	/** The exit block for the CFG being built */
	private CFGBasicBlock exitBlock;

	public CFGBuilder(String source, int tabSize) {
		this.source = source;
		this.tabSize = tabSize;
	}

	@SuppressWarnings("unchecked")
	private void addVariableAccess(CFGBasicBlock basicBlock, Expression node,
			int flags) {
		if (node instanceof ArrayAccess) {
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
			// } else if (node instanceof SuperFieldAccess) {
			// SuperFieldAccess sfa = (SuperFieldAccess) node;
			// } else if (node instanceof SuperMethodInvocation) {
			// SuperMethodInvocation smi = (SuperMethodInvocation) node;
			// } else if (node instanceof ThisExpression) {
			// ThisExpression te = (ThisExpression) node;
		} else if (node instanceof VariableDeclarationExpression) {
			addDeclarations(basicBlock, (VariableDeclarationExpression) node);
		} else {
			throw new RuntimeException(notImplementedFor(node));
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

	@SuppressWarnings("unchecked")
	private List<CFGEdgeBuilder> buildCFG(Statement node,
			List<CFGEdgeBuilder> liveBlocks, CFGBasicBlock currentBasicBlock) {
		if (node == null) {
			return Collections.emptyList();
		}
		try {
			final Method m = getClass().getMethod("buildCFG", node.getClass(), List.class, CFGBasicBlock.class);
			return (List<CFGEdgeBuilder>) m.invoke(this, node, liveBlocks, currentBasicBlock);
		} catch (Exception e) {
			throw new RuntimeException("Unhandled exception", e);
		}
	}

	public void buildCFG(QualifiedName node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(PrimitiveType node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(QualifiedType node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(PrefixExpression node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(PostfixExpression node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(ParenthesizedExpression node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(SingleVariableDeclaration node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(SimpleType node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(SimpleName node) {
		throw new RuntimeException("Not implemented");
	}

	public List<CFGEdgeBuilder> buildCFG(ReturnStatement node,
			List<CFGEdgeBuilder> liveBlocks, CFGBasicBlock currentBasicBlock) {
		final CFGBasicBlock basicBlock = newCFGBasicBlock(node, currentBasicBlock);
		addVariableAccess(basicBlock, node.getExpression(), READ);
		buildWithTarget(liveBlocks, basicBlock);
		CFGEdgeBuilder.buildEdge(basicBlock, this.exitBlock);
		return Collections.emptyList();
	}

	public void buildCFG(Modifier node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(MethodInvocation node) {
		throw new RuntimeException("Not implemented");
	}

	public CFGBasicBlock buildCFG(MethodDeclaration node) {
		final CFGBasicBlock entryBlock = newEntryBlock(node);
		this.exitBlock = newExitBlock(node);

		addDeclarations(entryBlock, node.parameters());

		try {
			final List<CFGEdgeBuilder> liveBlocks = newList(new CFGEdgeBuilder(entryBlock));
			final List<CFGEdgeBuilder> edgesToBuild = buildCFG(node.getBody(), liveBlocks, null);
			if (!edgesToBuild.isEmpty()) {
				if ("void".equals(node.getReturnType2().resolveBinding().getName())) {
					for (CFGEdgeBuilder builder : edgesToBuild) {
						builder.withTarget(exitBlock).build();
					}
				} else {
					throw new IllegalStateException("Did not expect to find any edges to build for a non void method return type.");
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
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(MethodRef node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(MemberValuePair node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(ParameterizedType node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(NumberLiteral node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(NullLiteral node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(UnionType node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(TypeParameter node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(TypeLiteral node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(TypeDeclarationStatement node) {
		throw new RuntimeException("Not implemented");
	}

	public CFGBasicBlock buildCFG(TypeDeclaration node) {
		if (!node.isInterface()) {
			for (FieldDeclaration fieldDecl : node.getFields()) {
				buildCFG(fieldDecl);
			}
			for (MethodDeclaration methodDecl : node.getMethods()) {
				buildCFG(methodDecl);
			}
			for (TypeDeclaration typeDeclaration : node.getTypes()) {
				buildCFG(typeDeclaration);
			}
			// for (BodyDeclaration bodyDeclaration : (List<BodyDeclaration>)
			// node.bodyDeclarations()) {
			// buildCFG(bodyDeclaration);
			// }
		}
		return null;
	}

	public void buildCFG(TryStatement node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(WildcardType node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(WhileStatement node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(VariableDeclarationFragment node) {
		throw new RuntimeException("Not implemented");
	}

	public List<CFGEdgeBuilder> buildCFG(VariableDeclarationStatement node,
			List<CFGEdgeBuilder> liveBlocks, CFGBasicBlock currentBasicBlock) {
		final CFGBasicBlock basicBlock = newCFGBasicBlock(node, currentBasicBlock);
		addDeclarations(basicBlock, node.fragments(), node.getType());
		if (currentBasicBlock == null) {
			return newList(new CFGEdgeBuilder(basicBlock));
		} else {
			return liveBlocks;
		}
	}

	public void buildCFG(VariableDeclarationExpression node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(SwitchStatement node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(SwitchCase node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(SuperMethodInvocation node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(SuperFieldAccess node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(SuperConstructorInvocation node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(StringLiteral node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(ThrowStatement node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(ThisExpression node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(TextElement node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(TagElement node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(SynchronizedStatement node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(CatchClause node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(CastExpression node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(BreakStatement node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(BooleanLiteral node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(ConstructorInvocation node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(ConditionalExpression node) {
		throw new RuntimeException("Not implemented");
	}

	public List<CFGBasicBlock> buildCFG(CompilationUnit node) {
		List<CFGBasicBlock> results = new LinkedList<CFGBasicBlock>();
		for (AbstractTypeDeclaration decl : (List<AbstractTypeDeclaration>) node
				.types()) {
			if (decl instanceof TypeDeclaration) {
				results.add(buildCFG((TypeDeclaration) decl));
			} else {
				throw new RuntimeException(notImplementedFor(node));
			}
		}
		return results;
	}

	public void buildCFG(ClassInstanceCreation node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(CharacterLiteral node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(ArrayCreation node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(ArrayAccess node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(AnonymousClassDeclaration node) {
		throw new RuntimeException("Not implemented");
	}

	public List<CFGEdgeBuilder> buildCFG(Block node, List<CFGEdgeBuilder> previousLiveBlocks,
			CFGBasicBlock currentBasicBlock) {
		List<CFGEdgeBuilder> liveBlocks = previousLiveBlocks;
		final CFGBasicBlock basicBlock = newCFGBasicBlock(node, currentBasicBlock);
		if (currentBasicBlock == null) {
			buildWithTarget(previousLiveBlocks, basicBlock);
			liveBlocks = newList(new CFGEdgeBuilder(basicBlock));
		}
		try {
			CFGBasicBlock localBasicBlock = basicBlock;
			for (Statement stmt : (List<Statement>) node.statements()) {
				// if (stmt instanceof AssertStatement) {
				// // AssertStatement as = (AssertStatement) stmt;
				// } else if (stmt instanceof Block) {
				// Block b = (Block) stmt;
				// } else if (stmt instanceof BreakStatement) {
				// BreakStatement bs = (BreakStatement) stmt;
				// } else if (stmt instanceof ConstructorInvocation) {
				// ConstructorInvocation ci = (ConstructorInvocation) stmt;
				// } else if (stmt instanceof ContinueStatement) {
				// ContinueStatement cs = (ContinueStatement) stmt;
				// } else if (stmt instanceof DoStatement) {
				// DoStatement ws = (DoStatement) stmt;
				// } else if (stmt instanceof EmptyStatement) {
				// EmptyStatement es = (EmptyStatement) stmt;
				// } else if (stmt instanceof EnhancedForStatement) {
				// buildCFG((EnhancedForStatement) stmt);
				// } else
				if (stmt instanceof ExpressionStatement) {
					liveBlocks = buildCFG((ExpressionStatement) stmt, liveBlocks, localBasicBlock);
				} else if (stmt instanceof ForStatement) {
					liveBlocks = buildCFG((ForStatement) stmt, liveBlocks, localBasicBlock);
					localBasicBlock = null;
				} else if (stmt instanceof IfStatement) {
					liveBlocks = buildCFG((IfStatement) stmt, liveBlocks, localBasicBlock);
					localBasicBlock = null;
					// } else if (stmt instanceof LabeledStatement) {
					// LabeledStatement ls = (LabeledStatement) stmt;
				} else if (stmt instanceof ReturnStatement) {
					liveBlocks = buildCFG((ReturnStatement) stmt, liveBlocks, localBasicBlock);
					// } else if (stmt instanceof SuperConstructorInvocation) {
					// SuperConstructorInvocation sci =
					// (SuperConstructorInvocation)
					// stmt;
					// } else if (stmt instanceof SwitchCase) {
					// SwitchCase sc = (SwitchCase) stmt;
					// } else if (stmt instanceof SwitchStatement) {
					// SwitchStatement ss = (SwitchStatement) stmt;
					// } else if (stmt instanceof SynchronizedStatement) {
					// SynchronizedStatement ss = (SynchronizedStatement) stmt;
					// } else if (stmt instanceof ThrowStatement) {
					// // ThrowStatement ts = (ThrowStatement) stmt;
					// } else if (stmt instanceof TryStatement) {
					// TryStatement ts = (TryStatement) stmt;
					// } else if (stmt instanceof TypeDeclarationStatement) {
					// TypeDeclarationStatement tds = (TypeDeclarationStatement)
					// stmt;
				} else if (stmt instanceof VariableDeclarationStatement) {
					liveBlocks = buildCFG((VariableDeclarationStatement) stmt, liveBlocks, localBasicBlock);
					// } else if (stmt instanceof WhileStatement) {
					// WhileStatement ws = (WhileStatement) stmt;
					// buildCFG(ws);
				} else {
					throw new RuntimeException(notImplementedFor(stmt));
				}
			}
		} finally {
			moveEdgesToBuild(node);
		}
		return liveBlocks;
	}

	public void buildCFG(Assignment node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(AssertStatement node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(ArrayType node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(ArrayInitializer node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(Initializer node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(InstanceofExpression node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(InfixExpression node) {
		throw new RuntimeException("Not implemented");
	}

	/**
	 * Builds a CFG for the passed in statement.
         *
	 * @param node
	 *          the statement for which to build a CFG
	 * @param liveBlocks
	 *          the List of live blocks before the current statement
	 * @param currentBasicBlock
	 *          the current basic block to which the current statement might be
	 *          added. If null, then the a new basic block must be created for the
	 *          current statement
	 * @return the list of live blocks after the current statement
	 */
	public List<CFGEdgeBuilder> buildCFG(IfStatement node,
			List<CFGEdgeBuilder> liveBlocks, CFGBasicBlock currentBasicBlockIgnored) {
		final CFGBasicBlock exprBlock = newCFGBasicBlock(node, null, true);
		buildWithTarget(liveBlocks, exprBlock);
		try {
			addVariableAccess(exprBlock, node.getExpression(), READ);

			final List<CFGEdgeBuilder> results = new ArrayList<CFGEdgeBuilder>();
			CFGEdgeBuilder liveAfterThen = new CFGEdgeBuilder(node.getExpression(), true, exprBlock);
			results.addAll(buildCFG(node.getThenStatement(), newList(liveAfterThen), null));

			final Statement elseStmt = node.getElseStatement();
			if (elseStmt != null) {
				CFGEdgeBuilder liveAfterElse = new CFGEdgeBuilder(node.getExpression(), false, exprBlock);
				results.addAll(buildCFG(elseStmt, newList(liveAfterElse), null));
			} else {
				results.add(new CFGEdgeBuilder(node.getExpression(), false, exprBlock));
			}
			return results;
		} finally {
			moveEdgesToBuild(node);
		}
	}

	private List<CFGEdgeBuilder> newList(CFGEdgeBuilder... builders) {
		List<CFGEdgeBuilder> result = new ArrayList<CFGEdgeBuilder>();
		for (CFGEdgeBuilder builder : builders) {
			result.add(builder);
		}
		return result;
	}

	private void addEdgeToBuild(final Statement node, CFGEdgeBuilder builder) {
		if (builder != null) {
			Set<CFGEdgeBuilder> builders = this.edgesToBuild.get(node);
			if (builders == null) {
				builders = new HashSet<CFGEdgeBuilder>();
				this.edgesToBuild.put(node, builders);
			}
			builders.add(builder);
		}
	}

	private void addEdgesToBuild(final Statement node,
			Collection<CFGEdgeBuilder> toBuild) {
		if (isNotEmpty(toBuild)) {
			Set<CFGEdgeBuilder> builders = this.edgesToBuild.get(node);
			if (builders == null) {
				builders = new HashSet<CFGEdgeBuilder>();
				this.edgesToBuild.put(node, builders);
			}
			builders.addAll(toBuild);
		}
	}

	private void addEdgesToBuildForNextStatement(Statement node,
			final CFGEdgeBuilder... toBuild) {
		if (toBuild != null && toBuild.length != 0) {
			addEdgesToBuildForNextStatement(node, Arrays.asList(toBuild));
		}
	}

	private void addEdgesToBuildForNextStatement(Statement node,
			final Collection<CFGEdgeBuilder> toBuild) {
		if (isNotEmpty(toBuild)) {
			final Statement nextStmt = ASTHelper.getNextStatement(node);
			for (CFGEdgeBuilder builder : toBuild) {
				addEdgeToBuild(nextStmt, builder);
			}
		}
	}

	private void moveEdgesToBuild(final Statement node) {
		final Set<CFGEdgeBuilder> toBuild = this.edgesToBuild.remove(node);
		if (toBuild == null) {
			return;
		}
		final Statement nextStmt = ASTHelper.getNextStatement(node);
		final ASTNode parent = node.getParent();
		if (nextStmt.getParent() == parent) {
			addEdgesToBuildForNextStatement(node, toBuild);
		} else {
			addEdgesToBuild((Statement) parent, toBuild);
		}
	}

	private void buildWithTarget(final Collection<CFGEdgeBuilder> toBuild,
			final CFGBasicBlock targetBlock) {
		if (isNotEmpty(toBuild)) {
			for (CFGEdgeBuilder builder : toBuild) {
				builder.withTarget(targetBlock).build();
			}
		}
	}

	public void buildCFG(MemberRef node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(LabeledStatement node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(EnhancedForStatement node,
			List<CFGEdgeBuilder> liveBlocks, CFGBasicBlock currentBasicBlock) {
		CFGBasicBlock basicBlock = newCFGBasicBlock(node, null);
		CFGEdgeBuilder.buildEdge(currentBasicBlock, basicBlock);
		try { //TODO JNR remove try ... finally
			addDeclaration(basicBlock, node.getParameter(), DECL_INIT | WRITE);

			// for (CFGBasicBlock previousBasicBlock : liveBasicBlocks) {
			// CFGEdge.build(previousBasicBlock, basicBlock);
			// }
			// liveBasicBlocks.add(basicBlock);
			buildCFG(node.getBody(), Collections.EMPTY_LIST, null);
		} finally {
		}
	}

	public void buildCFG(EmptyStatement node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(DoStatement node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(ContinueStatement node) {
		throw new RuntimeException("Not implemented");
	}

	public List<CFGEdgeBuilder> buildCFG(ForStatement node,
			List<CFGEdgeBuilder> liveBlocks, CFGBasicBlock currentBasicBlockIgnored) {
		final CFGBasicBlock initBlock = newCFGBasicBlock(node.initializers());
		final CFGBasicBlock exprBlock = newCFGBasicBlock(node.getExpression(), null, true);
		final CFGBasicBlock updatersBlock = newCFGBasicBlock(node.updaters());

		buildWithTarget(liveBlocks, initBlock);
		CFGEdgeBuilder.buildEdge(initBlock, exprBlock);
		CFGEdgeBuilder.buildEdge(updatersBlock, exprBlock);

		try {
			for (Expression expression : (List<Expression>) node.initializers()) {
				if (expression instanceof VariableDeclarationExpression) {
					addDeclarations(initBlock,
							(VariableDeclarationExpression) expression);
				}
			}
			addVariableAccess(exprBlock, node.getExpression(), READ);
			addVariableAccesses(updatersBlock, node.updaters(), WRITE);

			CFGEdgeBuilder liveBlock = new CFGEdgeBuilder(node.getExpression(), true, exprBlock);

			final List<CFGEdgeBuilder> liveAfterLoop = buildCFG(node.getBody(), newList(liveBlock), null);
			buildWithTarget(liveAfterLoop, updatersBlock);

			final Set<CFGEdgeBuilder> toBuild = this.edgesToBuild.remove(node);
			buildWithTarget(toBuild, updatersBlock);
			return newList(new CFGEdgeBuilder(node.getExpression(), false, exprBlock));
		} finally {
			addEdgesToBuildForNextStatement(node,
					new CFGEdgeBuilder(node.getExpression(), false, exprBlock));
		}
	}

	public void buildCFG(FieldDeclaration node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(FieldAccess node) {
		throw new RuntimeException("Not implemented");
	}

	public List<CFGEdgeBuilder> buildCFG(ExpressionStatement node,
			List<CFGEdgeBuilder> liveBlocks, CFGBasicBlock currentBasicBlock) {
		final CFGBasicBlock basicBlock = newCFGBasicBlock(node, currentBasicBlock);
		addVariableAccess(basicBlock, node.getExpression(), READ);
		if (currentBasicBlock != null) {
			return newList(new CFGEdgeBuilder(basicBlock));
		} else {
			return liveBlocks;
		}
	}

	private CFGBasicBlock newCFGBasicBlock(ASTNode node, CFGBasicBlock currentBasicBlock) {
		return newCFGBasicBlock(node, currentBasicBlock, false);
	}

	private CFGBasicBlock newCFGBasicBlock(ASTNode node, CFGBasicBlock currentBasicBlock, boolean isDecision) {
		if (currentBasicBlock != null) {
			return currentBasicBlock;
		}
		final Pair<Integer, Integer> lineCol = getLineAndColumn(node);
		final CFGBasicBlock basicBlock = new CFGBasicBlock(node,
				getFileName(node), codeExcerpt(node), isDecision,
				lineCol.getFirst(), lineCol.getSecond());
		final Set<CFGEdgeBuilder> toBuild = this.edgesToBuild.remove(node);
		buildWithTarget(toBuild, basicBlock);
		return basicBlock;
	}

	private CFGBasicBlock newCFGBasicBlock(List<Expression> expressions) {
		if (isNotEmpty(expressions)) {
			final Expression firstExpr = expressions.get(0);
			final Pair<Integer, Integer> lineCol = getLineAndColumn(firstExpr
					.getStartPosition());
			return new CFGBasicBlock(expressions.get(0),
					getFileName(firstExpr), codeExcerpt(expressions), false,
					lineCol.getFirst(), lineCol.getSecond());
		}
		throw new RuntimeException(notImplementedFor(null));
	}

	private CFGBasicBlock newEntryBlock(MethodDeclaration node) {
		return CFGBasicBlock.buildEntryBlock(node, getFileName(node),
				codeExcerpt(node));
	}

	private CFGBasicBlock newExitBlock(MethodDeclaration node) {
		final Pair<Integer, Integer> lineCol = getLineAndColumn(node
				.getStartPosition() + node.getLength());
		return CFGBasicBlock.buildExitBlock(node, getFileName(node),
				codeExcerpt(node), lineCol.getFirst(), lineCol.getSecond());
	}

	private Pair<Integer, Integer> getLineAndColumn(ASTNode node) {
		return getLineAndColumn(node.getStartPosition());
	}

	private Pair<Integer, Integer> getLineAndColumn(final int position) {
		// file starts with line 1
		int lineNo = 1;
		int lastMatchPosition = 0;
		final Matcher matcher = Pattern.compile("\r\n|\r|\n").matcher(source);
		while (matcher.find()) {
			final MatchResult matchResult = matcher.toMatchResult();
			if (matchResult.end() >= position) {
				final String startOfLine = this.source.substring(
						lastMatchPosition, position);
				final int nbChars = countCharacters(startOfLine, tabSize);
				// + 1 because line starts with column 1
				return Pair.of(lineNo, nbChars + 1);
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

	private String notImplementedFor(ASTNode node) {
		return "Not implemented for " + (node != null? node.getClass().getSimpleName() : null);
	}

	private boolean isNotEmpty(final Collection<?> col) {
		return col != null && !col.isEmpty();
	}

}

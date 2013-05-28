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

import static org.autorefactor.cfg.VariableAccess.DECL_INIT;
import static org.autorefactor.cfg.VariableAccess.DECL_UNINIT;
import static org.autorefactor.cfg.VariableAccess.READ;
import static org.autorefactor.cfg.VariableAccess.WRITE;

import java.lang.reflect.Method;
import java.util.Collection;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.autorefactor.refactoring.ASTHelper;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.AbstractTypeDeclaration;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.ArrayAccess;
import org.eclipse.jdt.core.dom.ArrayCreation;
import org.eclipse.jdt.core.dom.ArrayInitializer;
import org.eclipse.jdt.core.dom.ArrayType;
import org.eclipse.jdt.core.dom.AssertStatement;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BooleanLiteral;
import org.eclipse.jdt.core.dom.BreakStatement;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.CatchClause;
import org.eclipse.jdt.core.dom.CharacterLiteral;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.ConstructorInvocation;
import org.eclipse.jdt.core.dom.ContinueStatement;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EmptyStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.Initializer;
import org.eclipse.jdt.core.dom.InstanceofExpression;
import org.eclipse.jdt.core.dom.LabeledStatement;
import org.eclipse.jdt.core.dom.MemberRef;
import org.eclipse.jdt.core.dom.MemberValuePair;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.MethodRef;
import org.eclipse.jdt.core.dom.MethodRefParameter;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.NullLiteral;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.ParameterizedType;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.PrimitiveType;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.QualifiedType;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SimpleType;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.jdt.core.dom.SuperConstructorInvocation;
import org.eclipse.jdt.core.dom.SuperFieldAccess;
import org.eclipse.jdt.core.dom.SuperMethodInvocation;
import org.eclipse.jdt.core.dom.SwitchCase;
import org.eclipse.jdt.core.dom.SwitchStatement;
import org.eclipse.jdt.core.dom.SynchronizedStatement;
import org.eclipse.jdt.core.dom.TagElement;
import org.eclipse.jdt.core.dom.TextElement;
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.jdt.core.dom.ThrowStatement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.TypeDeclarationStatement;
import org.eclipse.jdt.core.dom.TypeLiteral;
import org.eclipse.jdt.core.dom.TypeParameter;
import org.eclipse.jdt.core.dom.UnionType;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.jdt.core.dom.WhileStatement;
import org.eclipse.jdt.core.dom.WildcardType;

public class CFGBuilder {

	private String source;
	private int tabSize;
	private final Deque<CFGBasicBlock> currentBlockStack = new LinkedList<CFGBasicBlock>();
	/**
	 * Edges to be built before or after visiting the statement used as the key.
	 * When the statement is visited, code checks whether there are edges to
	 * build and builds them. After the statement was visited, code checks
	 * whether there are edges to build and decides whether to build them now
	 * (loops) or to forward them to the next statement for building them
	 * (decision statement).
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
			VariableDeclarationExpression vde = (VariableDeclarationExpression) node;
			addDeclarations(basicBlock, vde.fragments(), vde.getType());
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

	private void buildCFG(Statement node) {
		if (node == null) {
			return;
		}
		try {
			final Method m = getClass().getMethod("buildCFG", node.getClass());
			m.invoke(this, node);
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

	public void buildCFG(ReturnStatement node) {
		addVariableAccess(this.currentBlockStack.peek(), node.getExpression(),
				READ);
		final Set<CFGEdgeBuilder> toBuild = this.edgesToBuild.get(node);
		if (isNotEmpty(toBuild)) {
			final CFGBasicBlock basicBlock = newCFGBasicBlock(node);
			CFGEdgeBuilder.buildEdge(basicBlock, this.exitBlock);
		} else {
			CFGEdgeBuilder.buildEdge(this.currentBlockStack.peek(),
					this.exitBlock);
		}
	}

	public void buildCFG(Modifier node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(MethodInvocation node) {
		throw new RuntimeException("Not implemented");
	}

	public CFGBasicBlock buildCFG(MethodDeclaration node) {
		final CFGBasicBlock entryBlock = CFGBasicBlock.buildEntryBlock(node);
		final Pair<Integer, Integer> lineCol = getLineAndColumn(node
				.getStartPosition() + node.getLength());
		this.exitBlock = CFGBasicBlock.buildExitBlock(node, lineCol.getFirst(),
				lineCol.getSecond());

		addDeclarations(entryBlock, node.parameters());

		final CFGBasicBlock basicBlock = newCFGBasicBlock((ASTNode) node
				.getBody());
		CFGEdgeBuilder.buildEdge(entryBlock, basicBlock);
		this.currentBlockStack.push(basicBlock);
		try {
			buildCFG(node.getBody());
			if ("void".equals(node.getReturnType2().resolveBinding().getName())) {
				// TODO JNR handle methods with void return type
			}
			if (!this.edgesToBuild.isEmpty()) {
				throw new IllegalStateException(
						"At this point, there should not be any edges left to build");
			}
			// new CFGDotPrinter().toDot(entryBlock);
			// new CodePathCollector().getPaths(entryBlock);
			return entryBlock;
		} finally {
			this.currentBlockStack.pop();
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

	public void buildCFG(VariableDeclarationStatement node) {
		addDeclarations(this.currentBlockStack.peek(), node.fragments(),
				node.getType());
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

	public void buildCFG(Block node) {
		final ASTNode parent = node.getParent();
		final CFGBasicBlock basicBlock;
		final boolean needNewBlock = !(parent instanceof IfStatement)
				&& !(parent instanceof MethodDeclaration);
		if (needNewBlock) {
			basicBlock = newCFGBasicBlock(node);
			CFGEdgeBuilder.buildEdge(this.currentBlockStack.peek(), basicBlock);
			this.currentBlockStack.push(basicBlock);
		} else {
			basicBlock = this.currentBlockStack.peek();
		}
		try {
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
					ExpressionStatement es = (ExpressionStatement) stmt;
					addVariableAccess(currentBlockStack.peek(),
							es.getExpression(), READ);
					// } else if (stmt instanceof ForStatement) {
					// ForStatement fs = (ForStatement) stmt;
				} else if (stmt instanceof IfStatement) {
					buildCFG((IfStatement) stmt);
					// } else if (stmt instanceof LabeledStatement) {
					// LabeledStatement ls = (LabeledStatement) stmt;
				} else if (stmt instanceof ReturnStatement) {
					buildCFG((ReturnStatement) stmt);
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
					buildCFG((VariableDeclarationStatement) stmt);
					// } else if (stmt instanceof WhileStatement) {
					// WhileStatement ws = (WhileStatement) stmt;
					// buildCFG(ws);
				} else {
					throw new RuntimeException(notImplementedFor(stmt));
				}
			}
		} finally {
			if (needNewBlock) {
				this.currentBlockStack.pop();
			}
		}
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

	public void buildCFG(IfStatement node) {
		final Statement parent = (Statement) node.getParent();
		final CFGBasicBlock exprBlock;
		final boolean needNewBlock = !"elseStatement".equals(node
				.getLocationInParent().getId());
		if (needNewBlock) {
			exprBlock = newCFGBasicBlock(node);
			CFGEdgeBuilder.buildEdge(this.currentBlockStack.peek(), exprBlock);
			this.currentBlockStack.push(exprBlock);
		} else {
			exprBlock = this.currentBlockStack.peek();
			// let's remove the edge we thought we needed to build,
			// going from the else statement of the parent if to the
			// statement just after the if
			final Set<CFGEdgeBuilder> toBuild = this.edgesToBuild.get(parent);
			if (toBuild != null) {
				toBuild.remove(new CFGEdgeBuilder(exprBlock));
			}
		}
		try {
			addVariableAccess(exprBlock, node.getExpression(), READ);

			handleClause(node, exprBlock, node.getThenStatement(), true);
			handleClause(node, exprBlock, node.getElseStatement(), false);
		} finally {
			if (needNewBlock) {
				this.currentBlockStack.pop();
			}
			final Set<CFGEdgeBuilder> toBuild = this.edgesToBuild.remove(node);
			if (toBuild != null && !toBuild.isEmpty()) {
				final Statement nextStmt = ASTHelper.getNextStatement(node);
				for (CFGEdgeBuilder builder : toBuild) {
					addEdgeToBuild(nextStmt, builder);
				}
			}
		}
	}

	private void handleClause(IfStatement node, final CFGBasicBlock exprBlock,
			final Statement clauseStmt, boolean evaluationResult) {
		if (clauseStmt != null) {
			final CFGBasicBlock clauseBlock = newCFGBasicBlock(clauseStmt);
			CFGEdgeBuilder.buildEdge(node.getExpression(), evaluationResult,
					exprBlock, clauseBlock);
			this.currentBlockStack.push(clauseBlock);
			try {
				addEdgeToBuild(node, new CFGEdgeBuilder(clauseBlock));
				buildCFG(clauseStmt);
			} finally {
				this.currentBlockStack.pop();
			}
		}
	}

	private void addEdgeToBuild(final Statement node, CFGEdgeBuilder builder) {
		Set<CFGEdgeBuilder> builders = this.edgesToBuild.get(node);
		if (builders == null) {
			builders = new HashSet<CFGEdgeBuilder>();
			this.edgesToBuild.put(node, builders);
		}
		builders.add(builder);
	}

	public void buildCFG(MemberRef node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(LabeledStatement node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(EnhancedForStatement node) {
		CFGBasicBlock basicBlock = newCFGBasicBlock(node);
		CFGEdgeBuilder.buildEdge(this.currentBlockStack.peek(), basicBlock);
		this.currentBlockStack.push(basicBlock);
		try {
			addDeclaration(basicBlock, node.getParameter(), DECL_INIT | WRITE);

			// for (CFGBasicBlock previousBasicBlock : liveBasicBlocks) {
			// CFGEdge.build(previousBasicBlock, basicBlock);
			// }
			// liveBasicBlocks.add(basicBlock);
			buildCFG(node.getBody());
		} finally {
			this.currentBlockStack.pop();
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

	public void buildCFG(ForStatement node) {
		throw new RuntimeException("Not implemented");
		// CFGEdge.build(this.currentBasicBlock, basicBlock);
		// this.currentBasicBlock = basicBlock;
		//
		// for (Expression expression : (List<Expression>) node.initializers())
		// {
		// if (expression instanceof VariableDeclarationExpression) {
		// final VariableDeclarationExpression vde =
		// (VariableDeclarationExpression) expression;
		// addDeclarations(basicBlock, vde.fragments(), vde.getType());
		// }
		// }
		//
		// addReadWrite(basicBlock, node.getExpression());
		// edgesToBuild.add(Pair.of(basicBlock, node.getExpression()));
		// final Expression negatedExpr = ASTHelper.negate(this.ast,
		// ASTHelper.copySubtree(this.ast, node.getExpression()));
		// edgesToBuild.add(Pair.of(basicBlock, negatedExpr));
		// addReadWrites(basicBlock, node.updaters());
		//
		// for (CFGBasicBlock previousBasicBlock : liveBasicBlocks) {
		// CFGEdge.build(previousBasicBlock, basicBlock);
		// }
		// buildCFG(node.getBody());
	}

	public void buildCFG(FieldDeclaration node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(FieldAccess node) {
		throw new RuntimeException("Not implemented");
	}

	public void buildCFG(ExpressionStatement node) {
		addVariableAccess(this.currentBlockStack.peek(), node.getExpression(),
				READ);
	}

	private CFGBasicBlock newCFGBasicBlock(ASTNode node) {
		final Pair<Integer, Integer> lineCol = getLineAndColumn(node);
		final CFGBasicBlock basicBlock = new CFGBasicBlock(node,
				lineCol.getFirst(), lineCol.getSecond());
		final Set<CFGEdgeBuilder> toBuild = this.edgesToBuild.remove(node);
		if (isNotEmpty(toBuild)) {
			for (CFGEdgeBuilder builder : toBuild) {
				builder.withTarget(basicBlock).build();
			}
		}
		return basicBlock;
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
		if (node != null) {
			return "Not implemented for " + node.getClass().getSimpleName();
		}
		return "Not implemented for null";
	}

	private boolean isNotEmpty(final Collection<CFGEdgeBuilder> toBuild) {
		return toBuild != null && !toBuild.isEmpty();
	}

}

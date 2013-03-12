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
import java.util.LinkedList;
import java.util.List;

import org.autorefactor.refactoring.ASTHelper;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.AST;
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

	private final AST ast;
	private CFGBasicBlock currentBasicBlock;
	/** TODO JNR need to pass down a stack of live basic blocks? */
	private final List<CFGBasicBlock> liveBasicBlocks = new LinkedList<CFGBasicBlock>();
	private final List<Pair<CFGBasicBlock, Expression>> edgesToBuild = new LinkedList<Pair<CFGBasicBlock, Expression>>();
	private final CFGBasicBlock exitBlock = new CFGBasicBlock();

	public CFGBuilder(AST ast) {
		this.ast = ast;
	}

	private void addReadWrite(CFGBasicBlock basicBlock, Expression node) {
		addReadWrite(basicBlock, node, VariableAccess.READ
				| VariableAccess.WRITE);
	}

	private void addReadWrites(CFGBasicBlock basicBlock, List<Expression> nodes) {
		addReadWrites(basicBlock, nodes, VariableAccess.READ
				| VariableAccess.WRITE);
	}

	@SuppressWarnings("unchecked")
	private void addReadWrite(CFGBasicBlock basicBlock, Expression node,
			int flags) {
		if (node instanceof ArrayAccess) {
			ArrayAccess aa = (ArrayAccess) node;
			addReadWrite(basicBlock, aa.getArray(), flags);
			addReadWrite(basicBlock, aa.getIndex(), flags);
			return;
		} else if (node instanceof ArrayCreation) {
			ArrayCreation ac = (ArrayCreation) node;
			addReadWrite(basicBlock, ac.getInitializer(), flags);
			addReadWrites(basicBlock, ac.dimensions(), flags);
			return;
		} else if (node instanceof ArrayInitializer) {
			ArrayInitializer ai = (ArrayInitializer) node;
			addReadWrites(basicBlock, ai.expressions(), flags);
			return;
		} else if (node instanceof Assignment) {
			Assignment a = (Assignment) node;
			addReadWrite(basicBlock, a.getLeftHandSide(), VariableAccess.WRITE);
			addReadWrite(basicBlock, a.getRightHandSide(), VariableAccess.READ);
			return;
		} else if (node instanceof BooleanLiteral
				|| node instanceof CharacterLiteral
				|| node instanceof NullLiteral || node instanceof NumberLiteral
				|| node instanceof StringLiteral || node instanceof TypeLiteral) {
			// nothing to do
			return;
		} else if (node instanceof CastExpression) {
			CastExpression ce = (CastExpression) node;
			addReadWrite(basicBlock, ce.getExpression(), flags);
			return;
		} else if (node instanceof ClassInstanceCreation) {
			ClassInstanceCreation cic = (ClassInstanceCreation) node;
			addReadWrite(basicBlock, cic.getExpression(), flags);
			addReadWrites(basicBlock, cic.arguments(), flags);
			return;
		} else if (node instanceof ConditionalExpression) {
			ConditionalExpression ce = (ConditionalExpression) node;
			addReadWrite(basicBlock, ce.getExpression(), flags);
			addReadWrite(basicBlock, ce.getThenExpression(), flags);
			addReadWrite(basicBlock, ce.getElseExpression(), flags);
			return;
		} else if (node instanceof FieldAccess) {
			FieldAccess fa = (FieldAccess) node;
			basicBlock.addVariableAccess(new VariableAccess(fa, flags));
			return;
		} else if (node instanceof InfixExpression) {
			InfixExpression ie = (InfixExpression) node;
			addReadWrite(basicBlock, ie.getLeftOperand(), flags);
			addReadWrite(basicBlock, ie.getRightOperand(), flags);
			return;
		} else if (node instanceof InstanceofExpression) {
			InstanceofExpression ie = (InstanceofExpression) node;
			addReadWrite(basicBlock, ie.getLeftOperand(), flags);
			return;
		} else if (node instanceof MethodInvocation) {
			MethodInvocation mi = (MethodInvocation) node;
			addReadWrite(basicBlock, mi.getExpression(), flags);
			addReadWrites(basicBlock, mi.arguments(), flags);
			return;
		} else if (node instanceof SimpleName) {
			SimpleName sn = (SimpleName) node;
			basicBlock.addVariableAccess(new VariableAccess(sn, flags));
			return;
		} else if (node instanceof QualifiedName) {
			QualifiedName qn = (QualifiedName) node;
			basicBlock.addVariableAccess(new VariableAccess(qn, flags));
			return;
		} else if (node instanceof ParenthesizedExpression) {
			ParenthesizedExpression pe = (ParenthesizedExpression) node;
			addReadWrite(basicBlock, pe.getExpression(), flags);
			return;
		} else if (node instanceof PostfixExpression) {
			PostfixExpression pe = (PostfixExpression) node;
			addReadWrite(basicBlock, pe.getOperand(), flags);
			return;
		} else if (node instanceof PrefixExpression) {
			PrefixExpression pe = (PrefixExpression) node;
			addReadWrite(basicBlock, pe.getOperand(), flags);
			return;
		} else if (node instanceof SuperFieldAccess) {
			SuperFieldAccess sfa = (SuperFieldAccess) node;
		} else if (node instanceof SuperMethodInvocation) {
			SuperMethodInvocation smi = (SuperMethodInvocation) node;
		} else if (node instanceof ThisExpression) {
			ThisExpression te = (ThisExpression) node;
		} else if (node instanceof VariableDeclarationExpression) {
			VariableDeclarationExpression vde = (VariableDeclarationExpression) node;
			addDeclarations(basicBlock, vde.fragments(), vde.getType());
			return;
		}
		throw new RuntimeException("Not implemented for " + node.getClass());
	}

	private void addReadWrites(CFGBasicBlock basicBlock,
			List<Expression> expressions, int flags) {
		for (Expression exor : expressions) {
			addReadWrite(basicBlock, exor, flags);
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
		final int accessType = vdf.getInitializer() == null ? VariableAccess.DECL_INIT
				: VariableAccess.DECL_INIT | VariableAccess.WRITE;
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
		addDeclaration(basicBlock, varDecl, VariableAccess.DECL_INIT);
	}

	private void addDeclaration(CFGBasicBlock basicBlock,
			SingleVariableDeclaration varDecl, int flags) {
		basicBlock.addVariableAccess(new VariableAccess(varDecl, varDecl
				.getName(), varDecl.getType(), flags));
	}

	private void add(List<CFGBasicBlock> liveBasicBlocks,
			CFGBasicBlock lastBuiltBasicBlock) {
		if (lastBuiltBasicBlock != null) {
			liveBasicBlocks.add(lastBuiltBasicBlock);
		}
	}

	private CFGBasicBlock buildCFG(Statement node) {
		if (node == null) {
			return null;
		}
		try {
			final Method buildCfgMethod = getClass().getMethod("buildCFG",
					node.getClass());
			return (CFGBasicBlock) buildCfgMethod.invoke(this, node);
		} catch (Exception e) {
			throw new RuntimeException("Unhandled exception", e);
		}
	}

	public CFGBasicBlock buildCFG(QualifiedName node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(PrimitiveType node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(QualifiedType node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(PrefixExpression node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(PostfixExpression node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(ParenthesizedExpression node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(SingleVariableDeclaration node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(SimpleType node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(SimpleName node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(ReturnStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(Modifier node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(MethodInvocation node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(MethodDeclaration node) {
		final CFGBasicBlock basicBlock = new CFGBasicBlock(node);
		this.currentBasicBlock = basicBlock;

		addDeclarations(basicBlock, node.parameters());

		// ConstructorDeclaration:
		// [ Javadoc ] { ExtendedModifier }
		// [ < TypeParameter { , TypeParameter } > ]
		// Identifier (
		// [ FormalParameter
		// { , FormalParameter } ] )
		// [throws TypeName { , TypeName } ] Block
		// TODO JNR
		buildCFG(node.getBody());
		if ("void".equals(node.getReturnType2().resolveBinding().getName())) {
			// TODO JNR fix this when the last statement is a return statement
			CFGEdge.build(this.currentBasicBlock, this.exitBlock);
		}
		basicBlock.toString();
		return basicBlock;
	}

	public CFGBasicBlock buildCFG(MethodRefParameter node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(MethodRef node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(MemberValuePair node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(ParameterizedType node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(NumberLiteral node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(NullLiteral node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(UnionType node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(TypeParameter node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(TypeLiteral node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(TypeDeclarationStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
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

	public CFGBasicBlock buildCFG(TryStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(WildcardType node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(WhileStatement node) {
		return buildCFG(node.getBody());
	}

	public CFGBasicBlock buildCFG(VariableDeclarationFragment node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(VariableDeclarationStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(VariableDeclarationExpression node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(SwitchStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(SwitchCase node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(SuperMethodInvocation node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(SuperFieldAccess node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(SuperConstructorInvocation node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(StringLiteral node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(ThrowStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(ThisExpression node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(TextElement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(TagElement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(SynchronizedStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(CatchClause node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(CastExpression node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(BreakStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(BooleanLiteral node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(ConstructorInvocation node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(ConditionalExpression node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public List<CFGBasicBlock> buildCFG(CompilationUnit node) {
		List<CFGBasicBlock> results = new LinkedList<CFGBasicBlock>();
		for (AbstractTypeDeclaration decl : (List<AbstractTypeDeclaration>) node.types()) {
			if (decl instanceof TypeDeclaration) {
				results.add(buildCFG((TypeDeclaration) decl));
				// TODO JNR?
				// } else if (decl instanceof EnumDeclaration) {
				// buildCFG((EnumDeclaration) decl);
			}
		}

		boolean b = true;
		if (b) {
			throw new RuntimeException("Not implemented");
		}
		return results;
	}

	public CFGBasicBlock buildCFG(ClassInstanceCreation node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(CharacterLiteral node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(ArrayCreation node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(ArrayAccess node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(AnonymousClassDeclaration node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(Block node) {
		final CFGBasicBlock basicBlock;
		final ASTNode parent = node.getParent();
		if (parent instanceof ForStatement
				|| parent instanceof EnhancedForStatement
				|| parent instanceof WhileStatement) {
			basicBlock = this.currentBasicBlock;
		} else {
			basicBlock = new CFGBasicBlock(node);
			CFGEdge.build(this.currentBasicBlock, basicBlock);
			this.currentBasicBlock = basicBlock;
		}

		for (Statement stmt : (List<Statement>) node.statements()) {
			if (stmt instanceof Block) {
				Block b = (Block) stmt;
				// continuity of current block
			} else if (stmt instanceof BreakStatement) {
				BreakStatement bs = (BreakStatement) stmt;
				// adds an edge
			} else if (stmt instanceof ConstructorInvocation) {
				ConstructorInvocation ci = (ConstructorInvocation) stmt;
			} else if (stmt instanceof ContinueStatement) {
				ContinueStatement cs = (ContinueStatement) stmt;
				// adds an edge
			} else if (stmt instanceof DoStatement) {
				DoStatement ws = (DoStatement) stmt;
			} else if (stmt instanceof EmptyStatement) {
				EmptyStatement es = (EmptyStatement) stmt;
			} else if (stmt instanceof EnhancedForStatement) {
				buildCFG((EnhancedForStatement) stmt);
				continue;
			} else if (stmt instanceof ExpressionStatement) {
				ExpressionStatement es = (ExpressionStatement) stmt;
			} else if (stmt instanceof ForStatement) {
				add(liveBasicBlocks, buildCFG((ForStatement) stmt));
				continue;
			} else if (stmt instanceof IfStatement) {
				buildCFG((IfStatement) stmt);
				continue;
			} else if (stmt instanceof LabeledStatement) {
				LabeledStatement ls = (LabeledStatement) stmt;
			} else if (stmt instanceof ReturnStatement) {
				CFGEdge.build(basicBlock, exitBlock);
				for (CFGBasicBlock block : liveBasicBlocks) {
					CFGEdge.build(block, exitBlock);
				}
				for (Pair<CFGBasicBlock, Expression> pair : edgesToBuild) {
					CFGEdge.build(pair.getSecond(), pair.getFirst(), exitBlock);
				}
				continue;
			} else if (stmt instanceof SwitchCase) {
				SwitchCase sc = (SwitchCase) stmt;
			} else if (stmt instanceof SwitchStatement) {
				SwitchStatement ss = (SwitchStatement) stmt;
			} else if (stmt instanceof SynchronizedStatement) {
				SynchronizedStatement ss = (SynchronizedStatement) stmt;
			} else if (stmt instanceof ThrowStatement
					|| stmt instanceof AssertStatement) {
				// AssertStatement as = (AssertStatement) stmt;
				// ThrowStatement ts = (ThrowStatement) stmt;
				// adds an edge
			} else if (stmt instanceof TryStatement) {
				TryStatement ts = (TryStatement) stmt;
			} else if (stmt instanceof TypeDeclarationStatement) {
				TypeDeclarationStatement tds = (TypeDeclarationStatement) stmt;
			} else if (stmt instanceof VariableDeclarationStatement) {
				VariableDeclarationStatement vds = (VariableDeclarationStatement) stmt;
				addDeclarations(basicBlock, vds.fragments(), vds.getType());
				continue;
			} else if (stmt instanceof SuperConstructorInvocation) {
				SuperConstructorInvocation sci = (SuperConstructorInvocation) stmt;
			} else if (stmt instanceof WhileStatement) {
				WhileStatement ws = (WhileStatement) stmt;
				buildCFG(ws);
				continue;
			}
			throw new RuntimeException("Not implemented for "
					+ stmt.getClass().getSimpleName());
		}
		return basicBlock;
	}

	public CFGBasicBlock buildCFG(Assignment node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(AssertStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(ArrayType node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(ArrayInitializer node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(Initializer node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(InstanceofExpression node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(InfixExpression node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public void buildCFG(IfStatement node) {
		final CFGBasicBlock thenBlock = buildCFG(node.getThenStatement());
		if (thenBlock != null) {
			for (CFGBasicBlock previousBasicBlock : liveBasicBlocks) {
				CFGEdge.build(node.getExpression(), previousBasicBlock,
						thenBlock);
			}
		}
		final CFGBasicBlock elseBlock = buildCFG(node.getElseStatement());
		if (elseBlock != null) {
			for (CFGBasicBlock previousBasicBlock : liveBasicBlocks) {
				final Expression negatedExpr = ASTHelper.negate(this.ast,
						node.getExpression());
				CFGEdge.build(negatedExpr, previousBasicBlock, elseBlock);
			}
		}

		if (thenBlock != null) {
			if (elseBlock != null) {
				liveBasicBlocks.clear();
				liveBasicBlocks.add(elseBlock);
			}
			liveBasicBlocks.add(thenBlock);
		}
	}

	public CFGBasicBlock buildCFG(MemberRef node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(LabeledStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public void buildCFG(EnhancedForStatement node) {
		CFGBasicBlock basicBlock = new CFGBasicBlock(node);
		CFGEdge.build(this.currentBasicBlock, basicBlock);
		this.currentBasicBlock = basicBlock;

		addDeclaration(basicBlock, node.getParameter(),
				VariableAccess.DECL_INIT | VariableAccess.WRITE);

		// for (CFGBasicBlock previousBasicBlock : liveBasicBlocks) {
		// CFGEdge.build(previousBasicBlock, basicBlock);
		// }
		// liveBasicBlocks.add(basicBlock);
		buildCFG(node.getBody());
	}

	public CFGBasicBlock buildCFG(EmptyStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(DoStatement node) {
		return buildCFG(node.getBody());
	}

	public CFGBasicBlock buildCFG(ContinueStatement node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(ForStatement node) {
		final CFGBasicBlock basicBlock = new CFGBasicBlock(node);
		CFGEdge.build(this.currentBasicBlock, basicBlock);
		this.currentBasicBlock = basicBlock;

		for (Expression expression : (List<Expression>) node.initializers()) {
			if (expression instanceof VariableDeclarationExpression) {
				final VariableDeclarationExpression vde = (VariableDeclarationExpression) expression;
				addDeclarations(basicBlock, vde.fragments(),
						vde.getType());
			}
		}

		addReadWrite(basicBlock, node.getExpression());
		edgesToBuild.add(Pair.of(basicBlock, node.getExpression()));
		final Expression negatedExpr = ASTHelper.negate(this.ast,
				ASTHelper.copySubtree(this.ast, node.getExpression()));
		edgesToBuild.add(Pair.of(basicBlock, negatedExpr));
		addReadWrites(basicBlock, node.updaters());

		for (CFGBasicBlock previousBasicBlock : liveBasicBlocks) {
			CFGEdge.build(previousBasicBlock, basicBlock);
		}
		buildCFG(node.getBody());
		return basicBlock;
	}

	public CFGBasicBlock buildCFG(FieldDeclaration node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(FieldAccess node) {
		if (true) {
			throw new RuntimeException("Not implemented");
		}
		return null;
	}

	public CFGBasicBlock buildCFG(ExpressionStatement node) {
		addReadWrite(this.currentBasicBlock, node.getExpression());
		return null;
	}

}

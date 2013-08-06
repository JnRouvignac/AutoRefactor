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
package org.autorefactor.refactoring;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTMatcher;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.AssertStatement;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BooleanLiteral;
import org.eclipse.jdt.core.dom.BreakStatement;
import org.eclipse.jdt.core.dom.ChildListPropertyDescriptor;
import org.eclipse.jdt.core.dom.ChildPropertyDescriptor;
import org.eclipse.jdt.core.dom.ConstructorInvocation;
import org.eclipse.jdt.core.dom.ContinueStatement;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EmptyStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InstanceofExpression;
import org.eclipse.jdt.core.dom.LabeledStatement;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression.Operator;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.StructuralPropertyDescriptor;
import org.eclipse.jdt.core.dom.SuperConstructorInvocation;
import org.eclipse.jdt.core.dom.SwitchCase;
import org.eclipse.jdt.core.dom.SwitchStatement;
import org.eclipse.jdt.core.dom.SynchronizedStatement;
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.jdt.core.dom.ThrowStatement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.TypeDeclarationStatement;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.jdt.core.dom.WhileStatement;

/**
 * Helper class for manipulating, converting, navigating and checking {@link ASTNode}s.
 */
public class ASTHelper {

	public static final boolean DO_NOT_VISIT_SUBTREE = false;
	public static final boolean VISIT_SUBTREE = true;

	private ASTHelper() {
		super();
	}

	// AST nodes manipulation

	public static <T> T copySubtree(AST ast, T node) {
		return (T) ASTNode.copySubtree(ast, (ASTNode) node);
	}

	public static Expression negate(AST ast, Expression condition, boolean doCopy) {
		if (condition instanceof PrefixExpression) {
			final PrefixExpression pe = (PrefixExpression) condition;
			if (Operator.NOT.equals(pe.getOperator())) {
				return possiblyCopy(ast, removeParentheses(pe.getOperand()), doCopy);
			}
		}

		final PrefixExpression pe = ast.newPrefixExpression();
		pe.setOperator(Operator.NOT);
		pe.setOperand(parenthesize(ast, possiblyCopy(ast, condition, doCopy)));
		return pe;
	}

	private static Expression removeParentheses(Expression expr) {
		if (expr instanceof ParenthesizedExpression) {
			return ((ParenthesizedExpression) expr).getExpression();
		}
		return expr;
	}

	private static <T extends ASTNode> T possiblyCopy(AST ast, T node, boolean doCopy) {
		if (doCopy) {
			return copySubtree(ast, node);
		}
		return node;
	}

	private static Expression parenthesize(AST ast, Expression condition) {
		if (condition instanceof InfixExpression
				|| condition instanceof InstanceofExpression) {
			final ParenthesizedExpression newPe = ast
					.newParenthesizedExpression();
			newPe.setExpression(condition);
			return newPe;
		}
		return condition;
	}

	public static void replaceInParent(ASTNode node, ASTNode replacement) {
		if (node.getParent() == null) {
			throw new IllegalArgumentException();
		}
		final StructuralPropertyDescriptor locationInParent = node.getLocationInParent();
		if (locationInParent instanceof ChildPropertyDescriptor) {
			final ChildPropertyDescriptor cpd = (ChildPropertyDescriptor) locationInParent;
			node.getParent().setStructuralProperty(cpd, replacement);
		} else if (locationInParent instanceof ChildListPropertyDescriptor) {
			final ChildListPropertyDescriptor clpd = (ChildListPropertyDescriptor) locationInParent;
			final List<ASTNode> property = (List<ASTNode>) node.getParent()
					.getStructuralProperty(clpd);
			property.set(property.indexOf(node), replacement);
		} else {
			throw new RuntimeException("Not implemented for "
					+ (locationInParent != null ? locationInParent.getClass()
							: "null"));
		}
	}

	// AST nodes conversions

	public static List<Statement> asList(Statement node) {
		if (node == null) {
			return Collections.emptyList();
		} else if (node instanceof Block) {
			return ((Block) node).statements();
		}
		return Arrays.asList(node);
	}

	public static <T extends Statement> T as(Statement node, Class<T> stmtClazz) {
		if (node != null) {
			final List<Statement> stmts = asList(node);
			if (stmts.size() == 1
					&& stmtClazz.isAssignableFrom(stmts.get(0).getClass())) {
				return (T) stmts.get(0);
			}
		}
		return null;
	}

	public static <T extends Expression> T as(Expression node,
			Class<T> exprClazz) {
		if (node != null) {
			if (exprClazz.isAssignableFrom(node.getClass())) {
				return (T) node;
			} else if (node instanceof ParenthesizedExpression) {
				return as(((ParenthesizedExpression) node).getExpression(),
						exprClazz);
			}
		}
		return null;
	}

	public static Boolean getBooleanLiteral(Expression node) {
		final BooleanLiteral bl = ASTHelper.as(node, BooleanLiteral.class);
		if (bl != null) {
			return bl.booleanValue();
		}
		final QualifiedName qn = ASTHelper.as(node, QualifiedName.class);
		if (ASTHelper.hasType(qn, "java.lang.Boolean")) {
			return getBooleanObjectAsLiteral(qn);
		}
		return null;
	}

	public static boolean getBooleanObjectAsLiteral(final QualifiedName node) {
		final String fqn = node.getFullyQualifiedName();
		if ("Boolean.TRUE".equals(fqn)) {
			return true;
		} else if ("Boolean.FALSE".equals(fqn)) {
			return false;
		}
		throw new IllegalStateException(
				"Not implemented for fully qualified name \"" + fqn + "\"");
	}

	// AST navigation

	public static <T extends ASTNode> T getAncestor(ASTNode node,
			Class<T> ancestorClazz) {
		if (node == null || node.getParent() == null) {
			throw new IllegalStateException("Could not find any ancestor for "
					+ ancestorClazz + "and node " + node);
		}
		final ASTNode parent = node.getParent();
		if (ancestorClazz.isAssignableFrom(parent.getClass())) {
			return (T) parent;
		}
		return getAncestor(parent, ancestorClazz);
	}

	public static Statement getPreviousSibling(Statement node) {
		return getSibling(node, true);
	}

	/**
	 * @return the next statement in the same block if it exists, null otherwise
	 */
	public static Statement getNextSibling(Statement node) {
		return getSibling(node, false);
	}

	/**
	 * @return the next statement in the source file if it exists, null
	 *         otherwise
	 */
	public static Statement getNextStatement(Statement node) {
		final Statement nextSibling = getNextSibling(node);
		if (nextSibling != null) {
			return nextSibling;
		}
		final ASTNode parent = node.getParent();
		if (parent instanceof Statement) {
			return getNextStatement((Statement) parent);
		}
		return null;
	}

	private static Statement getSibling(Statement node, boolean isPrevious) {
		if (node.getParent() instanceof Block) {
			final List<Statement> stmts = asList((Statement) node.getParent());
			final int indexOfNode = stmts.indexOf(node);
			final int siblingIndex = isPrevious ? indexOfNode - 1
					: indexOfNode + 1;
			if (0 <= siblingIndex && siblingIndex < stmts.size()) {
				return stmts.get(siblingIndex);
			}
		}
		return null;
	}

	// AST checks

	public static boolean hasType(Expression expr, String... qualifiedTypeNames) {
		if (expr == null) {
			return false;
		}
		return hasType(expr.resolveTypeBinding(), qualifiedTypeNames);
	}

	public static boolean hasType(final ITypeBinding typeBinding,
			String... qualifiedTypeNames) {
		if (typeBinding != null) {
			for (String qualifiedTypeName : qualifiedTypeNames) {
				if (qualifiedTypeName.equals(typeBinding.getQualifiedName())) {
					return true;
				}
			}
		}
		return false;
	}

	public static boolean instanceOf(ITypeBinding typeBinding,
			String qualifiedTypeName) {
		if (typeBinding == null) {
			return false;
		}
		if (qualifiedTypeName.equals(typeBinding.getQualifiedName())) {
			return true;
		}
		final Set<String> visitedClasses = new HashSet<String>();
		visitedClasses.add(typeBinding.getErasure().getQualifiedName());
		return instanceOf(typeBinding, qualifiedTypeName, visitedClasses);
	}

	private static boolean instanceOf(ITypeBinding typeBinding,
			String qualifiedTypeName, Set<String> visitedInterfaces) {
		final ITypeBinding superclass = typeBinding.getSuperclass();
		if (superclass != null) {
			final String superClassQualifiedName = superclass.getErasure()
					.getQualifiedName();
			if (qualifiedTypeName.equals(superClassQualifiedName)) {
				return true;
			}
			visitedInterfaces.add(superClassQualifiedName);
			if (instanceOf(superclass, qualifiedTypeName, visitedInterfaces)) {
				return true;
			}
		}
		for (ITypeBinding itfBinding : typeBinding.getInterfaces()) {
			final String itfQualifiedName = itfBinding.getErasure()
					.getQualifiedName();
			if (qualifiedTypeName.equals(itfQualifiedName)) {
				return true;
			}
			visitedInterfaces.add(itfQualifiedName);
			if (instanceOf(itfBinding, qualifiedTypeName, visitedInterfaces)) {
				return true;
			}
		}
		return false;
	}

	public static boolean isLoop(ASTNode node) {
		return node instanceof DoStatement
				|| node instanceof EnhancedForStatement
				|| node instanceof ForStatement
				|| node instanceof WhileStatement;
	}

	public static boolean isBreakable(ASTNode node) {
		return isLoop(node) || node instanceof SwitchStatement;
	}

	/**
	 * Infers what type the parent node expects to be returned by the passed in
	 * Expression.
	 * 
	 * @param node
	 *            the expression for which to look at the type expected by the
	 *            context
	 * @return the type expected by the context of the current node
	 */
	public static ITypeBinding resolveTypeBindingForcedFromContext(
			Expression node) {
		final ASTNode parent = node.getParent();
		if (parent instanceof InfixExpression) {
			final InfixExpression ie = (InfixExpression) parent;
			return ie.resolveTypeBinding();
		}
		return null;
	}

	public static boolean thisExpressionRefersToCurrentType(Name name,
			ASTNode node) {
		final TypeDeclaration ancestor = ASTHelper.getAncestor(node,
				TypeDeclaration.class);
		if (name == null) {
			return true;
		} else if (name instanceof SimpleName) {
			return isEqual((SimpleName) name, ancestor.getName());
		} else if (name instanceof QualifiedName) {
			final QualifiedName qn = (QualifiedName) name;
			return isEqual(qn.getName(), ancestor.getName())
					&& thisExpressionRefersToCurrentType(qn.getQualifier(),
							ancestor);
		}
		throw new RuntimeException("Not implemented for "
				+ (name != null ? name.getClass() : "null"));
	}

	public static boolean isEqual(Name name1, Name name2) {
		if (name1 instanceof SimpleName && name2 instanceof SimpleName) {
			return isEqual((SimpleName) name1, (SimpleName) name2);
		} else if (name1 instanceof QualifiedName
				&& name2 instanceof QualifiedName) {
			return isEqual((QualifiedName) name1, (QualifiedName) name2);
		}
		return false;
	}

	public static boolean isEqual(SimpleName name1, SimpleName name2) {
		return name1.getIdentifier().equals(name2.getIdentifier());
	}

	public static boolean isEqual(QualifiedName name1, QualifiedName name2) {
		if (isEqual(name1.getName(), name2.getName())) {
			return isEqual(name1.getQualifier(), name2.getQualifier());
		}
		return false;
	}

	public static boolean match(ASTMatcher matcher, ASTNode node1, ASTNode node2) {
		if (node1 != null && node2 != null
				&& node1.getClass().equals(node2.getClass())) {
			if (node1 instanceof Name) {
				return match(matcher, (Name) node1, (Name) node2);
			} else if (node1 instanceof Statement) {
				return match(matcher, (Statement) node1, (Statement) node2);
			}
			throw new RuntimeException("Not implemented for "
					+ node1.getClass());
		}
		return false;
	}

	public static boolean match(ASTMatcher matcher, Name name1, Name name2) {
		if (name1 != null && name2 != null
				&& name1.getClass().equals(name2.getClass())) {
			// TODO JNR can we match here "this.ast" and the unqualified "ast"
			// for example?
			// can we match here "MyClass.CONSTANT" and the unqualified
			// "CONSTANT" for example?
			if (name1 instanceof SimpleName) {
				return matcher.match((SimpleName) name1, (SimpleName) name2);
			} else if (name1 instanceof QualifiedName) {
				return matcher.match((QualifiedName) name1,
						(QualifiedName) name2);
			}
			throw new IllegalStateException("Not implemented for "
					+ name1.getClass());
		}
		return false;
	}

	public static boolean match(ASTMatcher matcher, Statement stmt1,
			Statement stmt2) {
		if (stmt1 != null && stmt2 != null
				&& stmt1.getClass().equals(stmt2.getClass())) {
			if (stmt1 instanceof AssertStatement) {
				return matcher.match((AssertStatement) stmt1,
						(AssertStatement) stmt2);
			} else if (stmt1 instanceof BreakStatement) {
				return matcher.match((BreakStatement) stmt1,
						(BreakStatement) stmt2);
			} else if (stmt1 instanceof Block) {
				return matcher.match((Block) stmt1, (Block) stmt2);
			} else if (stmt1 instanceof ConstructorInvocation) {
				return matcher.match((ConstructorInvocation) stmt1,
						(ConstructorInvocation) stmt2);
			} else if (stmt1 instanceof ContinueStatement) {
				return matcher.match((ContinueStatement) stmt1,
						(ContinueStatement) stmt2);
			} else if (stmt1 instanceof DoStatement) {
				return matcher.match((DoStatement) stmt1, (DoStatement) stmt2);
			} else if (stmt1 instanceof EmptyStatement) {
				return matcher.match((EmptyStatement) stmt1,
						(EmptyStatement) stmt2);
			} else if (stmt1 instanceof EnhancedForStatement) {
				return matcher.match((EnhancedForStatement) stmt1,
						(EnhancedForStatement) stmt2);
			} else if (stmt1 instanceof ExpressionStatement) {
				return matcher.match((ExpressionStatement) stmt1,
						(ExpressionStatement) stmt2);
			} else if (stmt1 instanceof ForStatement) {
				return matcher
						.match((ForStatement) stmt1, (ForStatement) stmt2);
			} else if (stmt1 instanceof IfStatement) {
				return matcher.match((IfStatement) stmt1, (IfStatement) stmt2);
			} else if (stmt1 instanceof LabeledStatement) {
				return matcher.match((LabeledStatement) stmt1,
						(LabeledStatement) stmt2);
			} else if (stmt1 instanceof ReturnStatement) {
				return matcher.match((ReturnStatement) stmt1,
						(ReturnStatement) stmt2);
			} else if (stmt1 instanceof SuperConstructorInvocation) {
				return matcher.match((SuperConstructorInvocation) stmt1,
						(SuperConstructorInvocation) stmt2);
			} else if (stmt1 instanceof SwitchCase) {
				return matcher.match((SwitchCase) stmt1, (SwitchCase) stmt2);
			} else if (stmt1 instanceof SwitchStatement) {
				return matcher.match((SwitchStatement) stmt1,
						(SwitchStatement) stmt2);
			} else if (stmt1 instanceof SynchronizedStatement) {
				return matcher.match((SynchronizedStatement) stmt1,
						(SynchronizedStatement) stmt2);
			} else if (stmt1 instanceof ThrowStatement) {
				return matcher.match((ThrowStatement) stmt1,
						(ThrowStatement) stmt2);
			} else if (stmt1 instanceof TryStatement) {
				return matcher
						.match((TryStatement) stmt1, (TryStatement) stmt2);
			} else if (stmt1 instanceof TypeDeclarationStatement) {
				return matcher.match((TypeDeclarationStatement) stmt1,
						(TypeDeclarationStatement) stmt2);
			} else if (stmt1 instanceof VariableDeclarationStatement) {
				return matcher.match((VariableDeclarationStatement) stmt1,
						(VariableDeclarationStatement) stmt2);
			} else if (stmt1 instanceof WhileStatement) {
				return matcher.match((WhileStatement) stmt1,
						(WhileStatement) stmt2);
			}
			throw new IllegalStateException("Not implemented for "
					+ stmt1.getClass());
		}
		return false;
	}

	public static boolean isSameVariable(SimpleName name1, SimpleName name2) {
		final IBinding bnd1 = name1.resolveBinding();
		final IBinding bnd2 = name2.resolveBinding();
		if (bnd1 == null || bnd2 == null) {
			return false;
		}
		return bnd1.isEqualTo(bnd2);
	}

	public static boolean isSameVariable(SimpleName name1, QualifiedName name2) {
		return false;
	}

	public static boolean isSameVariable(SimpleName name1, FieldAccess name2) {
		if (!(name2.getExpression() instanceof ThisExpression)) {
			// TODO JNR parenthesized expr??
			return false;
		}
		IBinding cb = name1.resolveBinding();
		IBinding sb = name2.resolveFieldBinding();
		if (cb == null || sb == null) {
			return false;
		}
		return cb.isEqualTo(sb);
	}

	public static boolean isSameVariable(QualifiedName name1,
			QualifiedName name2) {
		IBinding cb = name1.resolveBinding();
		IBinding sb = name2.resolveBinding();
		if (cb == null || sb == null) {
			return false;
		}
		if (cb.isEqualTo(sb)) {
			return isSameVariable(name1.getQualifier(), name2.getQualifier());
		}
		return false;
	}

	public static boolean isSameVariable(QualifiedName name1, FieldAccess name2) {
		IBinding sb = name1.resolveBinding();
		IBinding cb = name2.resolveFieldBinding();
		if (cb == null || sb == null)
			return false;
		if (cb.isEqualTo(sb)) {
			return isSameVariable(name2.getExpression(), name1.getQualifier());
		}
		return false;
	}

	public static boolean isSameVariable(FieldAccess name1, FieldAccess name2) {
		IBinding cb = name1.resolveFieldBinding();
		IBinding sb = name2.resolveFieldBinding();
		if (cb == null || sb == null)
			return false;
		if (cb.isEqualTo(sb)) {
			return isSameVariable(name1.getExpression(), name2.getExpression());
		}
		return false;
	}

	public static boolean isSameVariable(Expression name1, Expression name2) {
		if (name1 instanceof ThisExpression) {
			// TODO JNR
			// ThisExpression te = (ThisExpression) name1;
			// te.getQualifier();
			return name2 instanceof ThisExpression;
		} else if (name1 instanceof SimpleName) {
			final SimpleName sn = (SimpleName) name1;
			if (name2 instanceof SimpleName) {
				return isSameVariable(sn, (SimpleName) name2);
			} else if (name2 instanceof QualifiedName) {
				return isSameVariable(sn, (QualifiedName) name2);
			} else if (name2 instanceof FieldAccess) {
				return isSameVariable(sn, (FieldAccess) name2);
			}
		} else if (name1 instanceof QualifiedName) {
			final QualifiedName qn = (QualifiedName) name1;
			if (name2 instanceof SimpleName) {
				return isSameVariable((SimpleName) name2, qn);
			} else if (name2 instanceof QualifiedName) {
				return isSameVariable(qn, (QualifiedName) name2);
			} else if (name2 instanceof FieldAccess) {
				return isSameVariable(qn, (FieldAccess) name2);
			}
		} else if (name1 instanceof FieldAccess) {
			final FieldAccess fa = (FieldAccess) name1;
			if (name2 instanceof SimpleName) {
				return isSameVariable((SimpleName) name2, fa);
			} else if (name2 instanceof QualifiedName) {
				return isSameVariable((QualifiedName) name2, fa);
			} else if (name2 instanceof FieldAccess) {
				return isSameVariable(fa, (FieldAccess) name2);
			}
		}
		return false;
	}

	/**
	 * @return the parent node filtering out ParenthesizedExpression and Blocks.
	 */
	public static Object getParent(ASTNode node) {
		ASTNode parent = node.getParent();
		if (parent instanceof ParenthesizedExpression) {
			return getParent(((ParenthesizedExpression) parent).getParent());
		} else if (parent instanceof Block) {
			return getParent(((Block) parent).getParent());
		}
		return parent;
	}

}

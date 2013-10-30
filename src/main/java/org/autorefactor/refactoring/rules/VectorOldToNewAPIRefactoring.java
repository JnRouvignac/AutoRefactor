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

import java.util.List;

import org.autorefactor.refactoring.ASTHelper;
import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.Release;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;

import static org.autorefactor.refactoring.ASTHelper.*;

/**
 * Replaces Vector pre Collections API with equivalent Collections API.
 */
public class VectorOldToNewAPIRefactoring extends ASTVisitor implements
		IJavaRefactoring {

	private RefactoringContext ctx;

	public void setRefactoringContext(RefactoringContext ctx) {
		this.ctx = ctx;
	}

	@Override
	public boolean visit(MethodInvocation node) {
		if (this.ctx.getJavaSERelease().isCompatibleWith(
				Release.javaSE("1.2.0"))) {
			if (isMethod(node, "java.util.Vector", "elementAt", "int")) {
				replaceWith(node, "get");
			} else if (isMethod(node, "java.util.Vector", "addElement",
					"java.lang.Object")) {
				replaceWith(node, "add");
			} else if (isMethod(node, "java.util.Vector", "insertElementAt",
					"java.lang.Object", "int")) {
				replaceWithAndSwapArguments(node, "add");
			} else if (isMethod(node, "java.util.Vector", "copyInto",
					"java.lang.Object[]")) {
				replaceWith(node, "toArray");
			} else if (isMethod(node, "java.util.Vector", "removeAllElements")) {
				replaceWith(node, "clear");
			} else if (isMethod(node, "java.util.Vector", "removeElement",
					"java.lang.Object")) {
				replaceWithSpecial(node, "remove");
			} else if (isMethod(node, "java.util.Vector", "removeElementAt", "int")) {
				replaceWith(node, "remove");
			} else if (isMethod(node, "java.util.Vector", "setElementAt",
					"java.lang.Object", "int")) {
				replaceWith(node, "set");
			}
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	private boolean isMethod(MethodInvocation node, String qualifiedTypeName,
			String methodName, String... paramTypes) {
		// 1- infer from MethodBinding
		// 2- if not possible, infer from actual types/parameters
		final IMethodBinding methodDeclaration = node.resolveMethodBinding();
		if (methodDeclaration == null) {
			return false;
		}
		final ITypeBinding type = methodDeclaration.getDeclaringClass();
		return type.getErasure().getQualifiedName().equals(qualifiedTypeName)
				&& methodDeclaration.getName().equals(methodName)
				&& parameterTypesEqual(paramTypes, methodDeclaration
						.getMethodDeclaration().getParameterTypes());
	}

	private boolean parameterTypesEqual(String[] paramTypes,
			ITypeBinding[] parameterTypes) {
		if (paramTypes.length != parameterTypes.length) {
			return false;
		}
		for (int i = 0; i < paramTypes.length; i++) {
			String paramType = paramTypes[i];
			String parameterType = parameterTypes[i].getErasure().getQualifiedName();
			if (!parameterType.equals(paramType)) {
				return false;
			}
		}
		return true;
	}

	private void replaceWith(MethodInvocation node, String newMethodName) {
		AST ast = this.ctx.getAST();
		MethodInvocation mi = ast.newMethodInvocation();
		mi.setName(ast.newSimpleName(newMethodName));
		mi.setExpression(ASTHelper.copySubtree(ast, node.getExpression()));
		if (node.arguments() != null) {
			mi.arguments().addAll(ASTNode.copySubtrees(ast, node.arguments()));
		}
		this.ctx.getRefactorings().replace(node, mi);
	}

	private void replaceWithSpecial(MethodInvocation node, String newMethodName) {
		AST ast = this.ctx.getAST();
		MethodInvocation mi = ast.newMethodInvocation();
		mi.setName(ast.newSimpleName(newMethodName));
		mi.setExpression(copySubtree(ast, node.getExpression()));
		final List<Expression> args = node.arguments();
		assertSize(args, 1);
		if (ASTHelper.hasType((Expression) args.get(0), "int", "short", "byte")) {
			final CastExpression ce = ast.newCastExpression();
			ce.setType(ast.newSimpleType(ast.newSimpleName("Object")));
			ce.setExpression(copySubtree(ast, args.get(0)));
			mi.arguments().add(ce);
		} else {
			mi.arguments().add(copySubtree(ast, args.get(0)));
		}
		this.ctx.getRefactorings().replace(node, mi);
	}

	private void replaceWithAndSwapArguments(MethodInvocation node,
			String newMethodName) {
		AST ast = this.ctx.getAST();
		MethodInvocation mi = ast.newMethodInvocation();
		mi.setName(ast.newSimpleName(newMethodName));
		mi.setExpression(copySubtree(ast, node.getExpression()));
		final List<Expression> args = node.arguments();
		assertSize(args, 2);
		mi.arguments().add(copySubtree(ast, args.get(1)));
		mi.arguments().add(copySubtree(ast, args.get(0)));
		this.ctx.getRefactorings().replace(node, mi);
	}

	private void assertSize(final List<Expression> args, int expectedSize) {
		if (args == null) {
			throw new IllegalArgumentException("Expected " + args
					+ "to not be null");
		}
		if (args.size() != expectedSize) {
			throw new IllegalArgumentException("Expected " + args
					+ " to have size <" + expectedSize + ">, but found <"
					+ args.size() + ">");
		}
	}

	public Refactorings getRefactorings(CompilationUnit astRoot) {
		astRoot.accept(this);
		return this.ctx.getRefactorings();
	}

}

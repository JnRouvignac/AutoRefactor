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
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;

/**
 * Replaces unnecessary primitive wrappers instance creations by using static
 * factory methods or existing constants
 */
public class PrimitiveWrapperCreationRefactoring extends ASTVisitor implements
		IJavaRefactoring {

	private final Refactorings refactorings = new Refactorings();
	private AST ast;
	private Release javaSERelease;
	private int javaMinorVersion;

	public PrimitiveWrapperCreationRefactoring() {
		super();
	}

	public void setAST(final AST ast) {
		this.ast = ast;
	}

	public void setJavaSERelease(Release javaSERelease) {
		this.javaSERelease = javaSERelease;
		this.javaMinorVersion = this.javaSERelease.getMinorVersion();
	}

	// TODO Can we reduce bad effects of autoboxing / unboxing
	// fix autoboxing and unboxing (returning boxed value in primitve
	// context)

	public boolean visit(MethodInvocation node) {
		if (node.getExpression() == null) {
			return ASTHelper.VISIT_SUBTREE;
		}
		final ITypeBinding typeBinding = node.getExpression()
				.resolveTypeBinding();
		if (typeBinding != null
				&& node.getExpression() instanceof ClassInstanceCreation) {
			final ClassInstanceCreation cic = (ClassInstanceCreation) node
					.getExpression();
			List<Expression> arguments = cic.arguments();
			if (arguments.size() == 1) {
				ITypeBinding argTypeBinding = arguments.get(0)
						.resolveTypeBinding();
				if (argTypeBinding != null
						&& node.arguments().size() == 0
						&& "java.lang.String".equals(argTypeBinding
								.getQualifiedName())) {
					final String methodName = getMethodName(
							typeBinding.getQualifiedName(), node.getName()
									.getIdentifier());
					if (methodName != null) {
						final Expression arg = (Expression) cic.arguments()
								.get(0);
						this.refactorings.replace(
								node,
								newMethodInvocation(typeBinding.getName(),
										methodName, arg));
					}
				}
			}
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	private String getMethodName(final String typeName,
			final String invokedMethodName) {
		if ("java.lang.Boolean".equals(typeName)
				&& "booleanValue".equals(invokedMethodName)) {
			return "valueOf";
		} else if ("java.lang.Byte".equals(typeName)
				&& "byteValue".equals(invokedMethodName)) {
			return "parseByte";
		} else if ("java.lang.Double".equals(typeName)
				&& "doubleValue".equals(invokedMethodName)) {
			return "parseDouble";
		} else if ("java.lang.Float".equals(typeName)
				&& "floatValue".equals(invokedMethodName)) {
			return "parseFloat";
		} else if ("java.lang.Long".equals(typeName)
				&& "longValue".equals(invokedMethodName)) {
			return "parseLong";
		} else if ("java.lang.Integer".equals(typeName)
				&& "intValue".equals(invokedMethodName)) {
			return "parseInt";
		}
		return null;
	}

	@Override
	public boolean visit(ClassInstanceCreation node) {
		final ITypeBinding typeBinding = node.getType().resolveBinding();
		if (javaMinorVersion >= 5 && typeBinding != null
				&& node.arguments().size() == 1) {
			final String qualifiedName = typeBinding.getQualifiedName();
			if ("java.lang.Boolean".equals(qualifiedName)
					|| "java.lang.Byte".equals(qualifiedName)
					|| "java.lang.Character".equals(qualifiedName)
					|| "java.lang.Double".equals(qualifiedName)
					|| "java.lang.Float".equals(qualifiedName)
					|| "java.lang.Long".equals(qualifiedName)
					|| "java.lang.Short".equals(qualifiedName)
					|| "java.lang.Integer".equals(qualifiedName)) {
				this.refactorings.replace(
						node,
						newMethodInvocation(typeBinding.getName(), "valueOf",
								(Expression) node.arguments().get(0)));
				return ASTHelper.DO_NOT_VISIT_SUBTREE;
			}
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	private MethodInvocation newMethodInvocation(String typeName,
			String methodName, Expression arg) {
		final MethodInvocation mi = this.ast.newMethodInvocation();
		mi.setExpression(this.ast.newSimpleName(typeName));
		mi.setName(this.ast.newSimpleName(methodName));
		mi.arguments().add(ASTHelper.copySubtree(mi.getAST(), arg));
		return mi;
	}

	public Refactorings getRefactorings(CompilationUnit astRoot) {
		astRoot.accept(this);
		return this.refactorings;
	}
}

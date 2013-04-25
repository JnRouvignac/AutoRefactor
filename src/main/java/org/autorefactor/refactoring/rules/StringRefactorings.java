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
 * Removes:
 * <ul>
 * <li>Creating a {@link String} instance from a {@link String} constant or
 * literal.</li>
 * <li>Calling {@link String#toString()} on a {@link String} instance</li>
 * </ul>
 *
 * @author jnrouvignac
 */
public class StringRefactorings extends ASTVisitor implements IJavaRefactoring {

	private final Refactorings refactorings = new Refactorings();
	private AST ast;
	private Release javaSERelease;

	public StringRefactorings() {
		super();
	}

	public void setAST(final AST ast) {
		this.ast = ast;
	}

	public void setJavaSERelease(Release javaSERelease) {
		this.javaSERelease = javaSERelease;
	}

	// TODO JNR remove calls to toString() inside string concatenation

	@Override
	public boolean visit(ClassInstanceCreation node) {
		final ITypeBinding typeBinding = node.getType().resolveBinding();
		if (typeBinding != null
				&& "java.lang.String".equals(typeBinding.getQualifiedName())
				&& node.arguments().size() == 1) {
			final Expression arg = (Expression) node.arguments().get(0);
			if (arg.resolveConstantExpressionValue() != null) {
				this.refactorings.replace(node,
						ASTHelper.copySubtree(this.ast, arg));
				return ASTHelper.DO_NOT_VISIT_SUBTREE;
			}
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(MethodInvocation node) {
		final Expression expression = node.getExpression();
		if (expression != null
				&& "toString".equals(node.getName().getIdentifier())
				&& node.arguments().isEmpty()
				&& canRemoveToStringMethodCall(node, expression)) {
			this.refactorings.replace(node,
					ASTHelper.copySubtree(this.ast, expression));
			return ASTHelper.DO_NOT_VISIT_SUBTREE;
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	private boolean canRemoveToStringMethodCall(MethodInvocation node,
			final Expression expression) {
		if (ASTHelper.hasType(
				ASTHelper.resolveTypeBindingForcedFromContext(node),
				"java.lang.String")) {
			// We are in a String context, no need to call toString()
			return true;
		} else if (ASTHelper.hasType(expression, "java.lang.String")) {
			// It's already a String, no need to call toString()
			return true;
		}
		return false;
	}

	public Refactorings getRefactorings(CompilationUnit astRoot) {
		astRoot.accept(this);
		return this.refactorings;
	}
}

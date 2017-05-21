/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - initial API and implementation
 * Copyright (C) 2017 Jean-Noël Rouvignac - fix NPE with Eclipse 4.5.2
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

import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.fragments;
import static org.autorefactor.refactoring.ASTHelper.getAncestorOrNull;
import static org.autorefactor.refactoring.ASTHelper.getFirstAncestorOrNull;
import static org.autorefactor.refactoring.ASTHelper.hasType;
import static org.autorefactor.refactoring.ASTHelper.removeParentheses;
import static org.autorefactor.refactoring.ASTHelper.typeArguments;
import static org.eclipse.jdt.core.dom.ASTNode.ASSIGNMENT;
import static org.eclipse.jdt.core.dom.ASTNode.RETURN_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.VARIABLE_DECLARATION_STATEMENT;

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.ParameterizedType;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

public abstract class AbstractEnumCollectionReplacementRefactoring extends
AbstractRefactoringRule {

	@Override
	public boolean visit(ClassInstanceCreation node) {
		Type type = node.getType();
		if (isEnabled() && type.isParameterizedType()
				&& creates(node, getImplType())) {

			ASTNode parent = getFirstAncestorOrNull(node,
					ReturnStatement.class, Assignment.class,
					VariableDeclarationStatement.class);

			if (parent != null) {
				switch (parent.getNodeType()) {

				case RETURN_STATEMENT: {
					return handleReturnStatement(node, (ReturnStatement) parent);
				}

				case ASSIGNMENT: {
					return handleAssignment(node, (Assignment) parent);
				}

				case VARIABLE_DECLARATION_STATEMENT: {
					return handleVarDeclarationStatement((VariableDeclarationStatement) parent);
				}

				// TODO: probably, it can be applied to method invocation for
				// some cases
				// [A.Paikin]
				// case ASTNode.METHOD_INVOCATION: {
				// return handleMethodInvocation((MethodInvocation) parent);
				// }
				}
			}
		}
		return VISIT_SUBTREE;
	}

	abstract String getImplType();

	abstract String getInterfaceType();

	boolean handleReturnStatement(ClassInstanceCreation node, ReturnStatement rs) {
		MethodDeclaration md = getAncestorOrNull(node, MethodDeclaration.class);
		if (md != null) {
			Type returnType = md.getReturnType2();
			if (isTargetType(returnType)) {
				List<Type> typeArguments = typeArgs(returnType);
				if (isEnum(typeArguments.get(0))) {
					return replace(node, typeArguments.toArray(new Type[] {}));
				}
			}
		}
		return VISIT_SUBTREE;
	}

	abstract boolean replace(ClassInstanceCreation node, Type... types);

	boolean handleAssignment(ClassInstanceCreation node, Assignment a) {
		Expression lhs = a.getLeftHandSide();
		if (isTargetType(lhs.resolveTypeBinding())) {

			ITypeBinding[] typeArguments = lhs.resolveTypeBinding()
					.getTypeArguments();
			ITypeBinding keyTypeBinding = typeArguments[0];
			if (keyTypeBinding.isEnum()) {
				ASTBuilder b = ctx.getASTBuilder();
				Type[] types = new Type[typeArguments.length];
				for (int i = 0; i < types.length; i++) {
					types[i] = b.type(typeArguments[i].getName());
				}
				return replace(node, types);
			}
		}
		return VISIT_SUBTREE;
	}

	boolean handleVarDeclarationStatement(VariableDeclarationStatement node) {
		Type type = node.getType();
		if (type.isParameterizedType() && isTargetType(type)) {

			ParameterizedType ptype = (ParameterizedType) type;
			List<Type> typeArguments = typeArguments(ptype);
			if (typeArguments.get(0).resolveBinding().isEnum()) {
				List<VariableDeclarationFragment> fragments = fragments(node);
				for (VariableDeclarationFragment vdf : fragments) {
					Expression initExpr = vdf.getInitializer();
					if (initExpr != null) {
						initExpr = removeParentheses(initExpr);
						if (creates(initExpr, getImplType())) {
							return replace((ClassInstanceCreation) initExpr,
									typeArguments.toArray(new Type[] {}));
						}
					}
				}
			}

		}
		return VISIT_SUBTREE;
	}

	/**
	 * Just one more wrapper to extract type arguments, <br>
	 * to avoid boilerplate casting and shorten method name.
	 */
	List<Type> typeArgs(Type parameterizedType) {
		return typeArguments((ParameterizedType) parameterizedType);
	}

	boolean isTargetType(ITypeBinding it) {
		return hasType(it, getInterfaceType());
	}

	private boolean isEnum(Type type) {
		return type.resolveBinding().isEnum();
	}

	private boolean creates(Expression exp, String type) {
		return exp.getNodeType() == Expression.CLASS_INSTANCE_CREATION
				&& hasType(exp.resolveTypeBinding(), type);
	}

	private boolean isTargetType(Type type) {
		return type != null && type.isParameterizedType()
				&& isTargetType(type.resolveBinding());
	}

	private boolean isEnabled() {
		return ctx.getJavaProjectOptions().getJavaSERelease().getMinorVersion() >= 5;
	}

}

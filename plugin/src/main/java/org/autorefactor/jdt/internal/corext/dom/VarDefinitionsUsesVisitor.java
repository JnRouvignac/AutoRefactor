/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.jdt.internal.corext.dom;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.ChildPropertyDescriptor;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

/** Visitor collecting all definitions and uses of a local variable. */
public final class VarDefinitionsUsesVisitor extends ASTVisitor {
	private final IVariableBinding variableBinding;
	private final ASTNode scopeNode;
	private final boolean includeInnerScopes;
	private final List<SimpleName> writes= new ArrayList<>();
	private final List<SimpleName> reads= new ArrayList<>();

	/**
	 * Builds from a {@link VariableDeclaration} and infers the variable binding and
	 * the scope from it.
	 *
	 * @param variableDeclaration the variable declaration, cannot be {@code null}
	 */
	public VarDefinitionsUsesVisitor(final VariableDeclaration variableDeclaration) {
		this(variableDeclaration.resolveBinding(), getDeclaringScope(variableDeclaration), true);
	}

	/**
	 * Builds with the variable binding to look for and the scope where to look for
	 * references.
	 *
	 * @param variableBinding the variable binding to find, cannot be {@code null}
	 * @param scopeNode       the {@link ASTNode} which is the scope of the search
	 * @param includeInnerScopes True if the sub blocks should be analyzed
	 */
	public VarDefinitionsUsesVisitor(final IVariableBinding variableBinding, final ASTNode scopeNode, final boolean includeInnerScopes) {
		this.variableBinding= variableBinding;
		this.scopeNode= scopeNode;
		this.includeInnerScopes= includeInnerScopes;
	}

	private static ASTNode getDeclaringScope(final VariableDeclaration variableDeclaration) {
		ASTNode node= variableDeclaration.getParent();
		while (isVariableDeclaration(node)) {
			node= node.getParent();
		}

		return node;
	}

	private static boolean isVariableDeclaration(final ASTNode node) {
		switch (node.getNodeType()) {
		case ASTNode.SINGLE_VARIABLE_DECLARATION:
		case ASTNode.VARIABLE_DECLARATION_EXPRESSION:
		case ASTNode.VARIABLE_DECLARATION_FRAGMENT:
		case ASTNode.VARIABLE_DECLARATION_STATEMENT:
			return true;

		default:
			return false;
		}
	}

	/**
	 * Finds all the definitions and uses of the variable.
	 *
	 * @return this visitor
	 */
	public VarDefinitionsUsesVisitor find() {
		if (variableBinding != null && scopeNode != null) {
			scopeNode.accept(this);
		}

		return this;
	}

	@Override
	public boolean visit(final SimpleName node) {
		if (ASTNodes.isSameLocalVariable(variableBinding, node)) {
			switch (node.getParent().getNodeType()) {
			case ASTNode.ASSIGNMENT:
				addWriteOrRead(node, Assignment.LEFT_HAND_SIDE_PROPERTY);
				break;

			case ASTNode.VARIABLE_DECLARATION_FRAGMENT:
				addWriteOrRead(node, VariableDeclarationFragment.NAME_PROPERTY);
				break;

			case ASTNode.SINGLE_VARIABLE_DECLARATION:
				addWriteOrRead(node, SingleVariableDeclaration.NAME_PROPERTY);
				break;

			default:
				reads.add(node);
				break;
			}
		}

		return true;
	}

	@Override
	public boolean visit(final Block node) {
		return scopeNode == node || includeInnerScopes;
	}

	private void addWriteOrRead(final SimpleName node, final ChildPropertyDescriptor definitionPropertyDescriptor) {
		if (node.getLocationInParent() == definitionPropertyDescriptor) {
			writes.add(node);
		} else {
			reads.add(node);
		}
	}

	/**
	 * Returns all the definitions (declarations and assignments) found.
	 *
	 * @return all the definitions found.
	 */
	public List<SimpleName> getWrites() {
		return writes;
	}

	/**
	 * Returns all the uses found.
	 *
	 * @return all the uses found.
	 */
	public List<SimpleName> getReads() {
		return reads;
	}
}

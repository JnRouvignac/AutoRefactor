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
package org.autorefactor.refactoring.rules;

import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.isSameLocalVariable;
import static org.eclipse.jdt.core.dom.ASTNode.ASSIGNMENT;
import static org.eclipse.jdt.core.dom.ASTNode.SINGLE_VARIABLE_DECLARATION;
import static org.eclipse.jdt.core.dom.ASTNode.VARIABLE_DECLARATION_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.VARIABLE_DECLARATION_FRAGMENT;
import static org.eclipse.jdt.core.dom.ASTNode.VARIABLE_DECLARATION_STATEMENT;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.ChildPropertyDescriptor;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

/** Visitor collecting all definitions and uses of a variable. */
public final class VariableDefinitionsUsesVisitor extends ASTVisitor {
    private final IVariableBinding variableBinding;
    private final ASTNode scopeNode;
    private final List<SimpleName> definitions = new ArrayList<SimpleName>();
    private final List<SimpleName> uses = new ArrayList<SimpleName>();

    /**
     * Builds from a {@link VariableDeclaration} and infers the variable binding and the scope from it.
     *
     * @param variableDeclaration the variable declaration, cannot be {@code null}
     */
    public VariableDefinitionsUsesVisitor(VariableDeclaration variableDeclaration) {
        this(variableDeclaration.resolveBinding(), getDeclaringScope(variableDeclaration));
    }

    /**
     * Builds with the variable binding to look for and the scope where to look for references.
     *
     * @param variableBinding the variable binding to find, cannot be {@code null}
     * @param scopeNode the {@link ASTNode} which is the scope of the search
     */
    public VariableDefinitionsUsesVisitor(IVariableBinding variableBinding, ASTNode scopeNode) {
        this.variableBinding = variableBinding;
        this.scopeNode = scopeNode;
    }

    private static ASTNode getDeclaringScope(VariableDeclaration variableDeclaration) {
        ASTNode node = variableDeclaration.getParent();
        while (isVariableDeclaration(node)) {
            node = node.getParent();
        }
        return node;
    }

    private static boolean isVariableDeclaration(ASTNode node) {
        switch (node.getNodeType()) {
        case SINGLE_VARIABLE_DECLARATION:
        case VARIABLE_DECLARATION_EXPRESSION:
        case VARIABLE_DECLARATION_FRAGMENT:
        case VARIABLE_DECLARATION_STATEMENT:
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
    public VariableDefinitionsUsesVisitor find() {
        if (variableBinding != null && scopeNode != null) {
            scopeNode.accept(this);
        }
        return this;
    }

    @Override
    public boolean visit(SimpleName node) {
        if (isSameLocalVariable(variableBinding, node)) {
            switch (node.getParent().getNodeType()) {
            case ASSIGNMENT:
                addDefinitionOrUse(node, Assignment.LEFT_HAND_SIDE_PROPERTY);
                break;

            case VARIABLE_DECLARATION_FRAGMENT:
                addDefinitionOrUse(node, VariableDeclarationFragment.NAME_PROPERTY);
                break;

            case SINGLE_VARIABLE_DECLARATION:
                addDefinitionOrUse(node, SingleVariableDeclaration.NAME_PROPERTY);
                break;

            default:
                uses.add(node);
                break;
            }
        }
        return VISIT_SUBTREE;
    }

    private void addDefinitionOrUse(SimpleName node, ChildPropertyDescriptor definitionPropertyDescriptor) {
        if (node.getLocationInParent() == definitionPropertyDescriptor) {
            definitions.add(node);
        } else {
            uses.add(node);
        }
    }

    /**
     * Returns all the definitions (declarations and assignments) found.
     *
     * @return all the definitions found.
     */
    public List<SimpleName> getDefinitions() {
        return definitions;
    }

    /**
     * Returns all the uses found.
     *
     * @return all the uses found.
     */
    public List<SimpleName> getUses() {
        return uses;
    }
}

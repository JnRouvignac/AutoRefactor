/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2015 Jean-Noël Rouvignac - initial API and implementation
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

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.initializers;
import static org.autorefactor.refactoring.ASTHelper.updaters;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.WhileStatement;

/** See {@link #getDescription()} method. */
public class ForeachRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Foreach";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Replaces \"while\"/\"for with iterator\"/\"for with index loops\" into foreach loops"
            + " (applicable to arrays or Iterable)."
            + "Replaces Map.keySet() iteration with calls to Map.get()} into iterations over Map.entrySet().";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces code to focus attention on code that matters.";
    }

    private static class VariableUseVisitor extends ASTVisitor {
        @Override
        public boolean visit(SimpleName node) {
            ASTNode parent = node.getParent();
            if (parent instanceof QualifiedName
                    || parent instanceof FieldAccess) {
                return DO_NOT_VISIT_SUBTREE;
            }
            return DO_NOT_VISIT_SUBTREE;
        }
    }

    @Override
    public boolean visit(ForStatement node) {
        final VariableUseVisitor variableUseVisitor = new VariableUseVisitor();
        node.accept(variableUseVisitor);

        if (initializers(node).size() == 1) {
            initializers(node);
        }

        node.getExpression();
        updaters(node);
        node.getBody();
        // TODO JNR iterate over array with index
        // TODO JNR iterate over array with temporary variable with generics
        // TODO JNR iterate over array with temporary variable without generics
        // TODO JNR iterate over col with index
        // TODO JNR iterate over col with temporary variable with generics
        // TODO JNR iterate over col with temporary variable without generics
        // TODO JNR iterate over col with Iterator with generics
        // TODO JNR iterate over col with Iterator without generics
        // TODO JNR iterate over col with ListIterator with generics
        // TODO JNR iterate over col with ListIterator without generics
        // be careful with use of index/iterator inside the loop
        return VISIT_SUBTREE;
    }

    @Override
    public boolean visit(WhileStatement node) {
        node.getExpression();
        node.getBody();
        // TODO JNR iterate over array with index
        // TODO JNR iterate over array with temporary variable with generics
        // TODO JNR iterate over array with temporary variable without generics
        // TODO JNR iterate over col with index
        // TODO JNR iterate over col with temporary variable with generics
        // TODO JNR iterate over col with temporary variable without generics
        // TODO JNR iterate over col with Iterator with generics
        // TODO JNR iterate over col with Iterator without generics
        // TODO JNR iterate over col with ListIterator with generics
        // TODO JNR iterate over col with ListIterator without generics
        // be careful with use of index/iterator inside the loop
        return VISIT_SUBTREE;
    }
}

/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015-2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Make sure we do not visit again modified nodes
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
import static org.autorefactor.refactoring.ASTHelper.arg0;
import static org.autorefactor.refactoring.ASTHelper.arguments;
import static org.autorefactor.refactoring.ASTHelper.as;
import static org.autorefactor.refactoring.ASTHelper.asExpression;
import static org.autorefactor.refactoring.ASTHelper.getPreviousSibling;
import static org.autorefactor.refactoring.ASTHelper.getUniqueFragment;
import static org.autorefactor.refactoring.ASTHelper.hasOperator;
import static org.autorefactor.refactoring.ASTHelper.hasType;
import static org.autorefactor.refactoring.ASTHelper.isCastCompatible;
import static org.autorefactor.refactoring.ASTHelper.isMethod;
import static org.autorefactor.refactoring.ASTHelper.isPrimitive;
import static org.autorefactor.refactoring.ASTHelper.isSameLocalVariable;

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/** See {@link #getDescription()} method. */
public class MapRefactoring extends AbstractRefactoringRule {
    @Override
    public String getDescription() {
        return ""
            + "Map related refactorings:\n"
            + "- replaces creating a new Map, then invoking Map.putAll() on it,"
            + " by creating the new Map with the other Map as parameter,\n"
            + "- replaces some checks on Map.size() with checks on Map.isEmpty().";
    }

    @Override
    public String getName() {
        return "Map";
    }

    @Override
    public boolean visit(ExpressionStatement node) {
        final MethodInvocation mi = asExpression(node, MethodInvocation.class);
        if (isMethod(mi, "java.util.Map", "putAll", "java.util.Map")) {
            final Expression arg0 = arg0(mi);
            final Statement previousStmt = getPreviousSibling(node);

            final Assignment as = asExpression(previousStmt, Assignment.class);
            if (hasOperator(as, Assignment.Operator.ASSIGN)) {
                final Expression lhs = as.getLeftHandSide();
                if (lhs instanceof SimpleName) {
                    if (isSameLocalVariable(lhs, mi.getExpression())) {
                        return replaceInitializer(as.getRightHandSide(), arg0, node);
                    }
                }
            } else if (previousStmt instanceof VariableDeclarationStatement) {
                final VariableDeclarationFragment vdf = getUniqueFragment((VariableDeclarationStatement) previousStmt);
                if (vdf != null && isSameLocalVariable(vdf, mi.getExpression())) {
                    return replaceInitializer(vdf.getInitializer(), arg0, node);
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean replaceInitializer(Expression nodeToReplace,
            final Expression arg0, ExpressionStatement nodeToRemove) {
        final ClassInstanceCreation cic = as(nodeToReplace, ClassInstanceCreation.class);
        if (canReplaceInitializer(cic)
                && isCastCompatible(nodeToReplace, arg0)) {
            final ASTBuilder b = this.ctx.getASTBuilder();
            this.ctx.getRefactorings().replace(nodeToReplace,
                    b.new0(b.copy(cic.getType()), b.copy(arg0)));
            this.ctx.getRefactorings().remove(nodeToRemove);
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private boolean canReplaceInitializer(final ClassInstanceCreation cic) {
        if (cic == null) {
            return false;
        }
        final List<Expression> args = arguments(cic);
        final boolean noArgsCtor = args.size() == 0;
        final boolean mapCapacityCtor = args.size() == 1 && isPrimitive(args.get(0), "int");
        if (noArgsCtor && hasType(cic,
                "java.util.concurrent.ConcurrentHashMap",
                "java.util.concurrent.ConcurrentSkipListMap",
                "java.util.Hashtable",
                "java.util.HashMap",
                "java.util.IdentityHashMap",
                "java.util.LinkedHashMap",
                "java.util.TreeMap",
                "java.util.WeakHashMap")) {
            return true;
        }
        if (mapCapacityCtor && hasType(cic,
                "java.util.concurrent.ConcurrentHashMap",
                "java.util.Hashtable",
                "java.util.HashMap",
                "java.util.IdentityHashMap",
                "java.util.LinkedHashMap",
                "java.util.WeakHashMap")) {
            // TODO JNR verify capacity arguments is 0, 1 or map.length
            return true;
        }
        return false;
    }
}

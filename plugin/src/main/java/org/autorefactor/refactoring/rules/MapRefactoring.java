/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015-2016 Jean-Noël Rouvignac - initial API and implementation
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

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.*;

/** See {@link #getDescription()} method. */
@SuppressWarnings("javadoc")
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
                    final SimpleName sn = (SimpleName) lhs;
                    if (isSameLocalVariable(mi.getExpression(), sn.resolveBinding())) {
                        return replaceInitializer(as.getRightHandSide(), arg0, node);
                    }
                }
            } else if (previousStmt instanceof VariableDeclarationStatement) {
                final VariableDeclarationStatement vds = (VariableDeclarationStatement) previousStmt;
                if (vds.fragments().size() == 1) {
                    final VariableDeclarationFragment vdf = fragments(vds).get(0);
                    if (isSameLocalVariable(mi.getExpression(), vdf.resolveBinding())) {
                        return replaceInitializer(vdf.getInitializer(), arg0, node);
                    }
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean replaceInitializer(Expression nodeToReplace,
            final Expression arg0, ExpressionStatement nodeToRemove) {
        final ClassInstanceCreation cic = as(nodeToReplace, ClassInstanceCreation.class);
        if (canReplaceInitializer(cic)) {
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

    private boolean isSameLocalVariable(Expression expr, IBinding varBinding) {
        return expr instanceof SimpleName && varBinding instanceof IVariableBinding
            && ((SimpleName) expr).resolveBinding().equals(varBinding);
    }

    @Override
    public boolean visit(InfixExpression node) {
        final MethodInvocation leftMi = as(node.getLeftOperand(), MethodInvocation.class);
        final MethodInvocation rightMi = as(node.getRightOperand(), MethodInvocation.class);
        if (isMethod(leftMi, "java.util.Map", "size")
                && isZero(node.getRightOperand())) {
            return replaceCollectionSize(node, leftMi);
        } else if (isMethod(rightMi, "java.util.Map", "size")
                && isZero(node.getLeftOperand())) {
            return replaceInverseCollectionSize(node, rightMi);
        }
        return VISIT_SUBTREE;
    }

    private boolean replaceCollectionSize(InfixExpression node, MethodInvocation miToReplace) {
        final Refactorings r = this.ctx.getRefactorings();
        final ASTBuilder b = this.ctx.getASTBuilder();
        final Expression copyOfExpr = b.copyExpression(miToReplace);
        if (hasOperator(node, GREATER)) {
            r.replace(node,
                    b.not(b.invoke(copyOfExpr, "isEmpty")));
            return DO_NOT_VISIT_SUBTREE;
        } else if (hasOperator(node, GREATER_EQUALS)) {
            r.replace(node,
                    b.boolean0(true));
            return DO_NOT_VISIT_SUBTREE;
        } else if (hasOperator(node, EQUALS)) {
            r.replace(node,
                    b.invoke(copyOfExpr, "isEmpty"));
            return DO_NOT_VISIT_SUBTREE;
        } else if (hasOperator(node, LESS_EQUALS)) {
            r.replace(node,
                    b.invoke(copyOfExpr, "isEmpty"));
            return DO_NOT_VISIT_SUBTREE;
        } else if (hasOperator(node, LESS)) {
            r.replace(node,
                    b.boolean0(false));
        }
        return VISIT_SUBTREE;
    }

    private boolean replaceInverseCollectionSize(InfixExpression node, MethodInvocation miToReplace) {
        final Refactorings r = this.ctx.getRefactorings();
        final ASTBuilder b = this.ctx.getASTBuilder();
        final Expression copyOfExpr = b.copyExpression(miToReplace);
        if (hasOperator(node, LESS)) {
            r.replace(node,
                    b.not(b.invoke(copyOfExpr, "isEmpty")));
            return DO_NOT_VISIT_SUBTREE;
        } else if (hasOperator(node, LESS_EQUALS)) {
            r.replace(node,
                    b.boolean0(true));
            return DO_NOT_VISIT_SUBTREE;
        } else if (hasOperator(node, EQUALS)) {
            r.replace(node,
                    b.invoke(copyOfExpr, "isEmpty"));
            return DO_NOT_VISIT_SUBTREE;
        } else if (hasOperator(node, GREATER_EQUALS)) {
            r.replace(node,
                    b.invoke(copyOfExpr, "isEmpty"));
            return DO_NOT_VISIT_SUBTREE;
        } else if (hasOperator(node, GREATER)) {
            r.replace(node,
                    b.boolean0(false));
        }
        return VISIT_SUBTREE;
    }

    private boolean isZero(Expression expr) {
        if (expr != null) {
            final Object val = expr.resolveConstantExpressionValue();
            if (val instanceof Integer) {
                return ((Integer) val).intValue() == 0;
            } else if (val instanceof Long) {
                return ((Long) val).longValue() == 0;
            }
        }
        return false;
    }
}

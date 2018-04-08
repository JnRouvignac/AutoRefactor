/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Separate the code.
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
import static org.autorefactor.refactoring.ASTHelper.as;
import static org.autorefactor.refactoring.ASTHelper.asExpression;
import static org.autorefactor.refactoring.ASTHelper.asList;
import static org.autorefactor.refactoring.ASTHelper.findImplementedType;
import static org.autorefactor.refactoring.ASTHelper.instanceOf;
import static org.autorefactor.refactoring.ASTHelper.isArray;
import static org.autorefactor.refactoring.ASTHelper.isMethod;
import static org.autorefactor.refactoring.ASTHelper.isSameLocalVariable;
import static org.autorefactor.refactoring.ForLoopHelper.iterateOverContainer;

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.ForLoopHelper.ForLoopContent;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ArrayAccess;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;

/** See {@link #getDescription()} method. */
public class AllInOneMethodRatherThanLoopRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "All in one method rather than loop";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Collection related refactorings:\n"
            + "- replaces for/foreach loops to use Collections.addAll() where possible,\n"
            + "- replaces for/foreach loops to use Collection.addAll() where possible,\n"
            + "- replaces for/foreach loops to use Collection.removeAll() where possible.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces code to focus attention on code that matters.";
    }

    @Override
    public boolean visit(EnhancedForStatement node) {
        final Expression iterable = node.getExpression();
        final List<Statement> stmts = asList(node.getBody());
        if (stmts.size() != 1) {
            return VISIT_SUBTREE;
        }

        final MethodInvocation mi = asExpression(stmts.get(0), MethodInvocation.class);
        final IVariableBinding foreachVariable = node.getParameter().resolveBinding();
        // We should remove all the loop variable occurrences
        // As we replace only one, there should be no more than one occurrence
        if (getVariableUseCount(foreachVariable, node.getBody()) == 1) {
            if (instanceOf(iterable, "java.util.Collection")) {
                if (isMethod(mi, "java.util.Collection", "add", "java.lang.Object")) {
                    return maybeReplaceWithCollectionMethod(node, iterable, "addAll", mi);
                } else if (isMethod(mi, "java.util.Collection", "remove", "java.lang.Object")) {
                    return maybeReplaceWithCollectionMethod(node, iterable, "removeAll", mi);
                }
            } else if (isArray(iterable)
                    && isMethod(mi, "java.util.Collection", "add", "java.lang.Object")
                    && areTypeCompatible(mi.getExpression(), iterable)
                    && isSameLocalVariable(foreachVariable, arg0(mi))) {
                replaceWithCollectionsAddAll(node, iterable, mi);
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    private void replaceWithCollectionsAddAll(Statement node, Expression iterable, MethodInvocation mi) {
        ASTBuilder b = ctx.getASTBuilder();
        ctx.getRefactorings().replace(node,
                b.toStmt(b.invoke(
                        b.name("java", "util", "Collections"),
                        "addAll",
                        b.copy(mi.getExpression()),
                        b.copy(iterable))));
    }

    private boolean maybeReplaceWithCollectionMethod(EnhancedForStatement node,
            Expression collection, String methodName, MethodInvocation colMI) {
        if (isSameLocalVariable(node.getParameter(), arg0(colMI))) {
            replaceWithCollectionMethod(node, methodName, colMI.getExpression(), collection);
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    @Override
    public boolean visit(ForStatement node) {
        final ForLoopContent loopContent = iterateOverContainer(node);
        final List<Statement> stmts = asList(node.getBody());
        if (loopContent != null
                && loopContent.getLoopVariable() != null
                && stmts.size() == 1) {
            final SimpleName loopVariable = (SimpleName) loopContent.getLoopVariable();
            final IVariableBinding loopVariableName = (IVariableBinding) loopVariable.resolveBinding();
            // We should remove all the loop variable occurrences
            // As we replace only one, there should be no more than one occurrence
            if (getVariableUseCount(loopVariableName, node.getBody()) == 1) {
                final MethodInvocation mi = asExpression(stmts.get(0), MethodInvocation.class);
                switch (loopContent.getContainerType()) {
                case COLLECTION:
                    if (isMethod(mi, "java.util.Collection", "add", "java.lang.Object")) {
                        return maybeReplaceWithCollectionMethod(node, loopContent, "addAll", mi);
                    } else if (isMethod(mi, "java.util.Collection", "remove", "java.lang.Object")) {
                        return maybeReplaceWithCollectionMethod(node, loopContent, "removeAll", mi);
                    }
                    break;
                case ARRAY:
                    if (isMethod(mi, "java.util.Collection", "add", "java.lang.Object")
                            && areTypeCompatible(mi.getExpression(), loopContent.getContainerVariable())) {
                        final Expression addArg0 = arg0(mi);
                        final ArrayAccess aa = as(addArg0, ArrayAccess.class);
                        if (isSameVariable(loopContent, aa)) {
                            replaceWithCollectionsAddAll(node, loopContent.getContainerVariable(), mi);
                            return DO_NOT_VISIT_SUBTREE;
                        }
                    }
                    break;
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private int getVariableUseCount(final IVariableBinding variableBinding, Statement toVisit) {
        if (variableBinding != null) {
            final VariableDefinitionsUsesVisitor variableUseVisitor =
                new VariableDefinitionsUsesVisitor(variableBinding, toVisit).find();
            return variableUseVisitor.getUses().size();
        }
        return 0;
    }

    private boolean isSameVariable(ForLoopContent loopContent, ArrayAccess aa) {
        return aa != null
            && isSameLocalVariable(aa.getArray(), loopContent.getContainerVariable())
            && isSameLocalVariable(aa.getIndex(), loopContent.getLoopVariable());
    }

    private boolean areTypeCompatible(Expression colExpr, Expression arrayExpr) {
        ITypeBinding arrayTypeBinding = arrayExpr.resolveTypeBinding();
        ITypeBinding colTypeBinding = colExpr.resolveTypeBinding();
        if (arrayTypeBinding != null && colTypeBinding != null) {
            ITypeBinding jucTypeBinding = findImplementedType(colTypeBinding, "java.util.Collection");
            if (jucTypeBinding.isRawType()) {
                return true;
            }
            ITypeBinding componentType = arrayTypeBinding.getComponentType();
            ITypeBinding colTypeArgument = jucTypeBinding.getTypeArguments()[0];
            return componentType.isSubTypeCompatible(colTypeArgument);
        }
        return false;
    }

    private boolean maybeReplaceWithCollectionMethod(ForStatement node, ForLoopContent loopContent,
            String methodName, MethodInvocation colMI) {
        final Expression addArg0 = arg0(colMI);
        final MethodInvocation getMI = as(addArg0, MethodInvocation.class);
        if (isSameVariable(loopContent, getMI)) {
            replaceWithCollectionMethod(node, methodName,
                    colMI.getExpression(), getMI.getExpression());
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private boolean isSameVariable(ForLoopContent loopContent, final MethodInvocation getMI) {
        return isMethod(getMI, "java.util.List", "get", "int")
                && getMI.getExpression() instanceof Name
                && isSameLocalVariable(arg0(getMI), loopContent.getLoopVariable());
    }

    private void replaceWithCollectionMethod(ASTNode toReplace, String methodName,
            Expression colWhereToAddAll, Expression colToAddAll) {
        final ASTBuilder b = ctx.getASTBuilder();
        if (colWhereToAddAll != null) {
            ctx.getRefactorings().replace(toReplace,
                    b.toStmt(b.invoke(
                            b.copy(colWhereToAddAll),
                            methodName,
                            b.copy(colToAddAll))));
        } else {
            ctx.getRefactorings().replace(toReplace,
                    b.toStmt(b.invoke(
                            methodName,
                            b.copy(colToAddAll))));
        }
    }
}

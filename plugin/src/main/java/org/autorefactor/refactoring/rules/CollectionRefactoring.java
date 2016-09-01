/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Annoying remaining loop variable occurrence
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
import org.autorefactor.refactoring.ForLoopHelper.ForLoopContent;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.ASTMatcher;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.ArrayAccess;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

import static org.autorefactor.refactoring.ASTBuilder.Copy.*;
import static org.autorefactor.refactoring.ASTHelper.*;
import static org.autorefactor.refactoring.ForLoopHelper.*;
import static org.autorefactor.refactoring.ForLoopHelper.ContainerType.*;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.*;
import static org.eclipse.jdt.core.dom.PrefixExpression.Operator.*;

/** See {@link #getDescription()} method. */
public class CollectionRefactoring extends AbstractRefactoringRule {

    private final class VariableUseCounterVisitor extends ASTVisitor {
        private final IBinding variableBinding;
        private int useCount = 0;

        public int getUseCount() {
            return useCount;
        }

        private VariableUseCounterVisitor(IBinding variableBinding) {
            this.variableBinding = variableBinding;
        }

        @Override
        public boolean visit(SimpleName node) {
            if (isSameLocalVariable(variableBinding, node)) {
                useCount++;
            }
            return VISIT_SUBTREE;
        }
    }

    @Override
    public String getDescription() {
        return ""
            + "Collection related refactorings:\n"
            + "- replaces for/foreach loops to use Collection.addAll() where possible,\n"
            + "- replaces for/foreach loops to use Collection.containsAll() where possible,\n"
            + "- replaces for/foreach loops to use Collection.removeAll() where possible,\n"
            + "- replaces for/foreach loops to use Collections.addAll() where possible,\n"
            + "- replaces creating a new Collection, then invoking Collection.addAll() on it,"
            + " by creating the new Collection with the other Collection as parameter,\n"
            + "- replaces some checks on Collection.size() with checks on Collection.isEmpty(),\n"
            + "- replaces calls to Set.contains() immediately followed by Set.add() with straight calls to Set.add().";
    }

    @Override
    public String getName() {
        return "Collection";
    }

    @Override
    public boolean visit(ExpressionStatement node) {
        final MethodInvocation mi = asExpression(node, MethodInvocation.class);
        if (isMethod(mi, "java.util.Collection", "addAll", "java.util.Collection")) {
            final Expression arg0 = arg0(mi);
            final Statement previousStmt = getPreviousSibling(node);

            final Assignment as = asExpression(previousStmt, Assignment.class);
            if (hasOperator(as, Assignment.Operator.ASSIGN)) {
                final Expression lhs = as.getLeftHandSide();
                if (lhs instanceof SimpleName) {
                    final SimpleName sn = (SimpleName) lhs;
                    if (isSameLocalVariable(sn.resolveBinding(), mi.getExpression())) {
                        return replaceInitializer(as.getRightHandSide(), arg0, node);
                    }
                }
            } else if (previousStmt instanceof VariableDeclarationStatement) {
                final VariableDeclarationStatement vds = (VariableDeclarationStatement) previousStmt;
                if (vds.fragments().size() == 1) {
                    final VariableDeclarationFragment vdf = fragments(vds).get(0);
                    if (isSameLocalVariable(vdf.resolveBinding(), mi.getExpression())) {
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
        final boolean colCapacityCtor = args.size() == 1 && isPrimitive(args.get(0), "int");
        if (noArgsCtor && hasType(cic,
                "java.util.concurrent.ConcurrentLinkedDeque",
                "java.util.concurrent.ConcurrentLinkedQueue",
                "java.util.concurrent.ConcurrentSkipListSet",
                "java.util.concurrent.CopyOnWriteArrayList",
                "java.util.concurrent.CopyOnWriteArraySet",
                "java.util.concurrent.DelayQueue",
                "java.util.concurrent.LinkedBlockingDeque",
                "java.util.concurrent.LinkedBlockingQueue",
                "java.util.concurrent.LinkedTransferQueue",
                "java.util.concurrent.PriorityBlockingQueue",
                "java.util.ArrayDeque",
                "java.util.ArrayList",
                "java.util.HashSet",
                "java.util.LinkedHashSet",
                "java.util.LinkedList",
                "java.util.PriorityQueue",
                "java.util.TreeSet",
                "java.util.Vector")) {
            return true;
        }
        if (colCapacityCtor && hasType(cic,
                "java.util.concurrent.LinkedBlockingDeque",
                "java.util.concurrent.LinkedBlockingQueue",
                "java.util.concurrent.PriorityBlockingQueue",
                "java.util.ArrayDeque",
                "java.util.ArrayList",
                "java.util.HashSet",
                "java.util.LinkedHashSet",
                "java.util.PriorityQueue",
                "java.util.Vector")) {
            // TODO JNR verify capacity arguments is 0, 1 or col.length
            return true;
        }
        return false;
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
                    return replaceWithCollectionMethod(node, iterable, "addAll", mi);
                } else if (isMethod(mi, "java.util.Collection", "contains", "java.lang.Object")) {
                    return replaceWithCollectionMethod(node, iterable, "containsAll", mi);
                } else if (isMethod(mi, "java.util.Collection", "remove", "java.lang.Object")) {
                    return replaceWithCollectionMethod(node, iterable, "removeAll", mi);
                }
            } else if (isArray(iterable)) {
                if (isMethod(mi, "java.util.Collection", "add", "java.lang.Object")
                        && areTypeCompatible(mi.getExpression(), iterable)
                        && isSameLocalVariable(node.getParameter().resolveBinding(), arg0(mi))) {
                    return replaceWithCollectionsAddAll(node, iterable, mi);
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean replaceWithCollectionsAddAll(Statement node, Expression iterable, MethodInvocation mi) {
        ASTBuilder b = ctx.getASTBuilder();
        ctx.getRefactorings().replace(node,
                b.toStmt(b.invoke(
                        b.name("java", "util", "Collections"),
                        "addAll",
                        b.copy(mi.getExpression()),
                        b.copy(iterable))));
        return DO_NOT_VISIT_SUBTREE;
    }

    private boolean replaceWithCollectionMethod(EnhancedForStatement node,
            Expression collection, String methodName, MethodInvocation colMI) {
        final SingleVariableDeclaration foreachVariable = node.getParameter();
        if (isSameLocalVariable(foreachVariable.resolveBinding(), arg0(colMI))) {
            return replaceWithCollectionMethod(node, methodName,
                    colMI.getExpression(), collection);
        }
        return VISIT_SUBTREE;
    }

    @Override
    public boolean visit(ForStatement node) {
        final ForLoopContent loopContent = iterateOverContainer(node);
        final List<Statement> stmts = asList(node.getBody());
        if (loopContent != null
                && stmts.size() == 1) {

            final Name loopVariable = loopContent.getLoopVariable();
            final IBinding loopVariableName = ((SimpleName) loopVariable).resolveBinding();
            // We should remove all the loop variable occurrences
            // As we replace only one, there should be no more than one occurrence
            if (getVariableUseCount(loopVariableName, node.getBody()) == 1) {
                final MethodInvocation mi = asExpression(stmts.get(0), MethodInvocation.class);
                if (COLLECTION.equals(loopContent.getContainerType())) {
                    if (isMethod(mi, "java.util.Collection", "add", "java.lang.Object")) {
                        return replaceWithCollectionMethod(node, loopContent, "addAll", mi);
                    } else if (isMethod(mi, "java.util.Collection", "contains", "java.lang.Object")) {
                        return replaceWithCollectionMethod(node, loopContent, "containsAll", mi);
                    } else if (isMethod(mi, "java.util.Collection", "remove", "java.lang.Object")) {
                        return replaceWithCollectionMethod(node, loopContent, "removeAll", mi);
                    }
                } else if (ARRAY.equals(loopContent.getContainerType())) {
                    if (isMethod(mi, "java.util.Collection", "add", "java.lang.Object")
                            && areTypeCompatible(mi.getExpression(), loopContent.getContainerVariable())) {
                        final Expression addArg0 = arg0(mi);
                        final ArrayAccess aa = as(addArg0, ArrayAccess.class);
                        if (isSameVariable(loopContent, aa)) {
                            return replaceWithCollectionsAddAll(node, loopContent.getContainerVariable(), mi);
                        }
                    }
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private int getVariableUseCount(final IBinding variableBinding, Statement toVisit) {
        if (variableBinding != null) {
            final VariableUseCounterVisitor variableUseVisitor = new VariableUseCounterVisitor(variableBinding);
            toVisit.accept(variableUseVisitor);
            return variableUseVisitor.getUseCount();
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
            ITypeBinding componentType = arrayTypeBinding.getComponentType();
            ITypeBinding jucTypeBinding = findImplementedType(colTypeBinding, "java.util.Collection");
            ITypeBinding colTypeArgument = jucTypeBinding.getTypeArguments()[0];
            return componentType.isSubTypeCompatible(colTypeArgument);
        }
        return false;
    }

    private boolean replaceWithCollectionMethod(ForStatement node, ForLoopContent loopContent,
            String methodName, MethodInvocation colMI) {
        final Expression addArg0 = arg0(colMI);
        final MethodInvocation getMI = as(addArg0, MethodInvocation.class);
        if (isSameVariable(loopContent, getMI)) {
            return replaceWithCollectionMethod(node, methodName,
                    colMI.getExpression(), getMI.getExpression());
        }
        return VISIT_SUBTREE;
    }

    private boolean isSameVariable(ForLoopContent loopContent, final MethodInvocation getMI) {
        if (isMethod(getMI, "java.util.List", "get", "int")
                && getMI.getExpression() instanceof Name) {
            return isSameLocalVariable(arg0(getMI), loopContent.getLoopVariable());
        }
        return false;
    }

    private boolean replaceWithCollectionMethod(ASTNode toReplace, String methodName,
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
        return DO_NOT_VISIT_SUBTREE;
    }

    @Override
    public boolean visit(InfixExpression node) {
        final MethodInvocation leftMi = as(node.getLeftOperand(), MethodInvocation.class);
        final MethodInvocation rightMi = as(node.getRightOperand(), MethodInvocation.class);
        if (isMethod(leftMi, "java.util.Collection", "size")
                && isZero(node.getRightOperand())) {
            return replaceCollectionSize(node, leftMi);
        } else if (isMethod(rightMi, "java.util.Collection", "size")
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

    @Override
    public boolean visit(IfStatement node) {
        final PrefixExpression pe = as(node.getExpression(), PrefixExpression.class);
        if (hasOperator(pe, NOT)) {
            final MethodInvocation miContains = as(pe.getOperand(), MethodInvocation.class);
            return maybeReplaceSetContains(miContains, node.getThenStatement(), pe, false);
        } else {
            final MethodInvocation miContains = as(node.getExpression(), MethodInvocation.class);
            return maybeReplaceSetContains(miContains, node.getElseStatement(), miContains, true);
        }
    }

    private boolean maybeReplaceSetContains(
            MethodInvocation miContains, Statement stmt, Expression toReplace, boolean negate) {
        if (isMethod(miContains, "java.util.Set", "contains", "java.lang.Object")) {
            Statement firstStmt = getAsList(stmt, 0);
            MethodInvocation miAdd = asExpression(firstStmt, MethodInvocation.class);
            final ASTMatcher astMatcher = new ASTMatcher();
            if (isMethod(miAdd, "java.util.Set", "add", "java.lang.Object")
                    && match(astMatcher, miContains.getExpression(), miAdd.getExpression())
                    && match(astMatcher, arg0(miContains), arg0(miAdd))) {
                ASTBuilder b = this.ctx.getASTBuilder();
                Refactorings r = this.ctx.getRefactorings();
                r.replace(toReplace, negate ? b.negate(miAdd, COPY.MOVE) : b.move(miAdd));
                r.remove(firstStmt);
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    private Statement getAsList(Statement stmt, int index) {
        if (index < 0) {
            throw new IllegalArgumentException("A list index cannot be negative. Given: " + index);
        }
        final List<Statement> stmts = asList(stmt);
        if (stmts.size() > index) {
            return stmts.get(index);
        }
        return null;
    }
}

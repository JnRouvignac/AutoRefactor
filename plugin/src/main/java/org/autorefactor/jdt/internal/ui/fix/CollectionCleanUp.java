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
package org.autorefactor.jdt.internal.ui.fix;

import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.arg0;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.arguments;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.as;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.asExpression;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.getPreviousSibling;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.getUniqueFragment;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.hasOperator;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.hasType;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.isCastCompatible;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.isMethod;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.isPrimitive;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.isSameLocalVariable;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.match;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.PriorityQueue;
import java.util.TreeSet;
import java.util.Vector;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.concurrent.DelayQueue;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.PriorityBlockingQueue;

import org.autorefactor.jdt.internal.corext.dom.ASTBuilder;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/** See {@link #getDescription()} method. */
public class CollectionCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_CollectionCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_CollectionCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_CollectionCleanUp_reason;
    }

    @Override
    public boolean visit(Block node) {
        final NewAndAddAllMethodVisitor newAndAddAllMethodVisitor= new NewAndAddAllMethodVisitor(ctx, node);
        node.accept(newAndAddAllMethodVisitor);
        return newAndAddAllMethodVisitor.getResult();
    }

    private static final class NewAndAddAllMethodVisitor extends BlockSubVisitor {
        public NewAndAddAllMethodVisitor(final RefactoringContext ctx, final Block startNode) {
            super(ctx, startNode);
        }

        @Override
        public boolean visit(ExpressionStatement node) {
            final MethodInvocation mi= asExpression(node, MethodInvocation.class);
            if (isMethod(mi, Collection.class.getCanonicalName(), "addAll", Collection.class.getCanonicalName())) { //$NON-NLS-1$
                final Expression arg0= arg0(mi);
                final Statement previousStmt= getPreviousSibling(node);

                final Assignment as= asExpression(previousStmt, Assignment.class);
                if (hasOperator(as, Assignment.Operator.ASSIGN)) {
                    final Expression lhs= as.getLeftHandSide();
                    if (lhs instanceof SimpleName && isSameLocalVariable(lhs, mi.getExpression())) {
                        return replaceInitializer(as.getRightHandSide(), arg0, node);
                    }
                } else if (previousStmt instanceof VariableDeclarationStatement) {
                    final VariableDeclarationFragment vdf= getUniqueFragment(
                            (VariableDeclarationStatement) previousStmt);
                    if (vdf != null && isSameLocalVariable(vdf, mi.getExpression())) {
                        return replaceInitializer(vdf.getInitializer(), arg0, node);
                    }
                }
            }
            return true;
        }

        private boolean replaceInitializer(Expression nodeToReplace, final Expression arg0,
                ExpressionStatement nodeToRemove) {
            final ClassInstanceCreation cic= as(nodeToReplace, ClassInstanceCreation.class);
            if (canReplaceInitializer(cic, arg0) && isCastCompatible(nodeToReplace, arg0)) {
                final ASTBuilder b= ctx.getASTBuilder();
                ctx.getRefactorings().replace(nodeToReplace, b.new0(b.copy(cic.getType()), b.copy(arg0)));
                ctx.getRefactorings().remove(nodeToRemove);
                setResult(false);
                return false;
            }
            return true;
        }

        private boolean canReplaceInitializer(final ClassInstanceCreation cic, Expression sourceCollection) {
            if (cic == null) {
                return false;
            }
            final List<Expression> args= arguments(cic);
            final boolean noArgsCtor= args.isEmpty();
            final boolean colCapacityCtor= isValidCapacityParameter(sourceCollection, args);
            return (noArgsCtor && hasType(cic, "java.util.concurrent.ConcurrentLinkedDeque", //$NON-NLS-1$
                    ConcurrentLinkedQueue.class.getCanonicalName(), ConcurrentSkipListSet.class.getCanonicalName(),
                    CopyOnWriteArrayList.class.getCanonicalName(), CopyOnWriteArraySet.class.getCanonicalName(),
                    DelayQueue.class.getCanonicalName(), LinkedBlockingDeque.class.getCanonicalName(),
                    LinkedBlockingQueue.class.getCanonicalName(), "java.util.concurrent.LinkedTransferQueue", //$NON-NLS-1$
                    PriorityBlockingQueue.class.getCanonicalName(), ArrayDeque.class.getCanonicalName(), ArrayList.class.getCanonicalName(),
                    HashSet.class.getCanonicalName(), LinkedHashSet.class.getCanonicalName(), LinkedList.class.getCanonicalName(), PriorityQueue.class.getCanonicalName(),
                    TreeSet.class.getCanonicalName(), Vector.class.getCanonicalName()))
                    || (colCapacityCtor && hasType(cic, LinkedBlockingDeque.class.getCanonicalName(),
                            LinkedBlockingQueue.class.getCanonicalName(), PriorityBlockingQueue.class.getCanonicalName(),
                            ArrayDeque.class.getCanonicalName(), ArrayList.class.getCanonicalName(), HashSet.class.getCanonicalName(),
                            LinkedHashSet.class.getCanonicalName(), PriorityQueue.class.getCanonicalName(), Vector.class.getCanonicalName()));
        }

        private boolean isValidCapacityParameter(Expression sourceCollection, final List<Expression> args) {
            if (args.size() == 1 && isPrimitive(args.get(0), int.class.getSimpleName())) {
                final Object constant= args.get(0).resolveConstantExpressionValue();
                final MethodInvocation mi= as(args.get(0), MethodInvocation.class);
                if (constant != null) {
                    return constant.equals(0);
                } else {
                    return isMethod(mi, Collection.class.getCanonicalName(), "size") && match(mi.getExpression(), sourceCollection); //$NON-NLS-1$
                }
            }
            return false;
        }
    }
}

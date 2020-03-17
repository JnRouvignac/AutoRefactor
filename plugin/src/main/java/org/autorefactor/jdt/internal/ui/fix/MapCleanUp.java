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
package org.autorefactor.jdt.internal.ui.fix;

import java.util.HashMap;
import java.util.Hashtable;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.WeakHashMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentSkipListMap;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.refactoring.structure.CompilationUnitRewrite;
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
public class MapCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_MapCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_MapCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_MapCleanUp_reason;
    }

    @Override
    public boolean visit(final Block node) {
        NewAndPutAllMethodVisitor newAndPutAllMethodVisitor= new NewAndPutAllMethodVisitor(cuRewrite, node);
        node.accept(newAndPutAllMethodVisitor);
        return newAndPutAllMethodVisitor.getResult();
    }

    private static final class NewAndPutAllMethodVisitor extends BlockSubVisitor {
        public NewAndPutAllMethodVisitor(final CompilationUnitRewrite cuRewrite, final Block startNode) {
            super(cuRewrite, startNode);
        }

        @Override
        public boolean visit(final ExpressionStatement node) {
            MethodInvocation mi= ASTNodes.asExpression(node, MethodInvocation.class);

            if (getResult() && ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "putAll", Map.class.getCanonicalName())) { //$NON-NLS-1$
                Expression arg0= ASTNodes.arguments(mi).get(0);
                Statement previousStatement= ASTNodes.getPreviousSibling(node);

                Assignment as= ASTNodes.asExpression(previousStatement, Assignment.class);
                if (ASTNodes.hasOperator(as, Assignment.Operator.ASSIGN)) {
                    SimpleName lhs= ASTNodes.as(as.getLeftHandSide(), SimpleName.class);

                    if (lhs != null && ASTNodes.isSameLocalVariable(lhs, mi.getExpression())) {
                        return maybeReplaceInitializer(as.getRightHandSide(), arg0, node);
                    }
                } else if (previousStatement instanceof VariableDeclarationStatement) {
                    VariableDeclarationFragment vdf= ASTNodes.getUniqueFragment(
                            (VariableDeclarationStatement) previousStatement);
                    if (vdf != null && ASTNodes.isSameLocalVariable(vdf, mi.getExpression())) {
                        return maybeReplaceInitializer(vdf.getInitializer(), arg0, node);
                    }
                }
            }

            return true;
        }

        private boolean maybeReplaceInitializer(final Expression nodeToReplace, final Expression arg0,
                final ExpressionStatement nodeToRemove) {
            ClassInstanceCreation cic= ASTNodes.as(nodeToReplace, ClassInstanceCreation.class);
            if (canReplaceInitializer(cic, arg0) && ASTNodes.isCastCompatible(nodeToReplace, arg0)) {
                ASTNodeFactory b= cuRewrite.getASTBuilder();
                cuRewrite.getRefactorings().replace(nodeToReplace, b.new0(b.createMoveTarget(cic.getType()), b.createMoveTarget(arg0)));
                cuRewrite.getRefactorings().remove(nodeToRemove);
                setResult(false);
                return false;
            }

            return true;
        }

        private boolean canReplaceInitializer(final ClassInstanceCreation cic, final Expression sourceMap) {
            if (cic == null) {
                return false;
            }
            List<Expression> args= ASTNodes.arguments(cic);
            boolean noArgsCtor= args.isEmpty();
            boolean mapCapacityCtor= isValidCapacityParameter(sourceMap, args);
            return noArgsCtor && ASTNodes.hasType(cic, ConcurrentHashMap.class.getCanonicalName(),
                    ConcurrentSkipListMap.class.getCanonicalName(), Hashtable.class.getCanonicalName(), HashMap.class.getCanonicalName(),
                    IdentityHashMap.class.getCanonicalName(), LinkedHashMap.class.getCanonicalName(), TreeMap.class.getCanonicalName(),
                    WeakHashMap.class.getCanonicalName())
                    || mapCapacityCtor && ASTNodes.hasType(cic, ConcurrentHashMap.class.getCanonicalName(), Hashtable.class.getCanonicalName(),
                            HashMap.class.getCanonicalName(), IdentityHashMap.class.getCanonicalName(), LinkedHashMap.class.getCanonicalName(),
                            WeakHashMap.class.getCanonicalName());
        }

        private boolean isValidCapacityParameter(final Expression sourceMap, final List<Expression> args) {
            if (args.size() == 1 && ASTNodes.isPrimitive(args.get(0), int.class.getSimpleName())) {
                Object constant= args.get(0).resolveConstantExpressionValue();
                MethodInvocation mi= ASTNodes.as(args.get(0), MethodInvocation.class);
                if (constant != null) {
                    return constant.equals(0);
                }

                return ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "size") && ASTNodes.match(mi.getExpression(), sourceMap); //$NON-NLS-1$
            }

            return false;
        }
    }
}

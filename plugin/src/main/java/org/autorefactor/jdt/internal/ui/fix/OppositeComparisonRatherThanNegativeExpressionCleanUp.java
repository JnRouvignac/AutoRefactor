/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2019 Fabrice Tiercelin - initial API and implementation
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

import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.VISIT_SUBTREE;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.as;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.hasType;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.isMethod;

import org.autorefactor.jdt.internal.corext.dom.ASTBuilder;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression.Operator;

/** See {@link #getDescription()} method. */
public class OppositeComparisonRatherThanNegativeExpressionCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_OppositeComparisonRatherThanNegativeExpressionCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_OppositeComparisonRatherThanNegativeExpressionCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_OppositeComparisonRatherThanNegativeExpressionCleanUp_reason;
    }

    @Override
    public boolean visit(final PrefixExpression node) {
        if (Operator.MINUS.equals(node.getOperator())) {
            final MethodInvocation mi= as(node.getOperand(), MethodInvocation.class);

            if (mi != null && mi.getExpression() != null && mi.arguments().size() == 1) {
                final String[] classes= { "java.lang.Double", "java.lang.Float", "java.lang.Short", "java.lang.Integer", "java.lang.Long", "java.lang.Character", "java.lang.Byte", "java.lang.Boolean" };

                for (final String clazz : classes) {
                    if (isMethod(mi, clazz, "compareTo", clazz) && hasType((Expression) mi.arguments().get(0), clazz)) {
                        reverseObjects(node, mi);
                        return DO_NOT_VISIT_SUBTREE;
                    }
                }
            }
        }

        return VISIT_SUBTREE;
    }

    private void reverseObjects(final PrefixExpression node, final MethodInvocation mi) {
        final ASTBuilder b= ctx.getASTBuilder();
        final Refactorings r= ctx.getRefactorings();

        r.replace(node, b.invoke(b.parenthesizeIfNeeded(b.copy((Expression) mi.arguments().get(0))), "compareTo",
                b.copy(mi.getExpression())));
    }
}

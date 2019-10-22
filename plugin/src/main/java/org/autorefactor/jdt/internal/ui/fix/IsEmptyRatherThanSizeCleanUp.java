/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016-2017 Fabrice Tiercelin - Annoying remaining loop variable occurrence
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

import java.util.Collection;
import java.util.Map;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;

/** See {@link #getDescription()} method. */
public class IsEmptyRatherThanSizeCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_IsEmptyRatherThanSizeCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_IsEmptyRatherThanSizeCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_IsEmptyRatherThanSizeCleanUp_reason;
    }

    private int getJavaMinorVersion() {
        return ctx.getJavaProjectOptions().getJavaSERelease().getMinorVersion();
    }

    @Override
    public boolean visit(InfixExpression node) {
        final MethodInvocation leftMi= ASTNodes.as(node.getLeftOperand(), MethodInvocation.class);
        final Long rightLiteral= ASTNodes.integerLiteral(node.getRightOperand());

        if (!maybeReplaceCollectionSize(node, leftMi, node.getOperator(),
                rightLiteral)) {
            return false;
        }

        final MethodInvocation rightMi= ASTNodes.as(node.getRightOperand(), MethodInvocation.class);
        final Long leftLiteral= ASTNodes.integerLiteral(node.getLeftOperand());

        return maybeReplaceCollectionSize(node, rightMi, sign(node.getOperator()), leftLiteral);
    }

    private boolean maybeReplaceCollectionSize(final InfixExpression node, final MethodInvocation miToReplace,
            final InfixExpression.Operator operator, final Long literalSize) {
        if ((ASTNodes.usesGivenSignature(miToReplace, Collection.class.getCanonicalName(), "size") || ASTNodes.usesGivenSignature(miToReplace, Map.class.getCanonicalName(), "size") //$NON-NLS-1$ $NON-NLS-2$
                || (ASTNodes.usesGivenSignature(miToReplace, String.class.getCanonicalName(), "length") && getJavaMinorVersion() >= 6)) //$NON-NLS-1$
                && literalSize != null) {
            final Refactorings r= this.ctx.getRefactorings();
            final ASTNodeFactory b= this.ctx.getASTBuilder();

            if (literalSize == 0) {
                if (InfixExpression.Operator.GREATER_EQUALS.equals(operator)) {
                    r.replace(node, b.boolean0(true));
                    return false;
                }
                if (InfixExpression.Operator.LESS.equals(operator)) {
                    r.replace(node, b.boolean0(false));
                    return false;
                }
                if (InfixExpression.Operator.GREATER.equals(operator)) {
                    r.replace(node, b.not(b.invoke(b.copyExpression(miToReplace), "isEmpty"))); //$NON-NLS-1$
                    return false;
                }
                if (InfixExpression.Operator.EQUALS.equals(operator)) {
                    r.replace(node, b.invoke(b.copyExpression(miToReplace), "isEmpty")); //$NON-NLS-1$
                    return false;
                }
                if (InfixExpression.Operator.NOT_EQUALS.equals(operator)) {
                    r.replace(node, b.not(b.invoke(b.copyExpression(miToReplace), "isEmpty"))); //$NON-NLS-1$
                    return false;
                }
                if (InfixExpression.Operator.LESS_EQUALS.equals(operator)) {
                    r.replace(node, b.invoke(b.copyExpression(miToReplace), "isEmpty")); //$NON-NLS-1$
                    return false;
                }
            } else if (literalSize == 1) {
                if (InfixExpression.Operator.GREATER_EQUALS.equals(operator)) {
                    r.replace(node, b.not(b.invoke(b.copyExpression(miToReplace), "isEmpty"))); //$NON-NLS-1$
                    return false;
                }
                if (InfixExpression.Operator.LESS.equals(operator)) {
                    r.replace(node, b.invoke(b.copyExpression(miToReplace), "isEmpty")); //$NON-NLS-1$
                    return false;
                }
            }
        }

        return true;
    }

    private InfixExpression.Operator sign(final InfixExpression.Operator operator) {
        if (InfixExpression.Operator.LESS.equals(operator)) {
            return InfixExpression.Operator.GREATER;
        }
        if (InfixExpression.Operator.LESS_EQUALS.equals(operator)) {
            return InfixExpression.Operator.GREATER_EQUALS;
        }
        if (InfixExpression.Operator.GREATER.equals(operator)) {
            return InfixExpression.Operator.LESS;
        }
        if (InfixExpression.Operator.GREATER_EQUALS.equals(operator)) {
            return InfixExpression.Operator.LESS_EQUALS;
        }
        return operator;
    }
}

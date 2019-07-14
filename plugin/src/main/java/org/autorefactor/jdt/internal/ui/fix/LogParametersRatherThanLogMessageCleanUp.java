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
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.arguments;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.hasType;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.isMethod;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.ASTBuilder;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.StringLiteral;

/** See {@link #getDescription()} method. */
public class LogParametersRatherThanLogMessageCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_LogParametersRatherThanLogMessageCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_LogParametersRatherThanLogMessageCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_LogParametersRatherThanLogMessageCleanUp_reason;
    }

    @Override
    public boolean visit(final MethodInvocation node) {
        return maybeRefactorMethod(node, "debug")
                && maybeRefactorMethod(node, "error")
                && maybeRefactorMethod(node, "info")
                && maybeRefactorMethod(node, "trace")
                && maybeRefactorMethod(node, "warn");
    }

    private boolean maybeRefactorMethod(final MethodInvocation node, final String methodName) {
        if (isMethod(node, "org.slf4j.Logger", methodName, "java.lang.String")
                || isMethod(node, "ch.qos.logback.classic.Logger", methodName, "java.lang.String")) {
            final List<Expression> args = arguments(node);

            if (args != null && args.size() == 1) {
                final Expression message = args.get(0);

                if (message instanceof InfixExpression) {
                    return maybeReplaceConcatenation(node, methodName, message);
                }
            }
        }

        return VISIT_SUBTREE;
    }

    @SuppressWarnings("unchecked")
    private boolean maybeReplaceConcatenation(final MethodInvocation node, final String methodName,
            final Expression message) {
        final ASTBuilder b = this.ctx.getASTBuilder();

        final InfixExpression concatenation = (InfixExpression) message;
        final List<Expression> strings = new ArrayList<Expression>();
        strings.add(concatenation.getLeftOperand());
        strings.add(concatenation.getRightOperand());

        if (concatenation.hasExtendedOperands()) {
            strings.addAll(concatenation.extendedOperands());
        }

        final StringBuilder messageBuilder = new StringBuilder();
        final List<Expression> params = new LinkedList<Expression>();
        boolean hasLiteral = false;
        boolean hasObjects = false;

        for (final Expression string : strings) {
            if (string instanceof StringLiteral) {
                hasLiteral = true;
                final String literal = (String) string.resolveConstantExpressionValue();

                if ((literal != null) && (literal.contains("{") || literal.contains("}"))) {
                    return VISIT_SUBTREE;
                }

                messageBuilder.append(literal);
            } else {
                hasObjects = true;
                messageBuilder.append("{}");

                if (hasType(string, "java.lang.Throwable")) {
                    params.add(b.invoke("String", "valueOf", b.copy(string)));
                } else {
                    params.add(b.copy(string));
                }
            }
        }

        if (hasLiteral && hasObjects) {
            replaceConcatenation(node, methodName, b, messageBuilder, params);
            return DO_NOT_VISIT_SUBTREE;
        }

        return VISIT_SUBTREE;
    }

    private void replaceConcatenation(final MethodInvocation node, final String methodName, final ASTBuilder b,
            final StringBuilder messageBuilder, final List<Expression> params) {
        params.add(0, b.string(messageBuilder.toString()));

        final Refactorings r = this.ctx.getRefactorings();
        r.replace(node, b.invoke(b.copy(node.getExpression()), methodName,
                params));
    }
}

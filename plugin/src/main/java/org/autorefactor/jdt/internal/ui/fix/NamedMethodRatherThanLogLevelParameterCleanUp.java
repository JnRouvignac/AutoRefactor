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
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.isField;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.isMethod;

import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.ASTBuilder;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.QualifiedName;

/** See {@link #getDescription()} method. */
public class NamedMethodRatherThanLogLevelParameterCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_NamedMethodRatherThanLogLevelParameterCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_NamedMethodRatherThanLogLevelParameterCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_NamedMethodRatherThanLogLevelParameterCleanUp_reason;
    }

    @Override
    public boolean visit(final MethodInvocation node) {
        if (isMethod(node, "java.util.logging.Logger", "log", "java.util.logging.Level", "java.lang.String")) {
            final List<Expression> args = arguments(node);

            if (args != null && args.size() == 2) {
                final Expression level = args.get(0);
                final Expression message = args.get(1);

                if (level instanceof QualifiedName) {
                    final QualifiedName levelType = (QualifiedName) level;
                    final String methodName;

                    if (isField(levelType, "java.util.logging.Level", "SEVERE")) {
                        methodName = "severe";
                    } else if (isField(levelType, "java.util.logging.Level", "WARNING")) {
                        methodName = "warning";
                    } else if (isField(levelType, "java.util.logging.Level", "INFO")) {
                        methodName = "info";
                    } else if (isField(levelType, "java.util.logging.Level", "FINE")) {
                        methodName = "fine";
                    } else if (isField(levelType, "java.util.logging.Level", "FINER")) {
                        methodName = "finer";
                    } else if (isField(levelType, "java.util.logging.Level", "FINEST")) {
                        methodName = "finest";
                    } else {
                        return VISIT_SUBTREE;
                    }

                    replaceLevelByMethodName(node, methodName, message);
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
        }

        return VISIT_SUBTREE;
    }

    private void replaceLevelByMethodName(final MethodInvocation node, final String methodName,
            final Expression message) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        final Refactorings r = this.ctx.getRefactorings();

        r.replace(node, b.invoke(b.copy(node.getExpression()), methodName, b.copy(message)));
    }
}

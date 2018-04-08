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
import static org.autorefactor.refactoring.ASTHelper.arguments;
import static org.autorefactor.refactoring.ASTHelper.as;
import static org.autorefactor.refactoring.ASTHelper.isMethod;

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.dom.BooleanLiteral;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;

/** See {@link #getDescription()} method. */
public class BooleanConstantRatherThanValueOfRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Boolean constant rather than valueOf()";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Replace Boolean.valueOf(true) and Boolean.valueOf(false) by Boolean.TRUE and Boolean.FALSE.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves the readibility. It also improves the time performance.";
    }

    @Override
    public boolean visit(MethodInvocation node) {
        if (isMethod(node, "java.lang.Boolean", "valueOf", "java.lang.String")
                || isMethod(node, "java.lang.Boolean", "valueOf", "boolean")) {
            final BooleanLiteral l = as(arguments(node), BooleanLiteral.class);
            if (l != null) {
                ctx.getRefactorings().replace(node,
                        toFieldAccess(node, l.booleanValue()));
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    private FieldAccess toFieldAccess(final MethodInvocation node, final boolean booleanLiteral) {
        final ASTBuilder b = ctx.getASTBuilder();
        final FieldAccess fa = b.getAST().newFieldAccess();
        if (node.getExpression() instanceof Name) {
            fa.setExpression(b.copy(node.getExpression()));
        }
        fa.setName(b.simpleName(booleanLiteral ? "TRUE" : "FALSE"));
        return fa;
    }
}

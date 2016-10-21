/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Zsombor Gegesy - initial API and implementation
 * Copyright (C) 2016 Jean-Noël Rouvignac - code cleanups
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

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.MethodInvocation;

import static org.autorefactor.refactoring.ASTHelper.*;

/** See {@link #getDescription()} method. */
@SuppressWarnings("javadoc")
public class MapEliminateKeySetCallsRefactoring extends AbstractRefactoringRule {

    @Override
    public String getDescription() {
        return ""
                + "Directly invoke methods on Map rather than on Map.keySet() where possible\n";
    }

    @Override
    public String getName() {
        return "Replace useless calls to Map.keySet() when direct calls to the Map are possible";
    }

    @Override
    public boolean visit(MethodInvocation mi) {
        Expression miExpr = mi.getExpression();
        if (miExpr instanceof MethodInvocation) {
            MethodInvocation parentMi = (MethodInvocation) miExpr;
            if (isMethod(parentMi, "java.util.Map", "keySet")) {
                if (isMethod(mi, "java.util.Set", "clear")) {
                    removeInvocationOfMapKeySet(parentMi, mi, "clear");
                    return DO_NOT_VISIT_SUBTREE;
                }
                if (isMethod(mi, "java.util.Set", "size")) {
                    removeInvocationOfMapKeySet(parentMi, mi, "size");
                    return DO_NOT_VISIT_SUBTREE;
                }
                if (isMethod(mi, "java.util.Set", "isEmpty")) {
                    removeInvocationOfMapKeySet(parentMi, mi, "isEmpty");
                    return DO_NOT_VISIT_SUBTREE;
                }
                if (isMethod(mi, "java.util.Set", "remove", "java.lang.Object")) {
                    removeInvocationOfMapKeySet(parentMi, mi, "remove");
                    return DO_NOT_VISIT_SUBTREE;
                }
                if (isMethod(mi, "java.util.Set", "contains", "java.lang.Object")) {
                    removeInvocationOfMapKeySet(parentMi, mi, "containsKey");
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private void removeInvocationOfMapKeySet(
            MethodInvocation mapKeySetMi, MethodInvocation actualMi, String methodName) {
        final ASTBuilder b = ctx.getASTBuilder();
        ctx.getRefactorings().replace(
                actualMi,
                b.invoke(b.copyExpression(mapKeySetMi), methodName, b.copyRange(arguments(actualMi))));
    }
}

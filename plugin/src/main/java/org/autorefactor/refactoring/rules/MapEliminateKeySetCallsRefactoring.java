/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Zsombor Gegesy
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

import static org.autorefactor.refactoring.ASTHelper.isMethod;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.ASTHelper;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.MethodInvocation;

/** See {@link #getDescription()} method. */
@SuppressWarnings("javadoc")
public class MapEliminateKeySetCallsRefactoring extends AbstractRefactoringRule {

    @Override
    public String getDescription() {
        return ""
                + "Map related refactorings:\n"
                + "- replaces calling methods on the keySet, which are just delegate methods on the underlying Map\n";
    }

    @Override
    public String getName() {
        return "MapKeySet";
    }

    @Override
    public boolean visit(MethodInvocation mi) {
        Expression otherExpression = mi.getExpression();
        if (otherExpression instanceof MethodInvocation) {
            MethodInvocation parentMethodInv = (MethodInvocation) otherExpression;
            if (isMethod(parentMethodInv, "java.util.Map", "keySet")) {
                if (isMethod(mi, "java.util.Set", "clear")) {
                    replace(parentMethodInv, mi, "clear");
                    return ASTHelper.DO_NOT_VISIT_SUBTREE;
                }
                if (isMethod(mi, "java.util.Set", "size")) {
                    replace(parentMethodInv, mi, "size");
                    return ASTHelper.DO_NOT_VISIT_SUBTREE;
                }
                if (isMethod(mi, "java.util.Set", "isEmpty")) {
                    replace(parentMethodInv, mi, "isEmpty");
                    return ASTHelper.DO_NOT_VISIT_SUBTREE;
                }
                if (isMethod(mi, "java.util.Set", "remove", "java.lang.Object")) {
                    replace(parentMethodInv, mi, "remove");
                    return ASTHelper.DO_NOT_VISIT_SUBTREE;
                }
                if (isMethod(mi, "java.util.Set", "contains", "java.lang.Object")) {
                    replace(parentMethodInv, mi, "containsKey");
                    return ASTHelper.DO_NOT_VISIT_SUBTREE;
                }
            }

        }
        return ASTHelper.VISIT_SUBTREE;
    }

    private void replace(MethodInvocation first, MethodInvocation last, String name) {
        final Refactorings r = this.ctx.getRefactorings();
        final ASTBuilder b = this.ctx.getASTBuilder();

        r.replace(last, b.invoke(b.copyExpression(first), name, b.copyList(last.arguments())));
    }

}

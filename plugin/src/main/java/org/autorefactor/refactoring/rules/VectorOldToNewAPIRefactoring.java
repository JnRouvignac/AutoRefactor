/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2016 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.refactoring.rules;

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.arguments;
import static org.autorefactor.refactoring.ASTHelper.hasType;
import static org.autorefactor.refactoring.ASTHelper.isMethod;
import static org.eclipse.jdt.core.dom.MethodInvocation.NAME_PROPERTY;

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.Release;
import org.autorefactor.util.IllegalArgumentException;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.MethodInvocation;

/** See {@link #getDescription()} method. */
public class VectorOldToNewAPIRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Collections APIs rather than Vector pre-Collections APIs";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Replaces Vector pre-Collections APIs with equivalent Collections APIs.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It makes the code more standard.";
    }

    @Override
    public boolean visit(MethodInvocation node) {
        if (ctx.getJavaProjectOptions().getJavaSERelease().isCompatibleWith(Release.javaSE("1.2.0"))) {
            if (isMethod(node, "java.util.Vector", "elementAt", "int")) {
                replaceWith(node, "get");
            } else if (isMethod(node, "java.util.Vector", "addElement", "java.lang.Object")) {
                replaceWith(node, "add");
            } else if (isMethod(node, "java.util.Vector", "insertElementAt", "java.lang.Object", "int")) {
                replaceWithAndSwapArguments(node, "add");
            } else if (isMethod(node, "java.util.Vector", "copyInto", "java.lang.Object[]")) {
                replaceWith(node, "toArray");
            } else if (isMethod(node, "java.util.Vector", "removeAllElements")) {
                replaceWith(node, "clear");
            } else if (isMethod(node, "java.util.Vector", "removeElement", "java.lang.Object")) {
                replaceWithSpecial(node, "remove");
            } else if (isMethod(node, "java.util.Vector", "removeElementAt", "int")) {
                replaceWith(node, "remove");
            } else if (isMethod(node, "java.util.Vector", "setElementAt", "java.lang.Object", "int")) {
                replaceWithAndSwapArguments(node, "set");
            } else {
                return VISIT_SUBTREE;
            }
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private void replaceWith(final MethodInvocation node, final String newMethodName) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        ctx.getRefactorings().set(node, NAME_PROPERTY, b.simpleName(newMethodName));
    }

    private void replaceWithSpecial(final MethodInvocation node, final String newMethodName) {
        final List<Expression> args = arguments(node);
        assertSize(args, 1);
        final Expression arg0 = args.get(0);

        final ASTBuilder b = this.ctx.getASTBuilder();
        final Refactorings r = this.ctx.getRefactorings();
        r.set(node, NAME_PROPERTY, b.simpleName(newMethodName));
        if (hasType(arg0, "int", "short", "byte")) {
            r.replace(arg0, b.cast(b.type("Object"), b.move(arg0)));
        }
    }

    private void replaceWithAndSwapArguments(final MethodInvocation node, final String newMethodName) {
        final List<Expression> args = arguments(node);
        assertSize(args, 2);
        final Expression arg1 = args.get(1);

        final ASTBuilder b = ctx.getASTBuilder();
        final Refactorings r = ctx.getRefactorings();
        r.set(node, NAME_PROPERTY, b.simpleName(newMethodName));
        r.moveToIndex(arg1, 0, b.move(arg1));
    }

    private void assertSize(final List<Expression> args, final int expectedSize) {
        if (args == null) {
            throw new IllegalArgumentException(null, "Expected " + args + "to not be null");
        }
        if (args.size() != expectedSize) {
            final Expression node = !args.isEmpty() ? args.get(0) : null;
            throw new IllegalArgumentException(node,
                    "Expected " + args
                    + " to have size <" + expectedSize + ">, but found <"
                    + args.size() + ">");
        }
    }
}

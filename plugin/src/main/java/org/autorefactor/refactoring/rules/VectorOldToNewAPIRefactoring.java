/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2014 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.Release;
import org.autorefactor.util.IllegalArgumentException;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.MethodInvocation;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.MethodInvocation.*;

/**
 * Replaces Vector pre Collections API with equivalent Collections API.
 */
public class VectorOldToNewAPIRefactoring extends AbstractRefactoring {

    /** {@inheritDoc} */
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
                replaceWith(node, "set");
            }
        }
        return VISIT_SUBTREE;
    }

    private void replaceWith(MethodInvocation node, String newMethodName) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        this.ctx.getRefactorings().replace(node,
            b.invoke(
                b.copy(node.getExpression()),
                newMethodName,
                b.copyRange(arguments(node))));
    }

    private void replaceWithSpecial(MethodInvocation node, String newMethodName) {
        final List<Expression> args = arguments(node);
        assertSize(args, 1);
        final Expression arg0 = args.get(0);

        final ASTBuilder b = this.ctx.getASTBuilder();
        final Refactorings r = this.ctx.getRefactorings();
        r.set(node, NAME_PROPERTY, b.simpleName(newMethodName));
        if (hasType(arg0, "int", "short", "byte")) {
            r.replace(arg0, b.cast("Object", b.move(arg0)));
        }
    }

    private void replaceWithAndSwapArguments(MethodInvocation node, String newMethodName) {
        final List<Expression> args = arguments(node);
        assertSize(args, 2);
        final Expression arg1 = args.get(1);

        final ASTBuilder b = this.ctx.getASTBuilder();
        final Refactorings r = this.ctx.getRefactorings();
        r.set(node, NAME_PROPERTY, b.simpleName(newMethodName));
        r.insertAt(b.move(arg1), 0, arg1.getLocationInParent(), arg1.getParent());
    }

    private void assertSize(final List<Expression> args, int expectedSize) {
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

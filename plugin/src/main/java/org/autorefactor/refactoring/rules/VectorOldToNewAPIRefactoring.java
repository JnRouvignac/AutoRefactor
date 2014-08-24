/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013 Jean-Noël Rouvignac - initial API and implementation
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
import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.Release;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.MethodInvocation;

import static org.autorefactor.refactoring.ASTHelper.*;

/**
 * Replaces Vector pre Collections API with equivalent Collections API.
 */
public class VectorOldToNewAPIRefactoring extends ASTVisitor implements
        IJavaRefactoring {

    private RefactoringContext ctx;

    /** {@inheritDoc} */
    public void setRefactoringContext(RefactoringContext ctx) {
        this.ctx = ctx;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(MethodInvocation node) {
        if (this.ctx.getJavaSERelease().isCompatibleWith(
                Release.javaSE("1.2.0"))) {
            if (isMethod(node, "java.util.Vector", "elementAt", "int")) {
                replaceWith(node, "get");
            } else if (isMethod(node, "java.util.Vector", "addElement",
                    "java.lang.Object")) {
                replaceWith(node, "add");
            } else if (isMethod(node, "java.util.Vector", "insertElementAt",
                    "java.lang.Object", "int")) {
                replaceWithAndSwapArguments(node, "add");
            } else if (isMethod(node, "java.util.Vector", "copyInto",
                    "java.lang.Object[]")) {
                replaceWith(node, "toArray");
            } else if (isMethod(node, "java.util.Vector", "removeAllElements")) {
                replaceWith(node, "clear");
            } else if (isMethod(node, "java.util.Vector", "removeElement",
                    "java.lang.Object")) {
                replaceWithSpecial(node, "remove");
            } else if (isMethod(node, "java.util.Vector", "removeElementAt", "int")) {
                replaceWith(node, "remove");
            } else if (isMethod(node, "java.util.Vector", "setElementAt",
                    "java.lang.Object", "int")) {
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
                b.copyAll(arguments(node))));
    }

    private void replaceWithSpecial(MethodInvocation node, String newMethodName) {
        AST ast = this.ctx.getAST();
        MethodInvocation mi = ast.newMethodInvocation();
        mi.setName(ast.newSimpleName(newMethodName));
        mi.setExpression(copySubtree(ast, node.getExpression()));
        final List<Expression> args = arguments(node);
        assertSize(args, 1);
        if (hasType(args.get(0), "int", "short", "byte")) {
            final CastExpression ce = ast.newCastExpression();
            ce.setType(ast.newSimpleType(ast.newSimpleName("Object")));
            ce.setExpression(copySubtree(ast, args.get(0)));
            arguments(mi).add(ce);
        } else {
            arguments(mi).add(copySubtree(ast, args.get(0)));
        }
        this.ctx.getRefactorings().replace(node, mi);
    }

    private void replaceWithAndSwapArguments(MethodInvocation node,
            String newMethodName) {
        AST ast = this.ctx.getAST();
        MethodInvocation mi = ast.newMethodInvocation();
        mi.setName(ast.newSimpleName(newMethodName));
        mi.setExpression(copySubtree(ast, node.getExpression()));
        final List<Expression> args = arguments(node);
        assertSize(args, 2);
        arguments(mi).add(copySubtree(ast, args.get(1)));
        arguments(mi).add(copySubtree(ast, args.get(0)));
        this.ctx.getRefactorings().replace(node, mi);
    }

    private void assertSize(final List<Expression> args, int expectedSize) {
        if (args == null) {
            throw new IllegalArgumentException("Expected " + args
                    + "to not be null");
        }
        if (args.size() != expectedSize) {
            throw new IllegalArgumentException("Expected " + args
                    + " to have size <" + expectedSize + ">, but found <"
                    + args.size() + ">");
        }
    }

    /** {@inheritDoc} */
    public Refactorings getRefactorings(CompilationUnit astRoot) {
        astRoot.accept(this);
        return this.ctx.getRefactorings();
    }

}

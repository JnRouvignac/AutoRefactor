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

import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.*;

import static org.autorefactor.refactoring.ASTHelper.*;

/**
 * Removes:
 * <ul>
 * <li>Creating a {@link String} instance from a {@link String} constant or
 * literal.</li>
 * <li>Calling {@link String#toString()} on a {@link String} instance</li>
 * </ul>
 *
 * @author jnrouvignac
 */
public class StringRefactoring extends ASTVisitor implements IJavaRefactoring {

    private RefactoringContext ctx;

    public StringRefactoring() {
        super();
    }

    /** {@inheritDoc} */
    public void setRefactoringContext(RefactoringContext ctx) {
        this.ctx = ctx;
    }

    // TODO JNR remove calls to toString() inside string concatenation

    /** {@inheritDoc} */
    @Override
    public boolean visit(ClassInstanceCreation node) {
        final ITypeBinding typeBinding = node.getType().resolveBinding();
        if (typeBinding != null
                && "java.lang.String".equals(typeBinding.getQualifiedName())
                && arguments(node).size() == 1) {
            final Expression arg0 = arguments(node).get(0);
            if (arg0.resolveConstantExpressionValue() != null) {
                this.ctx.getRefactorings().replace(node,
                        copySubtree(this.ctx.getAST(), arg0));
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(MethodInvocation node) {
        final Expression expression = node.getExpression();
        if (expression != null
                && "toString".equals(node.getName().getIdentifier())
                && arguments(node).isEmpty()
                && canRemoveToStringMethodCall(node, expression)) {
            this.ctx.getRefactorings().replace(node,
                    copySubtree(this.ctx.getAST(), expression));
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private boolean canRemoveToStringMethodCall(MethodInvocation node,
            final Expression expression) {
        if (hasType(resolveTypeBindingForcedFromContext(node), "java.lang.String")) {
            // We are in a String context, no need to call toString()
            return true;
        } else if (hasType(expression, "java.lang.String")) {
            // It's already a String, no need to call toString()
            return true;
        }
        return false;
    }

    /** {@inheritDoc} */
    public Refactorings getRefactorings(CompilationUnit astRoot) {
        astRoot.accept(this);
        return this.ctx.getRefactorings();
    }
}

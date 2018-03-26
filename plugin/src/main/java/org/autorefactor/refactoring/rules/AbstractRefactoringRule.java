/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2017 Jean-Noël Rouvignac - initial API and implementation
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

import org.autorefactor.preferences.Preferences;
import org.autorefactor.refactoring.JavaRefactoringRule;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.Release;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.CompilationUnit;

/**
 * Abstract class to extend when writing refactoring rules as {@link ASTVisitor}s.
 * It centralizes useful features for refactoring rules.
 */
public abstract class AbstractRefactoringRule extends ASTVisitor implements JavaRefactoringRule {
    /** The refactoring context of the current visitor. */
    protected RefactoringContext ctx;

    /**
     * True if it is the visitor by default.
     *
     * @return true if it is the visitor by default.
     */
    public boolean isByDefault() {
        return true;
    }

    /**
     * True if the visitor is enabled.
     *
     * @param preferences The preferences
     *
     * @return true if the visitor is enabled.
     */
    public boolean isEnabled(Preferences preferences) {
        return preferences.isEnabled(getClass());
    }

    /**
     * True if this Java version is supported.
     *
     * @param javaSeRelease The javaSe release
     *
     * @return true if this Java version is supported.
     */
    public boolean isJavaVersionSupported(Release javaSeRelease) {
        return true;
    }

    /**
     * Set the refactoring context.
     *
     * @param ctx the refactoring context.
     */
    public void setRefactoringContext(RefactoringContext ctx) {
        this.ctx = ctx;
    }

    @Override
    public boolean preVisit2(ASTNode node) {
        // only visit nodes that have not been refactored
        // to avoid trying to refactor twice the same node (or sub nodes)
        return !ctx.getRefactorings().hasBeenRefactored(node);
    }

    /**
     * Get the refactorings.
     *
     * @param astRoot The AST toot
     *
     * @return the refactorings.
     */
    public Refactorings getRefactorings(CompilationUnit astRoot) {
        astRoot.accept(this);
        return ctx.getRefactorings();
    }
}

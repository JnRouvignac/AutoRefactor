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
package org.autorefactor.jdt.internal.ui.fix;

import org.autorefactor.jdt.internal.corext.dom.InterruptibleVisitor;
import org.autorefactor.jdt.internal.corext.dom.JavaRefactoringRule;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.autorefactor.jdt.internal.corext.refactoring.structure.CompilationUnitRewrite;
import org.autorefactor.preferences.Preferences;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.QualifiedName;

/**
 * Abstract class to extend when writing cleanup rules as
 * {@link ASTVisitor}s. It centralizes useful features for cleanup rules.
 */
public abstract class AbstractCleanUpRule extends ASTVisitor implements JavaRefactoringRule {
    private static final class LombokVisitor extends InterruptibleVisitor {
        private boolean useLombok;

        @Override
        public boolean visit(final QualifiedName node) {
            if (node.getFullyQualifiedName().contains("lombok")) { //$NON-NLS-1$
                useLombok= true;
                return interruptVisit();
            }

            return true;
        }

        /**
         * @return the useLombok
         */
        public boolean isUseLombok() {
            return useLombok;
        }
    }

    /** The refactoring context of the current visitor. */
    protected CompilationUnitRewrite cuRewrite;

    /**
     * True if it is the visitor by default.
     *
     * @return true if it is the visitor by default.
     */
    @Override
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
    @Override
    public boolean isEnabled(final Preferences preferences) {
        return preferences.isEnabled(getClass());
    }

    /**
     * True if this Java version is supported.
     *
     * @param javaSeRelease The javaSe release
     *
     * @return true if this Java version is supported.
     */
    @Override
    public boolean isJavaVersionSupported(final Release javaSeRelease) {
        return true;
    }

    /**
     * Get the java minor version.
     *
     * @return the java minor version.
     */
    public int getJavaMinorVersion() {
        return cuRewrite.getJavaProjectOptions().getJavaSERelease().getMinorVersion();
    }

    /**
     * Set the cleanup context.
     *
     * @param cuRewrite the cleanup context.
     */
    @Override
    public void setRefactoringContext(final CompilationUnitRewrite cuRewrite) {
        this.cuRewrite= cuRewrite;
    }

    @Override
    public boolean preVisit2(final ASTNode node) {
        if (node instanceof CompilationUnit) {
            LombokVisitor lombokVisitor= new LombokVisitor();
            lombokVisitor.visitNode(node);

            if (lombokVisitor.isUseLombok()) {
                return false;
            }
        }
        // Only visit nodes that have not been refactored
        // to avoid trying to refactor twice the same node (or sub nodes)
        return !cuRewrite.getRefactorings().hasBeenRefactored(node);
    }

    /**
     * Get the cleanups.
     *
     * @param astRoot The AST toot
     *
     * @return the cleanups.
     */
    @Override
    public Refactorings getRefactorings(final CompilationUnit astRoot) {
        astRoot.accept(this);
        return cuRewrite.getRefactorings();
    }
}

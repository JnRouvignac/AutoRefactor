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
package org.autorefactor.refactoring.rules;

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;

import org.autorefactor.refactoring.InterruptibleVisitor;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.SimpleType;
import org.eclipse.jdt.core.dom.TypeDeclaration;

/**
 * Handle the need to add an import for a class.
 */
public abstract class NewClassImportRefactoring extends AbstractRefactoringRule {

    /**
     * The class that does the refactoring when an import needs to be added.
     */
    public abstract class RefactoringWithNewClassImport extends ASTVisitor {
        /**
         * True if an import needs to be added.
         */
        private boolean isImportToBeAdd = false;

        /**
         * True if an import needs to be added.
         *
         * @return True if an import needs to be added
         */
        public boolean isImportToBeAdd() {
            return isImportToBeAdd;
        }

        /**
         * Set true if an import needs to be added.
         *
         * @param isImportToBeUsed True if an import needs to be added
         */
        public void setImportToBeAdd(boolean isImportToBeUsed) {
            this.isImportToBeAdd = isImportToBeUsed;
        }
    }

    private class LocalClassVisitor extends InterruptibleVisitor {
        private boolean isClassnameLocallyUsed;

        public boolean isClassnameLocallyUsed() {
            return isClassnameLocallyUsed;
        }

        @Override
        public boolean visit(TypeDeclaration nestedClass) {

            if (nestedClass.getName().getIdentifier().equals(getClassNameToImport())) {
                isClassnameLocallyUsed = true;
                return interruptVisit();
            }

            return VISIT_SUBTREE;
        }

        @Override
        public boolean visit(SimpleType simpleName) {
            if (simpleName.getName().isSimpleName()
                    && hasSimpleName(simpleName.getName(), getClassNameToImport())) {
                isClassnameLocallyUsed = true;
                return interruptVisit();
            }

            return VISIT_SUBTREE;
        }
    }

    private boolean hasSimpleName(final Name aName, final String simpleNameToSearch) {
        final boolean matches = aName.getFullyQualifiedName()
                .matches("^(.*\\.)?" + simpleNameToSearch + "$");
        return matches;
    }

    /**
     * True if an import already exists for a class.
     *
     * @param node One node in the class file
     * @return True if an import already exists for a class.
     */
    public boolean isAlreadyImported(final ASTNode node) {
        final CompilationUnit cu = (CompilationUnit) node.getRoot();
        final String fullyQualifiedName = getPackageNameToImport() + "." + getClassNameToImport();

        for (Object anObject : cu.imports()) {
            ImportDeclaration anImport = (ImportDeclaration) anObject;

            if (anImport.isOnDemand()) {
                if (getPackageNameToImport().equals(anImport.getName().getFullyQualifiedName())) {
                    return true;
                }
            } else if (fullyQualifiedName.equals(anImport.getName().getFullyQualifiedName())) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean visit(final CompilationUnit node) {
        boolean isAlreadyImported = false;
        boolean canClassBeImported = true;
        final String fullyQualifiedName = getPackageNameToImport() + "." + getClassNameToImport();

        for (Object anObject : node.imports()) {
            ImportDeclaration anImport = (ImportDeclaration) anObject;

            if (!anImport.isStatic()) {
                if (anImport.isOnDemand()) {
                    if (getPackageNameToImport().equals(anImport.getName().getFullyQualifiedName())) {
                        isAlreadyImported = true;
                    }
                } else if (fullyQualifiedName.equals(anImport.getName().getFullyQualifiedName())) {
                    isAlreadyImported = true;
                    canClassBeImported = false;
                } else if (hasSimpleName(anImport.getName(), getClassNameToImport())) {
                    canClassBeImported = false;
                }
            }
        }

        if (canClassBeImported) {
            LocalClassVisitor nestedClassVisitor = new LocalClassVisitor();
            nestedClassVisitor.visitNode(node);

            canClassBeImported = !nestedClassVisitor.isClassnameLocallyUsed();
        }

        if (!isAlreadyImported && canClassBeImported) {
            final RefactoringWithNewClassImport refactoringClass = getRefactoringClassInstance();
            node.accept(refactoringClass);

            if (refactoringClass.isImportToBeAdd()) {
                final Refactorings r = ctx.getRefactorings();

                r.getImportRewrite().addImport(fullyQualifiedName);

                return DO_NOT_VISIT_SUBTREE;
            }
        }

        return VISIT_SUBTREE;
    }

    /**
     * The class that does the refactoring when an import needs to be added.
     *
     * @return The class that does the refactoring when an import needs to be added.
     */
    public abstract RefactoringWithNewClassImport getRefactoringClassInstance();

    /**
     * The package name to import.
     *
     * @return The package name to import
     */
    public abstract String getPackageNameToImport();

    /**
     * The class name to import.
     *
     * @return The class name to import
     */
    public abstract String getClassNameToImport();
}

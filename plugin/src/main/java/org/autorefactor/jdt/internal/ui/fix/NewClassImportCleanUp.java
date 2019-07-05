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
package org.autorefactor.jdt.internal.ui.fix;

import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.VISIT_SUBTREE;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.autorefactor.jdt.internal.corext.dom.InterruptibleVisitor;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SimpleType;
import org.eclipse.jdt.core.dom.TypeDeclaration;

/**
 * Handle the need to add an import for a class.
 */
public abstract class NewClassImportCleanUp extends AbstractCleanUpRule {

    /**
     * The class that does the refactoring when an import needs to be added.
     */
    public abstract class CleanUpWithNewClassImport extends ASTVisitor {
        private Set<String> classesToUseWithImport = new HashSet<String>();
        private Set<String> importsToAdd = new HashSet<String>();

        /**
         * The imports that need to be added.
         *
         * @return the importsToBeAdd
         */
        public Set<String> getImportsToAdd() {
            return importsToAdd;
        }

        /**
         * The imports already existing.
         *
         * @return the already imported classes
         */
        public Set<String> getClassesToUseWithImport() {
            return classesToUseWithImport;
        }
    }

    private class LocalClassVisitor extends InterruptibleVisitor {
        private Set<String> classnamesNeverUsedLocally = new HashSet<String>();

        /**
         * LocalClassVisitor.
         *
         * @param classnamesNeverUsedLocally Classnames never used locally
         */
        public LocalClassVisitor(Set<String> classnamesNeverUsedLocally) {
            this.classnamesNeverUsedLocally = classnamesNeverUsedLocally;
        }

        @Override
        public boolean visit(TypeDeclaration nestedClass) {
            classnamesNeverUsedLocally.remove(nestedClass.getName().getIdentifier());

            if (classnamesNeverUsedLocally.isEmpty()) {
                return interruptVisit();
            }

            return VISIT_SUBTREE;
        }

        @Override
        public boolean visit(SimpleType simpleName) {
            if (simpleName.getName().isSimpleName()) {
                classnamesNeverUsedLocally.remove(((SimpleName) simpleName.getName()).getIdentifier());

                if (classnamesNeverUsedLocally.isEmpty()) {
                    return interruptVisit();
                }
            }

            return VISIT_SUBTREE;
        }

        /**
         * @return the classnamesNeverUsedLocally
         */
        public Set<String> getClassnamesNeverUsedLocally() {
            return classnamesNeverUsedLocally;
        }
    }

    /**
     * True if an import already exists for a class.
     *
     * @param node One node in the class file
     * @return True if an import already exists for a class.
     */
    public Set<String> getAlreadyImportedClasses(final ASTNode node) {
        final Set<String> alreadyImportedClasses = new HashSet<String>();
        final CompilationUnit cu = (CompilationUnit) node.getRoot();
        final Set<String> classesToUse = getClassesToImport();
        final Map<String, String> importsByPackage = new HashMap<String, String>();

        for (String clazz : classesToUse) {
            importsByPackage.put(getPackageName(clazz), clazz);
        }

        for (final Object anObject : cu.imports()) {
            final ImportDeclaration anImport = (ImportDeclaration) anObject;

            if (anImport.isOnDemand()) {
                String fullName = importsByPackage.get(anImport.getName().getFullyQualifiedName());

                if (fullName != null) {
                    alreadyImportedClasses.add(fullName);
                }
            } else if (classesToUse.contains(anImport.getName().getFullyQualifiedName())) {
                alreadyImportedClasses.add(anImport.getName().getFullyQualifiedName());
            }
        }

        return alreadyImportedClasses;
    }

    @Override
    public boolean visit(final CompilationUnit node) {
        if (super.visit(node) == DO_NOT_VISIT_SUBTREE) {
            return DO_NOT_VISIT_SUBTREE;
        }

        final Set<String> classesToUse = getClassesToImport();

        if (classesToUse.isEmpty()) {
            return VISIT_SUBTREE;
        }

        final Map<String, String> importsByClassname = new HashMap<String, String>();
        final Map<String, String> importsByPackage = new HashMap<String, String>();

        for (String clazz : classesToUse) {
            importsByClassname.put(getSimpleName(clazz), clazz);
            importsByPackage.put(getPackageName(clazz), clazz);
        }

        Set<String> alreadyImportedClasses = new HashSet<String>();
        Set<String> classesToImport = new HashSet<String>(classesToUse);

        for (Object anObject : node.imports()) {
            ImportDeclaration anImport = (ImportDeclaration) anObject;

            if (!anImport.isStatic()) {
                if (anImport.isOnDemand()) {
                    String fullName = importsByPackage.get(anImport.getName().getFullyQualifiedName());

                    if (fullName != null) {
                        alreadyImportedClasses.add(fullName);
                    }
                } else if (classesToUse.contains(anImport.getName().getFullyQualifiedName())) {
                    alreadyImportedClasses.add(anImport.getName().getFullyQualifiedName());
                    classesToImport.remove(anImport.getName().getFullyQualifiedName());
                } else if (importsByClassname.containsKey(getSimpleName(anImport.getName()
                        .getFullyQualifiedName()))) {
                    classesToImport.remove(importsByClassname.get(getSimpleName(anImport.getName()
                            .getFullyQualifiedName())));
                }
            }
        }

        filterLocallyUsedNames(node, importsByClassname, classesToImport);

        if (alreadyImportedClasses.size() < classesToUse.size() && !classesToImport.isEmpty()) {
            final CleanUpWithNewClassImport refactoringClass = getRefactoringClassInstance();
            refactoringClass.getClassesToUseWithImport().addAll(alreadyImportedClasses);
            refactoringClass.getClassesToUseWithImport().addAll(classesToImport);
            node.accept(refactoringClass);

            if (!refactoringClass.getImportsToAdd().isEmpty()) {
                final Refactorings r = ctx.getRefactorings();

                for (String importToAdd : refactoringClass.getImportsToAdd()) {
                    r.getImportRewrite().addImport(importToAdd);
                }

                return DO_NOT_VISIT_SUBTREE;
            }
        }

        return VISIT_SUBTREE;
    }

    /**
     * The simple name of the class.
     *
     * @param fullyQualifiedName The name of the class with packages.
     * @return The simple name of the class.
     */
    public String getSimpleName(final String fullyQualifiedName) {
        return fullyQualifiedName.replaceFirst("^(?:.*\\.)?([^.]*)$", "$1");
    }

    /**
     * The package of the class.
     *
     * @param fullyQualifiedName The name of the class with packages.
     * @return The package of the class.
     */
    public String getPackageName(final String fullyQualifiedName) {
        return fullyQualifiedName.replaceFirst("^(.*)\\.[^.]+$", "$1");
    }

    private void filterLocallyUsedNames(final CompilationUnit node, final Map<String, String> importsByClassname,
            final Set<String> classesToImport) {
        final LocalClassVisitor nestedClassVisitor = new LocalClassVisitor(
                importsByClassname.keySet());
        nestedClassVisitor.visitNode(node);
        final Set<String> classnamesNeverUsedLocally = nestedClassVisitor.getClassnamesNeverUsedLocally();
        final Iterator<String> iterator = classesToImport.iterator();

        while (iterator.hasNext()) {
            final String classToImport = iterator.next();

            if (!classnamesNeverUsedLocally.contains(getSimpleName(classToImport))) {
                classesToImport.remove(classToImport);
            }
        }
    }

    /**
     * The class that does the refactoring when an import needs to be added.
     *
     * @return The class that does the refactoring when an import needs to be added.
     */
    public abstract CleanUpWithNewClassImport getRefactoringClassInstance();

    /**
     * The class names to import.
     *
     * @return The class names to import
     */
    public abstract Set<String> getClassesToImport();
}

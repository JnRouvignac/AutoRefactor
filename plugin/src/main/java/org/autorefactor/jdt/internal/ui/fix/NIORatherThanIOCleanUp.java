/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - initial API and implementation
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

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.autorefactor.jdt.internal.corext.dom.VarDefinitionsUsesVisitor;
import org.autorefactor.jdt.internal.corext.refactoring.structure.CompilationUnitRewrite;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/** See {@link #getDescription()} method. */
public class NIORatherThanIOCleanUp extends NewClassImportCleanUp {
    private static final String GET_METHOD= "get"; //$NON-NLS-1$
    private static final String TOPATH_METHOD= "toPath"; //$NON-NLS-1$
    private static final String TO_U_R_I_METHOD= "toURI"; //$NON-NLS-1$
    private static final String TOURI_METHOD= "toUri"; //$NON-NLS-1$

    private final class RefactoringWithObjectsClass extends CleanUpWithNewClassImport {
        @Override
        public boolean visit(final MethodInvocation node) {
            return NIORatherThanIOCleanUp.this
                    .maybeRefactorMethodInvocation(node, getClassesToUseWithImport(), getImportsToAdd());
        }
        @Override
        public boolean visit(final Block node) {
            return NIORatherThanIOCleanUp.this
                    .maybeRefactorBlock(node, getClassesToUseWithImport(), getImportsToAdd());
        }
    }

    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_NIORatherThanIOCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_NIORatherThanIOCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_NIORatherThanIOCleanUp_reason;
    }

    @Override
    public boolean isJavaVersionSupported(final Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 7;
    }

    @Override
    public RefactoringWithObjectsClass getRefactoringClassInstance() {
        return new RefactoringWithObjectsClass();
    }

    @Override
    public Set<String> getClassesToImport() {
        return new HashSet<>(Arrays.asList(Path.class.getCanonicalName(), Paths.class.getCanonicalName()));
    }

    @Override
    public boolean visit(final MethodInvocation node) {
        return maybeRefactorMethodInvocation(node, getAlreadyImportedClasses(node), new HashSet<String>());
    }

    private boolean maybeRefactorMethodInvocation(final MethodInvocation node,
            final Set<String> classesToUseWithImport, final Set<String> importsToAdd) {
        if (isFileCreation(node.getExpression())
                && isFileUse(node.getExpression())) {
            ASTNodeFactory ast= cuRewrite.getASTBuilder();
            ASTRewrite rewrite= cuRewrite.getASTRewrite();

            String pathsName= classesToUseWithImport.contains(Paths.class.getCanonicalName()) ? Paths.class.getSimpleName() : Paths.class.getCanonicalName();
            importsToAdd.add(Paths.class.getCanonicalName());

            ClassInstanceCreation classInstanceCreation= (ClassInstanceCreation) node.getExpression();
            Expression copyOfPathText= rewrite.createMoveTarget((Expression) classInstanceCreation.arguments().get(0));

            if (ASTNodes.usesGivenSignature(node, File.class.getCanonicalName(), TOPATH_METHOD)) {
                rewrite.replace(node, ast.invoke(ast.name(pathsName), GET_METHOD, copyOfPathText));
            } else {
                rewrite.replace(node, ast.invoke(ast.invoke(ast.name(pathsName), GET_METHOD, copyOfPathText), TOURI_METHOD));
            }

            return false;
        }

        return true;
    }

    @Override
    public boolean visit(final Block node) {
        return maybeRefactorBlock(node, getAlreadyImportedClasses(node), new HashSet<String>());
    }

    private boolean maybeRefactorBlock(final Block node,
            final Set<String> classesToUseWithImport, final Set<String> importsToAdd) {
        FileAndUsesVisitor fileAndUsesVisitor= new FileAndUsesVisitor(cuRewrite, node, classesToUseWithImport, importsToAdd);
        node.accept(fileAndUsesVisitor);
        return fileAndUsesVisitor.getResult();
    }

    private final class FileAndUsesVisitor extends BlockSubVisitor {
        private final Block blockNode;
        private final Set<String> classesToUseWithImport;
        private final Set<String> importsToAdd;

        public FileAndUsesVisitor(final CompilationUnitRewrite cuRewrite, final Block startNode,
                final Set<String> classesToUseWithImport, final Set<String> importsToAdd) {
            super(cuRewrite, startNode);

            this.blockNode= startNode;
            this.classesToUseWithImport= classesToUseWithImport;
            this.importsToAdd= importsToAdd;
        }

        @Override
        public boolean visit(final VariableDeclarationStatement node) {
            if (node.fragments().size() != 1) {
                return true;
            }

            VariableDeclarationFragment fragment= (VariableDeclarationFragment) node.fragments().get(0);
            return visitVariable(node.getType(), fragment.resolveBinding(), fragment.getExtraDimensions(), fragment.getInitializer());
        }

        @Override
        public boolean visit(final VariableDeclarationExpression node) {
            if (node.fragments().size() != 1) {
                return true;
            }

            VariableDeclarationFragment fragment= (VariableDeclarationFragment) node.fragments().get(0);
            return visitVariable(node.getType(), fragment.resolveBinding(), fragment.getExtraDimensions(), fragment.getInitializer());
        }

        @Override
        public boolean visit(final SingleVariableDeclaration node) {
            return visitVariable(node.getType(), node.resolveBinding(), node.getExtraDimensions(), node.getInitializer());
        }

        private boolean visitVariable(final Type type, final IVariableBinding variableBinding, final int extraDimensions, final Expression initializer) {
            if (getResult() && ASTNodes.hasType(type.resolveBinding(), File.class.getCanonicalName())
                    && extraDimensions == 0
                    && initializer != null) {
                if (isFileCreation(initializer)) {
                    VarDefinitionsUsesVisitor varOccurrencesVisitor= new VarDefinitionsUsesVisitor(variableBinding,
                            blockNode, true).find();

                    List<SimpleName> reads= varOccurrencesVisitor.getReads();
                    List<SimpleName> writes= varOccurrencesVisitor.getWrites();

                    if (writes.size() == 1 && !reads.isEmpty()) {
                        for (SimpleName simpleName : reads) {
                            if (!isFileUse(simpleName)) {
                                return true;
                            }
                        }

                        refactorFile(type, initializer, reads);

                        setResult(false);
                        return false;
                    }
                }
            }

            return true;
        }

        private void refactorFile(final Type type, final Expression initializer, final List<SimpleName> fileUses) {
            ASTNodeFactory ast= cuRewrite.getASTBuilder();
            ASTRewrite rewrite= cuRewrite.getASTRewrite();

            ClassInstanceCreation classInstanceCreation= ASTNodes.as(initializer, ClassInstanceCreation.class);

            String pathsName= classesToUseWithImport.contains(Paths.class.getCanonicalName()) ? Paths.class.getSimpleName() : Paths.class.getCanonicalName();
            String pathName= classesToUseWithImport.contains(Path.class.getCanonicalName()) ? Path.class.getSimpleName() : Path.class.getCanonicalName();
            importsToAdd.add(Paths.class.getCanonicalName());
            importsToAdd.add(Path.class.getCanonicalName());
            rewrite.replace(type, ast.type(pathName));
            rewrite.replace(classInstanceCreation, ast.invoke(ast.name(pathsName), GET_METHOD, rewrite.createMoveTarget((Expression) classInstanceCreation.arguments().get(0))));

            for (SimpleName fileUse : fileUses) {
                MethodInvocation methodInvocation= (MethodInvocation) fileUse.getParent();

                if (ASTNodes.usesGivenSignature(methodInvocation, File.class.getCanonicalName(), TOPATH_METHOD)) {
                    rewrite.replace(methodInvocation, rewrite.createMoveTarget(fileUse));
                } else {
                    rewrite.replace(methodInvocation, ast.invoke(rewrite.createMoveTarget(fileUse), TOURI_METHOD));
                }
            }
        }
    }

    private boolean isFileCreation(final Expression initializer) {
        ClassInstanceCreation classInstanceCreation= ASTNodes.as(initializer, ClassInstanceCreation.class);

        return classInstanceCreation != null
                && classInstanceCreation.getExpression() == null
                && classInstanceCreation.getAnonymousClassDeclaration() == null
                && ASTNodes.hasType(classInstanceCreation.resolveTypeBinding(), File.class.getCanonicalName())
                && Utils.isEmpty(classInstanceCreation.typeArguments())
                && classInstanceCreation.arguments() != null
                && classInstanceCreation.arguments().size() == 1
                && ASTNodes.hasType((Expression) classInstanceCreation.arguments().get(0), String.class.getCanonicalName());
    }

    private boolean isFileUse(final Expression fileObject) {
        if (fileObject != null && fileObject.getParent() instanceof MethodInvocation) {
            MethodInvocation methodInvocation= (MethodInvocation) fileObject.getParent();

            if (fileObject.getLocationInParent() == MethodInvocation.EXPRESSION_PROPERTY
                    && Utils.isEmpty(methodInvocation.arguments())
                    && (ASTNodes.usesGivenSignature(methodInvocation, File.class.getCanonicalName(), TOPATH_METHOD)
                            || ASTNodes.usesGivenSignature(methodInvocation, File.class.getCanonicalName(), TO_U_R_I_METHOD))) {
                return true;
            }
        }

        return false;
    }
}

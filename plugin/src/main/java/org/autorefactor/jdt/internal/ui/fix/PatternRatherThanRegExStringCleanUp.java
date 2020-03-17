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

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.autorefactor.jdt.internal.corext.dom.VarDefinitionsUsesVisitor;
import org.autorefactor.jdt.internal.corext.refactoring.structure.CompilationUnitRewrite;
import org.eclipse.jdt.core.dom.Block;
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
public class PatternRatherThanRegExStringCleanUp extends NewClassImportCleanUp {
    private static final String SPLIT_METHOD= "split"; //$NON-NLS-1$
    private static final String REPLACE_FIRST_METHOD= "replaceFirst"; //$NON-NLS-1$
    private static final String REPLACE_ALL_METHOD= "replaceAll"; //$NON-NLS-1$
    private static final String MATCHER_METHOD= "matcher"; //$NON-NLS-1$
    private static final String MATCHES_METHOD= "matches"; //$NON-NLS-1$
    private static final String COMPILE_METHOD= "compile"; //$NON-NLS-1$

    private final class RefactoringWithObjectsClass extends CleanUpWithNewClassImport {
        @Override
        public boolean visit(final Block node) {
            return PatternRatherThanRegExStringCleanUp.this
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
        return MultiFixMessages.CleanUpRefactoringWizard_PatternRatherThanRegExStringCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_PatternRatherThanRegExStringCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_PatternRatherThanRegExStringCleanUp_reason;
    }

    @Override
    public boolean isJavaVersionSupported(final Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 4;
    }

    @Override
    public RefactoringWithObjectsClass getRefactoringClassInstance() {
        return new RefactoringWithObjectsClass();
    }

    @Override
    public Set<String> getClassesToImport() {
        return new HashSet<>(Arrays.asList(Pattern.class.getCanonicalName()));
    }

    @Override
    public boolean visit(final Block node) {
        return maybeRefactorBlock(node, getAlreadyImportedClasses(node), new HashSet<String>());
    }

    private boolean maybeRefactorBlock(final Block node,
            final Set<String> classesToUseWithImport, final Set<String> importsToAdd) {
        RegExAndUsesVisitor regExAndUsesVisitor= new RegExAndUsesVisitor(cuRewrite, node, classesToUseWithImport, importsToAdd);
        node.accept(regExAndUsesVisitor);
        return regExAndUsesVisitor.getResult();
    }

    private static final class RegExAndUsesVisitor extends BlockSubVisitor {
        private final Block blockNode;
        private final Set<String> classesToUseWithImport;
        private final Set<String> importsToAdd;

        public RegExAndUsesVisitor(final CompilationUnitRewrite cuRewrite, final Block startNode,
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
            if (getResult() && ASTNodes.hasType(type.resolveBinding(), String.class.getCanonicalName())
                    && extraDimensions == 0
                    && initializer != null) {
                VarDefinitionsUsesVisitor varOccurrencesVisitor= new VarDefinitionsUsesVisitor(variableBinding,
                        blockNode, true).find();

                List<SimpleName> reads= varOccurrencesVisitor.getReads();
                List<SimpleName> writes= varOccurrencesVisitor.getWrites();

                if (writes.size() == 1 && reads.size() > 1) {
                    for (SimpleName simpleName : reads) {
                        if (!isRegExUse(simpleName)) {
                            return true;
                        }
                    }
                    refactorRegEx(type, initializer, reads);

                    setResult(false);
                    return false;
                }
            }

            return true;
        }

        private boolean isRegExUse(final SimpleName simpleName) {
            if (simpleName.getParent() instanceof MethodInvocation) {
                MethodInvocation methodInvocation= (MethodInvocation) simpleName.getParent();

                if (simpleName.getLocationInParent() == MethodInvocation.ARGUMENTS_PROPERTY
                        && !methodInvocation.arguments().isEmpty()
                        && simpleName.equals(methodInvocation.arguments().get(0))) {
                    if (ASTNodes.usesGivenSignature(methodInvocation, String.class.getCanonicalName(), MATCHES_METHOD, String.class.getCanonicalName())
                            || ASTNodes.usesGivenSignature(methodInvocation, String.class.getCanonicalName(), REPLACE_ALL_METHOD, String.class.getCanonicalName(), String.class.getCanonicalName())
                            || ASTNodes.usesGivenSignature(methodInvocation, String.class.getCanonicalName(), REPLACE_FIRST_METHOD, String.class.getCanonicalName(), String.class.getCanonicalName())
                            || ASTNodes.usesGivenSignature(methodInvocation, String.class.getCanonicalName(), SPLIT_METHOD, String.class.getCanonicalName())
                            || ASTNodes.usesGivenSignature(methodInvocation, String.class.getCanonicalName(), SPLIT_METHOD, String.class.getCanonicalName(), int.class.getCanonicalName())) {
                        return true;
                    }
                }
            }

            return false;
        }

        private void refactorRegEx(final Type type, final Expression initializer, final List<SimpleName> regExUses) {
            ASTNodeFactory ast= cuRewrite.getASTBuilder();
            ASTRewrite rewrite= cuRewrite.getASTRewrite();

            String patternName= classesToUseWithImport.contains(Pattern.class.getCanonicalName()) ? Pattern.class.getSimpleName() : Pattern.class.getCanonicalName();
            importsToAdd.add(Pattern.class.getCanonicalName());
            rewrite.replace(type, ast.type(patternName), null);
            rewrite.replace(initializer, ast.invoke(ast.name(patternName), COMPILE_METHOD, rewrite.createMoveTarget(initializer)), null);

            for (SimpleName regExUse : regExUses) {
                MethodInvocation methodInvocation= (MethodInvocation) regExUse.getParent();
                MethodInvocation newExpression= null;

                if (ASTNodes.usesGivenSignature(methodInvocation, String.class.getCanonicalName(), SPLIT_METHOD, String.class.getCanonicalName())) {
                    newExpression= ast.invoke(rewrite.createMoveTarget(regExUse), SPLIT_METHOD, rewrite.createMoveTarget(methodInvocation.getExpression()));
                } else if (ASTNodes.usesGivenSignature(methodInvocation, String.class.getCanonicalName(), SPLIT_METHOD, String.class.getCanonicalName(), int.class.getCanonicalName())) {
                    newExpression= ast.invoke(rewrite.createMoveTarget(regExUse), SPLIT_METHOD, rewrite.createMoveTarget(methodInvocation.getExpression()), rewrite.createMoveTarget((Expression) methodInvocation.arguments().get(1)));
                } else {
                    MethodInvocation matcherExpression= ast.invoke(rewrite.createMoveTarget(regExUse), MATCHER_METHOD, rewrite.createMoveTarget(methodInvocation.getExpression()));

                    if (ASTNodes.usesGivenSignature(methodInvocation, String.class.getCanonicalName(), MATCHES_METHOD, String.class.getCanonicalName())) {
                        newExpression= ast.invoke(matcherExpression, MATCHES_METHOD);
                    } else if (ASTNodes.usesGivenSignature(methodInvocation, String.class.getCanonicalName(), REPLACE_ALL_METHOD, String.class.getCanonicalName(), String.class.getCanonicalName())) {
                        newExpression= ast.invoke(matcherExpression, REPLACE_ALL_METHOD, rewrite.createMoveTarget((Expression) methodInvocation.arguments().get(1)));
                    } else if (ASTNodes.usesGivenSignature(methodInvocation, String.class.getCanonicalName(), REPLACE_FIRST_METHOD, String.class.getCanonicalName(), String.class.getCanonicalName())) {
                        newExpression= ast.invoke(matcherExpression, REPLACE_FIRST_METHOD, rewrite.createMoveTarget((Expression) methodInvocation.arguments().get(1)));
                    }
                }

                rewrite.replace(methodInvocation, newExpression, null);
            }
        }
    }
}

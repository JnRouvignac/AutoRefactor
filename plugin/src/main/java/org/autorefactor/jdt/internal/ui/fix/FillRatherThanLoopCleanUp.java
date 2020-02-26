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

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.ForLoopHelper;
import org.autorefactor.jdt.internal.corext.dom.ForLoopHelper.ContainerType;
import org.autorefactor.jdt.internal.corext.dom.ForLoopHelper.ForLoopContent;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.eclipse.jdt.core.dom.ArrayAccess;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.Statement;

/** See {@link #getDescription()} method. */
public class FillRatherThanLoopCleanUp extends NewClassImportCleanUp {
    private final class RefactoringWithObjectsClass extends CleanUpWithNewClassImport {
        @Override
        public boolean visit(final ForStatement node) {
            return FillRatherThanLoopCleanUp.this.maybeRefactorForStatement(node,
                    getClassesToUseWithImport(), getImportsToAdd());
        }
    }

    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_FillRatherThanLoopCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_FillRatherThanLoopCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_FillRatherThanLoopCleanUp_reason;
    }

    @Override
    public RefactoringWithObjectsClass getRefactoringClassInstance() {
        return new RefactoringWithObjectsClass();
    }

    @Override
    public Set<String> getClassesToImport() {
        return new HashSet<>(Arrays.asList(Arrays.class.getCanonicalName()));
    }

    @Override
    public boolean isJavaVersionSupported(final Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 2;
    }

    @Override
    public boolean visit(final ForStatement node) {
        return maybeRefactorForStatement(node, getAlreadyImportedClasses(node), new HashSet<String>());
    }

    private boolean maybeRefactorForStatement(final ForStatement node, final Set<String> classesToUseWithImport,
            final Set<String> importsToAdd) {
        ForLoopContent loopContent= ForLoopHelper.iterateOverContainer(node);
        List<Statement> statements= ASTNodes.asList(node.getBody());

        if (loopContent != null && loopContent.getLoopVariable() != null && loopContent.getContainerType() == ContainerType.ARRAY && statements.size() == 1) {
            Assignment assignment= ASTNodes.asExpression(statements.get(0), Assignment.class);

            if (ASTNodes.hasOperator(assignment, Assignment.Operator.ASSIGN) && ASTNodes.isHardCoded(assignment.getRightHandSide()) && ASTNodes.isPassive(assignment.getRightHandSide())) {
                ArrayAccess arrayAccess= ASTNodes.as(assignment.getLeftHandSide(), ArrayAccess.class);

                if (arrayAccess != null && isSameVariable(loopContent, arrayAccess)) {
                    replaceWithArraysFill(node, classesToUseWithImport, assignment, arrayAccess);
                    importsToAdd.add(Arrays.class.getCanonicalName());
                    return false;
                }
            }
        }

        return true;
    }

    private void replaceWithArraysFill(final ForStatement node, final Set<String> classesToUseWithImport,
            final Assignment assignment, final ArrayAccess arrayAccess) {
        ASTNodeFactory b= ctx.getASTBuilder();
        ctx.getRefactorings().replace(node,
                b.toStatement(b.invoke(b.name(classesToUseWithImport.contains(Arrays.class.getCanonicalName()) ? Arrays.class.getSimpleName() : Arrays.class.getCanonicalName()),
                        "fill", b.createMoveTarget(arrayAccess.getArray()), //$NON-NLS-1$
                        b.createMoveTarget(assignment.getRightHandSide()))));
    }

    private boolean isSameVariable(final ForLoopContent loopContent, final ArrayAccess aa) {
        return aa != null && ASTNodes.isSameVariable(aa.getArray(), loopContent.getContainerVariable())
                && ASTNodes.isSameLocalVariable(aa.getIndex(), loopContent.getLoopVariable());
    }
}

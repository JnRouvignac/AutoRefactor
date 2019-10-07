/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Andrei Paikin - Initial API and implementation
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

import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.IExtendedModifier;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SuperConstructorInvocation;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.TypeDeclaration;

/** See {@link #getDescription()} method. */
public class ImplicitDefaultConstructorRatherThanWrittenOneCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_ImplicitDefaultConstructorRatherThanWrittenOneCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_ImplicitDefaultConstructorRatherThanWrittenOneCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_ImplicitDefaultConstructorRatherThanWrittenOneCleanUp_reason;
    }

    @Override
    public boolean visit(final TypeDeclaration node) {
        if (!node.isInterface()) {
            MethodDeclaration uniqueConstructor= null;
            boolean isPublicClass= false;
            boolean isProtectedClass= false;
            boolean isPackageClass= true;
            boolean isPrivateClass= false;

            for (IExtendedModifier extendedModifier : ASTNodes.modifiers(node)) {
                if (extendedModifier.isModifier()) {
                    final Modifier modifier= (Modifier) extendedModifier;
                    if (modifier.isPublic()) {
                        isPublicClass= true;
                        isPackageClass= false;
                        break;
                    } else if (modifier.isProtected()) {
                        isProtectedClass= true;
                        isPackageClass= false;
                        break;
                    } else if (modifier.isPrivate()) {
                        isPrivateClass= true;
                        isPackageClass= false;
                        break;
                    }
                }
            }

            for (MethodDeclaration methodDeclaration : node.getMethods()) {
                if (methodDeclaration.isConstructor()) {
                    if (uniqueConstructor == null) {
                        uniqueConstructor= methodDeclaration;
                    } else {
                        // Too much constructors
                        return true;
                    }
                }
            }

            if (uniqueConstructor != null
                    && (!isCheckedExceptionThrown(uniqueConstructor) || node.getSuperclassType() == null
                            || ASTNodes.hasType(node.getSuperclassType().resolveBinding(), Object.class.getCanonicalName()))
                    && (uniqueConstructor.parameters() == null || uniqueConstructor.parameters().isEmpty())
                    && isDefaultStatements(uniqueConstructor)) {
                if (uniqueConstructor.modifiers() != null && uniqueConstructor.modifiers().size() == 1) {
                    final IExtendedModifier extendedModifier= (IExtendedModifier) uniqueConstructor.modifiers().get(0);
                    if (extendedModifier.isModifier()) {
                        Modifier modifier= (Modifier) extendedModifier;
                        if ((modifier.isPublic() && isPublicClass) || (modifier.isProtected() && isProtectedClass)
                                || (modifier.isPrivate() && isPrivateClass)) {
                            ctx.getRefactorings().remove(uniqueConstructor);
                            return false;
                        }
                    }
                } else if ((uniqueConstructor.modifiers() == null || uniqueConstructor.modifiers().isEmpty())
                        && isPackageClass) {
                    ctx.getRefactorings().remove(uniqueConstructor);
                    return false;
                }
            }
        }

        return true;
    }

    private boolean isDefaultStatements(final MethodDeclaration uniqueConstructor) {
        final List<Statement> statements= ASTNodes.statements(uniqueConstructor.getBody());

        if (statements == null || statements.isEmpty()) {
            return true;
        } else if (statements.size() == 1) {
            final SuperConstructorInvocation superStatement= ASTNodes.as(statements.get(0), SuperConstructorInvocation.class);

            return superStatement != null && (superStatement.arguments() == null || superStatement.arguments().isEmpty());
        }

        return false;
    }

    private boolean isCheckedExceptionThrown(final MethodDeclaration uniqueConstructor) {
        if (uniqueConstructor.thrownExceptionTypes() != null) {
            for (Object type : uniqueConstructor.thrownExceptionTypes()) {
                if (isChecked((Type) type)) {
                    return true;
                }
            }
        }
        return false;
    }

    private boolean isChecked(Type type) {
        final ITypeBinding binding= type.resolveBinding();
        return !ASTNodes.instanceOf(binding, RuntimeException.class.getCanonicalName()) && !ASTNodes.instanceOf(binding, Error.class.getCanonicalName());
    }
}

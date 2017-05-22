/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Jean-Noël Rouvignac - initial API and implementation
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
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.dom.Annotation;
import org.eclipse.jdt.core.dom.IExtendedModifier;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.MarkerAnnotation;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.internal.corext.util.MethodOverrideTester;
import org.eclipse.jdt.internal.corext.util.SuperTypeHierarchyCache;

/**
 * Automated refactoring to "Add missing @Override annotations" to methods.
 */
@SuppressWarnings("restriction")
public class AddMissingOverrideAnnotationRefactoring extends AbstractRefactoringRule {

    private static final String OVERRIDE = "Override";

    @Override
    public String getDescription() {
        return "Add missing @Override annotation";
    }

    @Override
    public String getName() {
        return "Add missing @Override annotation";
    }

    @Override
    public boolean visit(MethodDeclaration node) {
        @SuppressWarnings("unchecked")
        List<IExtendedModifier> modifiers = node.modifiers();
        if (-1 != findOverridenAnnotationModifier(modifiers)) {
            return true;
        }
        IMethod overridenMeth = findOverridenMethod(node);
        if (overridenMeth != null) {
            // detected missing @Overriden => add it
            final ASTBuilder b = ctx.getASTBuilder();
            Annotation overrideAnnotation = b.annotation(OVERRIDE);
            ctx.getRefactorings().insertAt(node, MethodDeclaration.MODIFIERS2_PROPERTY, overrideAnnotation, 0);
        }
        return true;
    }

    /** @return overriden method, or null if not found/failed. */
    private static IMethod findOverridenMethod(MethodDeclaration meth) {
        IMethod res = null;
        IMethod imeth = bindingToIMethod(meth);
        if (imeth != null) {
            IType itype = imeth.getDeclaringType();
            try {
                MethodOverrideTester methodOverrideTester = SuperTypeHierarchyCache.getMethodOverrideTester(itype);
                res = methodOverrideTester.findOverriddenMethod(imeth, true);
            } catch (Exception ex) {
                res = null; // ignore, no rethrow!
            }
        }
        return res;
    }

    /** @return index if exist of "@Override" MarkerAnnotation element in list. */
    private static int findOverridenAnnotationModifier(List<IExtendedModifier> modifiers) {
        for (int i = 0; i < modifiers.size(); i++) {
            IExtendedModifier m = modifiers.get(i);
            if (m instanceof MarkerAnnotation) {
                MarkerAnnotation ma = (MarkerAnnotation) m;
                String fqn = ma.getTypeName().getFullyQualifiedName();
                if (fqn.equals(OVERRIDE)) {
                    return i;
                }
            }
        }
        return -1;
    }

    /** @return corresponding IMethod of AST method declaration. */
    private static IMethod bindingToIMethod(MethodDeclaration node) {
        IMethodBinding binding = node.resolveBinding();
        return (binding != null) ? (IMethod) binding.getJavaElement() : null;
    }

}

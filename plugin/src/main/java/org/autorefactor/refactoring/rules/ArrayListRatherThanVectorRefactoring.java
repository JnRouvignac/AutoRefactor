/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - initial API and implementation
 * Copyright (C) 2017 Jean-Noël Rouvignac - minor changes
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
import org.autorefactor.refactoring.Release;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.MethodInvocation;

import static org.autorefactor.refactoring.ASTHelper.*;

/** See {@link #getDescription()} method. */
public class ArrayListRatherThanVectorRefactoring extends AbstractClassSubstituteRefactoring {
    @Override
    public String getDescription() {
        return ""
            + "Replace Vector by ArrayList when possible.";
    }

    @Override
    public String getName() {
        return "ArrayList rather than Vector";
    }

    @Override
    public boolean isJavaVersionSupported(Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 2;
    }

    @Override
    protected boolean canBeSharedInOtherThread() {
        return false;
    }

    @Override
    protected String[] getExistingClassCanonicalName() {
        return new String[] {"java.util.Vector"};
    }

    @Override
    protected String getSubstitutingClassName() {
        return "java.util.ArrayList";
    }

    @Override
    protected boolean canInstantiationBeRefactored(final ClassInstanceCreation instanceCreation) {
        return instanceCreation.arguments().size() < 2;
    }

    @Override
    protected boolean canMethodBeRefactored(final MethodInvocation mi,
            final List<MethodInvocation> methodCallsToRefactor) {
        if (isMethod(mi, "java.util.Vector", "addElement", "java.lang.Object")
                || isMethod(mi, "java.util.Vector", "elementAt", "int")
                || isMethod(mi, "java.util.Vector", "copyInto", "java.lang.Object[]")
                || isMethod(mi, "java.util.Vector", "removeElement", "java.lang.Object")
                || isMethod(mi, "java.util.Vector", "removeElementAt", "int")
                || isMethod(mi, "java.util.Vector", "removeAllElements")
                || isMethod(mi, "java.util.Vector", "setElementAt", "java.lang.Object", "int")
                || isMethod(mi, "java.util.Vector", "insertElementAt", "java.lang.Object", "int")) {
            methodCallsToRefactor.add(mi);
            return true;
        } else if (isMethod(mi, "java.util.Vector", "setSize", "int")
                || isMethod(mi, "java.util.Vector", "capacity")
                || isMethod(mi, "java.util.Vector", "elements")
                || isMethod(mi, "java.util.Vector", "indexOf", "java.lang.Object", "int")
                || isMethod(mi, "java.util.Vector", "lastIndexOf", "java.lang.Object", "int")
                || isMethod(mi, "java.util.Vector", "firstElement")
                || isMethod(mi, "java.util.Vector", "lastElement")) {
            return false;
        }
        return true;
    }

    @Override
    protected void refactorMethod(final ASTBuilder b, final MethodInvocation originalMi,
            final MethodInvocation refactoredMi) {
        if (isMethod(originalMi, "java.util.Vector", "addElement", "java.lang.Object")) {
            refactoredMi.setName(b.simpleName("add"));
        } else if (isMethod(originalMi, "java.util.Vector", "elementAt", "int")) {
            refactoredMi.setName(b.simpleName("get"));
        } else if (isMethod(originalMi, "java.util.Vector", "copyInto", "java.lang.Object[]")) {
            refactoredMi.setName(b.simpleName("toArray"));
        } else if (isMethod(originalMi, "java.util.Vector", "removeElement", "java.lang.Object")) {
            refactoredMi.setName(b.simpleName("remove"));
        } else if (isMethod(originalMi, "java.util.Vector", "removeElementAt", "int")) {
            refactoredMi.setName(b.simpleName("remove"));
        } else if (isMethod(originalMi, "java.util.Vector", "removeAllElements")) {
            refactoredMi.setName(b.simpleName("clear"));
        } else if (isMethod(originalMi, "java.util.Vector", "insertElementAt", "java.lang.Object", "int")) {
            refactoredMi.setName(b.simpleName("add"));
            reorderArguments(refactoredMi);
        } else if (isMethod(originalMi, "java.util.Vector", "setElementAt", "java.lang.Object", "int")) {
            refactoredMi.setName(b.simpleName("set"));
            reorderArguments(refactoredMi);
        }
    }

    private void reorderArguments(final MethodInvocation refactoredMi) {
        Object item = refactoredMi.arguments().get(0);
        Object index = refactoredMi.arguments().get(1);
        refactoredMi.arguments().clear();
        refactoredMi.arguments().add(index);
        refactoredMi.arguments().add(item);

        if (refactoredMi.typeArguments() != null && !refactoredMi.typeArguments().isEmpty()) {
            Object itemType = refactoredMi.typeArguments().get(0);
            Object indexType = refactoredMi.typeArguments().get(1);
            refactoredMi.typeArguments().clear();
            refactoredMi.typeArguments().add(indexType);
            refactoredMi.typeArguments().add(itemType);
        }
    }
}

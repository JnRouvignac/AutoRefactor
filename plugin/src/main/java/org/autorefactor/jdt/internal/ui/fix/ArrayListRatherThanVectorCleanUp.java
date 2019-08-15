/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - initial API and implementation
 * Copyright (C) 2017-2018 Jean-Noël Rouvignac - minor changes
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

import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.*;
import static org.autorefactor.util.Utils.getOrDefault;

import java.io.Serializable;
import java.util.AbstractCollection;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import org.autorefactor.jdt.internal.corext.dom.ASTBuilder;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Type;

/** See {@link #getDescription()} method. */
public class ArrayListRatherThanVectorCleanUp extends AbstractClassSubstituteCleanUp {
    private static final Map<String, String[]> CAN_BE_CASTED_TO= new HashMap<String, String[]>();

    static {
        CAN_BE_CASTED_TO.put(Object.class.getCanonicalName(), new String[] { Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(Cloneable.class.getCanonicalName(), new String[] { Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(Serializable.class.getCanonicalName(), new String[] { Serializable.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(Collection.class.getCanonicalName(), new String[] { Collection.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(AbstractCollection.class.getCanonicalName(), new String[] { AbstractCollection.class.getCanonicalName(), Collection.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(Vector.class.getCanonicalName(), new String[] { Vector.class.getCanonicalName(), AbstractCollection.class.getCanonicalName(), Collection.class.getCanonicalName(), Serializable.class.getCanonicalName(), Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
    }

    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_ArrayListRatherThanVectorCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_ArrayListRatherThanVectorCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_ArrayListRatherThanVectorCleanUp_reason;
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
        return new String[] { Vector.class.getCanonicalName() };
    }

    @Override
    public Set<String> getClassesToImport() {
        return new HashSet<String>(Arrays.asList(ArrayList.class.getCanonicalName()));
    }

    @Override
    protected String getSubstitutingClassName(String origRawType) {
        if (Vector.class.getCanonicalName().equals(origRawType)) {
            return ArrayList.class.getCanonicalName();
        } else {
            return null;
        }
    }

    @Override
    protected boolean canInstantiationBeRefactored(final ClassInstanceCreation instanceCreation) {
        return instanceCreation.arguments().size() < 2;
    }

    @Override
    protected boolean canMethodBeRefactored(final MethodInvocation mi,
            final List<MethodInvocation> methodCallsToRefactor) {
        if (usesGivenSignature(mi, Vector.class.getCanonicalName(), "addElement", Object.class.getCanonicalName()) //$NON-NLS-1$
                || usesGivenSignature(mi, Vector.class.getCanonicalName(), "elementAt", int.class.getSimpleName()) //$NON-NLS-1$
                || usesGivenSignature(mi, Vector.class.getCanonicalName(), "copyInto", Object[].class.getCanonicalName()) //$NON-NLS-1$
                || usesGivenSignature(mi, Vector.class.getCanonicalName(), "removeElement", Object.class.getCanonicalName()) //$NON-NLS-1$
                || usesGivenSignature(mi, Vector.class.getCanonicalName(), "removeElementAt", int.class.getSimpleName()) //$NON-NLS-1$
                || usesGivenSignature(mi, Vector.class.getCanonicalName(), "removeAllElements") //$NON-NLS-1$
                || usesGivenSignature(mi, Vector.class.getCanonicalName(), "setElementAt", Object.class.getCanonicalName(), int.class.getSimpleName()) //$NON-NLS-1$
                || usesGivenSignature(mi, Vector.class.getCanonicalName(), "insertElementAt", Object.class.getCanonicalName(), int.class.getSimpleName())) { //$NON-NLS-1$
            methodCallsToRefactor.add(mi);
            return true;
        }

        final String argumentType= getArgumentType(mi);
        return usesGivenSignature(mi, Collection.class.getCanonicalName(), "add", Object.class.getCanonicalName()) //$NON-NLS-1$
                || usesGivenSignature(mi, List.class.getCanonicalName(), "addAll", int.class.getSimpleName(), Collection.class.getCanonicalName()) //$NON-NLS-1$
                || usesGivenSignature(mi, Collection.class.getCanonicalName(), "clear") //$NON-NLS-1$
                || usesGivenSignature(mi, Collection.class.getCanonicalName(), "contains", Object.class.getCanonicalName()) //$NON-NLS-1$
                || usesGivenSignature(mi, Collection.class.getCanonicalName(), "containsAll", Collection.class.getCanonicalName()) //$NON-NLS-1$
                || usesGivenSignature(mi, Object.class.getCanonicalName(), "equals", Object.class.getCanonicalName()) //$NON-NLS-1$
                || usesGivenSignature(mi, Object.class.getCanonicalName(), "hashCode") || usesGivenSignature(mi, Collection.class.getCanonicalName(), "isEmpty") //$NON-NLS-1$ $NON-NLS-2$
                || usesGivenSignature(mi, Collection.class.getCanonicalName(), "iterator") //$NON-NLS-1$
                || usesGivenSignature(mi, Collection.class.getCanonicalName(), "remove", Object.class.getCanonicalName()) //$NON-NLS-1$
                || usesGivenSignature(mi, Collection.class.getCanonicalName(), "removeAll", Collection.class.getCanonicalName()) //$NON-NLS-1$
                || usesGivenSignature(mi, Collection.class.getCanonicalName(), "retainAll", Collection.class.getCanonicalName()) //$NON-NLS-1$
                || usesGivenSignature(mi, Collection.class.getCanonicalName(), "size") || usesGivenSignature(mi, Collection.class.getCanonicalName(), "toArray") //$NON-NLS-1$ $NON-NLS-2$
                || usesGivenSignature(mi, Collection.class.getCanonicalName(), "toArray", argumentType + "[]") //$NON-NLS-1$ $NON-NLS-2$
                || usesGivenSignature(mi, Object.class.getCanonicalName(), "clone") || usesGivenSignature(mi, Object.class.getCanonicalName(), "toString"); //$NON-NLS-1$ $NON-NLS-2$
    }

    @Override
    protected void refactorMethod(ASTBuilder b, MethodInvocation originalMi, MethodInvocation refactoredMi) {
        if (usesGivenSignature(originalMi, Vector.class.getCanonicalName(), "addElement", Object.class.getCanonicalName())) { //$NON-NLS-1$
            refactoredMi.setName(b.simpleName("add")); //$NON-NLS-1$
        } else if (usesGivenSignature(originalMi, Vector.class.getCanonicalName(), "elementAt", int.class.getSimpleName())) { //$NON-NLS-1$
            refactoredMi.setName(b.simpleName("get")); //$NON-NLS-1$
        } else if (usesGivenSignature(originalMi, Vector.class.getCanonicalName(), "copyInto", Object[].class.getCanonicalName())) { //$NON-NLS-1$
            refactoredMi.setName(b.simpleName("toArray")); //$NON-NLS-1$
        } else if (usesGivenSignature(originalMi, Vector.class.getCanonicalName(), "removeElement", Object.class.getCanonicalName()) //$NON-NLS-1$
                || usesGivenSignature(originalMi, Vector.class.getCanonicalName(), "removeElementAt", int.class.getSimpleName())) { //$NON-NLS-1$
            refactoredMi.setName(b.simpleName("remove")); //$NON-NLS-1$
        } else if (usesGivenSignature(originalMi, Vector.class.getCanonicalName(), "removeAllElements")) { //$NON-NLS-1$
            refactoredMi.setName(b.simpleName("clear")); //$NON-NLS-1$
        } else if (usesGivenSignature(originalMi, Vector.class.getCanonicalName(), "insertElementAt", Object.class.getCanonicalName(), int.class.getSimpleName())) { //$NON-NLS-1$
            refactoredMi.setName(b.simpleName("add")); //$NON-NLS-1$
            reorderArguments(refactoredMi);
        } else if (usesGivenSignature(originalMi, Vector.class.getCanonicalName(), "setElementAt", Object.class.getCanonicalName(), int.class.getSimpleName())) { //$NON-NLS-1$
            refactoredMi.setName(b.simpleName("set")); //$NON-NLS-1$
            reorderArguments(refactoredMi);
        }
    }

    private void reorderArguments(final MethodInvocation refactoredMi) {
        List<Expression> args= arguments(refactoredMi);
        Expression item= args.get(0);
        Expression index= args.get(1);
        args.clear();
        args.add(index);
        args.add(item);

        final List<Type> typeArgs= typeArguments(refactoredMi);
        if (typeArgs != null && !typeArgs.isEmpty()) {
            Type itemType= typeArgs.get(0);
            Type indexType= typeArgs.get(1);
            typeArgs.clear();
            typeArgs.add(indexType);
            typeArgs.add(itemType);
        }
    }

    @Override
    protected boolean isTypeCompatible(final ITypeBinding variableType, final ITypeBinding refType) {
        return super.isTypeCompatible(variableType, refType) || hasType(variableType,
                getOrDefault(CAN_BE_CASTED_TO, refType.getErasure().getQualifiedName(), new String[0]));
    }
}

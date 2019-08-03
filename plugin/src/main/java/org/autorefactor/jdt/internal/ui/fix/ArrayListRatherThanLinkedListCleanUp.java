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

import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.hasType;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.isMethod;
import static org.autorefactor.util.Utils.getOrDefault;

import java.io.Serializable;
import java.util.AbstractCollection;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;

/** See {@link #getDescription()} method. */
public class ArrayListRatherThanLinkedListCleanUp extends AbstractClassSubstituteCleanUp {
    private static final Map<String, String[]> CAN_BE_CASTED_TO= new HashMap<String, String[]>();

    static {
        CAN_BE_CASTED_TO.put(Object.class.getCanonicalName(), new String[] { Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(Cloneable.class.getCanonicalName(), new String[] { Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(Serializable.class.getCanonicalName(), new String[] { Serializable.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(Collection.class.getCanonicalName(), new String[] { Collection.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(List.class.getCanonicalName(), new String[] { List.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(AbstractList.class.getCanonicalName(), new String[] { AbstractList.class.getCanonicalName(), List.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(AbstractCollection.class.getCanonicalName(), new String[] { AbstractCollection.class.getCanonicalName(), Collection.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(LinkedList.class.getCanonicalName(), new String[] { LinkedList.class.getCanonicalName(), AbstractList.class.getCanonicalName(), List.class.getCanonicalName(), AbstractCollection.class.getCanonicalName(), Collection.class.getCanonicalName(), Serializable.class.getCanonicalName(), Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
    }

    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_ArrayListRatherThanLinkedListCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_ArrayListRatherThanLinkedListCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_ArrayListRatherThanLinkedListCleanUp_reason;
    }

    @Override
    protected String[] getExistingClassCanonicalName() {
        return new String[] { LinkedList.class.getCanonicalName() };
    }

    @Override
    public Set<String> getClassesToImport() {
        return new HashSet<String>(Arrays.asList(ArrayList.class.getCanonicalName()));
    }

    @Override
    protected String getSubstitutingClassName(String origRawType) {
        if (LinkedList.class.getCanonicalName().equals(origRawType)) {
            return ArrayList.class.getCanonicalName();
        } else {
            return null;
        }
    }

    @Override
    protected boolean canMethodBeRefactored(MethodInvocation mi, List<MethodInvocation> methodCallsToRefactor) {
        final String argumentType= getArgumentType(mi);
        return isMethod(mi, Collection.class.getCanonicalName(), "add", Object.class.getCanonicalName()) //$NON-NLS-1$
                || isMethod(mi, Collection.class.getCanonicalName(), "addAll", Collection.class.getCanonicalName()) //$NON-NLS-1$
                || isMethod(mi, Collection.class.getCanonicalName(), "clear") //$NON-NLS-1$
                || isMethod(mi, Collection.class.getCanonicalName(), "contains", Object.class.getCanonicalName()) //$NON-NLS-1$
                || isMethod(mi, Collection.class.getCanonicalName(), "containsAll", Collection.class.getCanonicalName()) //$NON-NLS-1$
                || isMethod(mi, Object.class.getCanonicalName(), "equals", Object.class.getCanonicalName()) //$NON-NLS-1$
                || isMethod(mi, List.class.getCanonicalName(), "get", int.class.getSimpleName()) || isMethod(mi, Object.class.getCanonicalName(), "hashCode") //$NON-NLS-1$ $NON-NLS-2$
                || isMethod(mi, List.class.getCanonicalName(), "indexOf", Object.class.getCanonicalName()) //$NON-NLS-1$
                || isMethod(mi, List.class.getCanonicalName(), "lastIndexOf", Object.class.getCanonicalName()) //$NON-NLS-1$
                || isMethod(mi, Collection.class.getCanonicalName(), "size") //$NON-NLS-1$
                || isMethod(mi, List.class.getCanonicalName(), "subList", int.class.getSimpleName(), int.class.getSimpleName()) //$NON-NLS-1$
                || isMethod(mi, Collection.class.getCanonicalName(), "toArray") //$NON-NLS-1$
                || isMethod(mi, Collection.class.getCanonicalName(), "toArray", argumentType + "[]") //$NON-NLS-1$ $NON-NLS-2$
                || isMethod(mi, Collection.class.getCanonicalName(), "isEmpty") || isMethod(mi, Object.class.getCanonicalName(), "toString") //$NON-NLS-1$ $NON-NLS-2$
                || isMethod(mi, Object.class.getCanonicalName(), "finalize") || isMethod(mi, Object.class.getCanonicalName(), "notify") //$NON-NLS-1$ $NON-NLS-2$
                || isMethod(mi, Object.class.getCanonicalName(), "notifyAll") || isMethod(mi, Object.class.getCanonicalName(), "wait") //$NON-NLS-1$ $NON-NLS-2$
                || isMethod(mi, Object.class.getCanonicalName(), "wait", long.class.getSimpleName()) //$NON-NLS-1$
                || isMethod(mi, Object.class.getCanonicalName(), "wait", long.class.getSimpleName(), int.class.getSimpleName()); //$NON-NLS-1$
    }

    @Override
    protected boolean isTypeCompatible(final ITypeBinding variableType, final ITypeBinding refType) {
        return super.isTypeCompatible(variableType, refType) || hasType(variableType,
                getOrDefault(CAN_BE_CASTED_TO, refType.getErasure().getQualifiedName(), new String[0]));
    }
}

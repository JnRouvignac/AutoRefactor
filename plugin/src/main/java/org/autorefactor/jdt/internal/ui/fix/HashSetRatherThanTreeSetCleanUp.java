/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Initial API and implementation
 * Copyright (C) 2018 Jean-Noël Rouvignac - minor changes
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

import java.io.Serializable;
import java.util.AbstractCollection;
import java.util.AbstractSet;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;

import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.*;
import static org.autorefactor.util.Utils.*;

/** See {@link #getDescription()} method. */
public class HashSetRatherThanTreeSetCleanUp extends AbstractClassSubstituteCleanUp {
    private static final Map<String, String[]> CAN_BE_CASTED_TO= new HashMap<String, String[]>();

    static {
        CAN_BE_CASTED_TO.put(Object.class.getCanonicalName(), new String[] { Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(Cloneable.class.getCanonicalName(), new String[] { Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(Serializable.class.getCanonicalName(), new String[] { Serializable.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(Collection.class.getCanonicalName(), new String[] { Collection.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(AbstractCollection.class.getCanonicalName(), new String[] { AbstractCollection.class.getCanonicalName(), Collection.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(AbstractSet.class.getCanonicalName(), new String[] { AbstractSet.class.getCanonicalName(), Set.class.getCanonicalName(), AbstractCollection.class.getCanonicalName(), Collection.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(TreeSet.class.getCanonicalName(), new String[] { TreeSet.class.getCanonicalName(), AbstractSet.class.getCanonicalName(), Set.class.getCanonicalName(), AbstractCollection.class.getCanonicalName(), Collection.class.getCanonicalName(), Serializable.class.getCanonicalName(), Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
    }

    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_HashSetRatherThanTreeSetCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_HashSetRatherThanTreeSetCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_HashSetRatherThanTreeSetCleanUp_reason;
    }

    @Override
    protected String[] getExistingClassCanonicalName() {
        return new String[] { TreeSet.class.getCanonicalName() };
    }

    @Override
    public Set<String> getClassesToImport() {
        return new HashSet<String>(Arrays.asList(HashSet.class.getCanonicalName()));
    }

    @Override
    protected String getSubstitutingClassName(String origRawType) {
        if (TreeSet.class.getCanonicalName().equals(origRawType)) {
            return HashSet.class.getCanonicalName();
        } else {
            return null;
        }
    }

    @Override
    protected boolean canInvokeIterator() {
        return false;
    }

    @Override
    protected boolean canInstantiationBeRefactored(final ClassInstanceCreation instanceCreation) {
        return instanceCreation.arguments().size() != 1
                || !hasType((Expression) instanceCreation.arguments().get(0), Comparator.class.getCanonicalName());
    }

    @Override
    protected boolean canMethodBeRefactored(final MethodInvocation mi,
            final List<MethodInvocation> methodCallsToRefactor) {
        return isMethod(mi, Collection.class.getCanonicalName(), "add", Object.class.getCanonicalName()) //$NON-NLS-1$
                || isMethod(mi, Collection.class.getCanonicalName(), "clear") //$NON-NLS-1$
                || isMethod(mi, Collection.class.getCanonicalName(), "contains", Object.class.getCanonicalName()) //$NON-NLS-1$
                || isMethod(mi, Collection.class.getCanonicalName(), "isEmpty") //$NON-NLS-1$
                || isMethod(mi, Collection.class.getCanonicalName(), "remove", Object.class.getCanonicalName()) //$NON-NLS-1$
                || isMethod(mi, Collection.class.getCanonicalName(), "size") //$NON-NLS-1$
                || isMethod(mi, Collection.class.getCanonicalName(), "removeAll", Collection.class.getCanonicalName()) //$NON-NLS-1$
                || isMethod(mi, Collection.class.getCanonicalName(), "addAll", Collection.class.getCanonicalName()) //$NON-NLS-1$
                || isMethod(mi, Collection.class.getCanonicalName(), "containsAll", Collection.class.getCanonicalName()) //$NON-NLS-1$
                || isMethod(mi, Collection.class.getCanonicalName(), "retainAll", Collection.class.getCanonicalName()) //$NON-NLS-1$
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

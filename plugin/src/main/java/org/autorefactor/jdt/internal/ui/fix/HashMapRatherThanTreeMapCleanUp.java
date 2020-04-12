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

import java.io.Serializable;
import java.util.AbstractMap;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;

/** See {@link #getDescription()} method. */
public class HashMapRatherThanTreeMapCleanUp extends AbstractClassSubstituteCleanUp {
	private static final Map<String, String[]> CAN_BE_CASTED_TO= new HashMap<>();

	static {
		CAN_BE_CASTED_TO.put(Object.class.getCanonicalName(), new String[] { Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(Cloneable.class.getCanonicalName(), new String[] { Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(Serializable.class.getCanonicalName(), new String[] { Serializable.class.getCanonicalName(), Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(Map.class.getCanonicalName(), new String[] { Map.class.getCanonicalName(), Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(AbstractMap.class.getCanonicalName(), new String[] { AbstractMap.class.getCanonicalName(), Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(TreeMap.class.getCanonicalName(), new String[] { TreeMap.class.getCanonicalName(), Serializable.class.getCanonicalName(), Map.class.getCanonicalName(), AbstractMap.class.getCanonicalName(), Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
	}

	/**
	 * Get the name.
	 *
	 * @return the name.
	 */
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_HashMapRatherThanTreeMapCleanUp_name;
	}

	/**
	 * Get the description.
	 *
	 * @return the description.
	 */
	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_HashMapRatherThanTreeMapCleanUp_description;
	}

	/**
	 * Get the reason.
	 *
	 * @return the reason.
	 */
	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_HashMapRatherThanTreeMapCleanUp_reason;
	}

	@Override
	protected String[] getExistingClassCanonicalName() {
		return new String[] { TreeMap.class.getCanonicalName() };
	}

	@Override
	public Set<String> getClassesToImport() {
		return new HashSet<>(Arrays.asList(HashMap.class.getCanonicalName()));
	}

	@Override
	protected String getSubstitutingClassName(final String origRawType) {
		if (TreeMap.class.getCanonicalName().equals(origRawType)) {
			return HashMap.class.getCanonicalName();
		}

		return null;
	}

	@Override
	protected boolean canInstantiationBeRefactored(final ClassInstanceCreation instanceCreation) {
		return instanceCreation.arguments().size() != 1
				|| !ASTNodes.hasType((Expression) instanceCreation.arguments().get(0), Comparator.class.getCanonicalName());
	}

	@Override
	protected boolean canMethodBeRefactored(final MethodInvocation mi,
			final List<MethodInvocation> methodCallsToRefactor) {
		return ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "clear") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "containsKey", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "containsValue", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "get", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "getOrDefault", Object.class.getCanonicalName(), Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "isEmpty") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "put", Object.class.getCanonicalName(), Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "putAll", Map.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "putIfAbsent", Object.class.getCanonicalName(), Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "remove", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "remove", Object.class.getCanonicalName(), Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "replace", Object.class.getCanonicalName(), Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "replace", Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "size") || ASTNodes.usesGivenSignature(mi, Object.class.getCanonicalName(), "finalize") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(mi, Object.class.getCanonicalName(), "notify") || ASTNodes.usesGivenSignature(mi, Object.class.getCanonicalName(), "notifyAll") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(mi, Object.class.getCanonicalName(), "wait") || ASTNodes.usesGivenSignature(mi, Object.class.getCanonicalName(), "wait", long.class.getSimpleName()) //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(mi, Object.class.getCanonicalName(), "wait", long.class.getSimpleName(), int.class.getSimpleName()); //$NON-NLS-1$
	}

	@Override
	protected boolean isTypeCompatible(final ITypeBinding variableType, final ITypeBinding refType) {
		return super.isTypeCompatible(variableType, refType) || ASTNodes.hasType(variableType,
				CAN_BE_CASTED_TO.getOrDefault(refType.getErasure().getQualifiedName(), new String[0]));
	}
}

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
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;

/** See {@link #getDescription()} method. */
public class HashMapRatherThanHashtableCleanUp extends AbstractClassSubstituteCleanUp {
	private static final Map<String, String[]> CAN_BE_CASTED_TO= new HashMap<>();

	static {
		CAN_BE_CASTED_TO.put(Object.class.getCanonicalName(), new String[] { Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(Cloneable.class.getCanonicalName(), new String[] { Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(Serializable.class.getCanonicalName(), new String[] { Serializable.class.getCanonicalName(), Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(Map.class.getCanonicalName(), new String[] { Map.class.getCanonicalName(), Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(Hashtable.class.getCanonicalName(), new String[] { Hashtable.class.getCanonicalName(), Serializable.class.getCanonicalName(), Map.class.getCanonicalName(), Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
	}

	@Override
	public String getName() {
		return MultiFixMessages.HashMapRatherThanHashtableCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.HashMapRatherThanHashtableCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.HashMapRatherThanHashtableCleanUp_reason;
	}

	@Override
	public boolean isJavaVersionSupported(final Release javaSeRelease) {
		return javaSeRelease.getMinorVersion() >= 2;
	}

	@Override
	protected boolean canBeSharedInOtherThread() {
		return false;
	}

	@Override
	protected String[] getExistingClassCanonicalName() {
		return new String[] { Hashtable.class.getCanonicalName() };
	}

	@Override
	public Set<String> getClassesToImport() {
		return new HashSet<>(Arrays.asList(HashMap.class.getCanonicalName()));
	}

	@Override
	protected String getSubstitutingClassName(final String origRawType) {
		if (Hashtable.class.getCanonicalName().equals(origRawType)) {
			return HashMap.class.getCanonicalName();
		}

		return null;
	}

	@Override
	protected boolean canMethodBeRefactored(final MethodInvocation methodInvocation,
			final List<MethodInvocation> methodCallsToRefactor) {
		if (ASTNodes.usesGivenSignature(methodInvocation, Hashtable.class.getCanonicalName(), "contains", Object.class.getCanonicalName())) { //$NON-NLS-1$
			methodCallsToRefactor.add(methodInvocation);
		}

		return true;
	}

	@Override
	protected void refactorMethod(final MethodInvocation originalMi, final MethodInvocation refactoredMi) {
		refactoredMi.setName(cuRewrite.getASTBuilder().newSimpleName("containsValue")); //$NON-NLS-1$
	}

	@Override
	protected boolean isTypeCompatible(final ITypeBinding variableType, final ITypeBinding refType) {
		return super.isTypeCompatible(variableType, refType) || ASTNodes.hasType(variableType,
				CAN_BE_CASTED_TO.getOrDefault(refType.getErasure().getQualifiedName(), new String[0]));
	}
}

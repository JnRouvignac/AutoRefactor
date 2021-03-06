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

import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;

/** See {@link #getDescription()} method. */
public class SetRatherThanListCleanUp extends AbstractClassSubstituteCleanUp {
	private final class RefactoringWithObjectsClass extends CleanUpWithNewClassImport {
		@Override
		public boolean visit(final Block visited) {
			isContainsMethodUsed= false;
			return maybeRefactorBlock(visited,
					getClassesToUseWithImport(), getImportsToAdd());
		}
	}

	@Override
	public RefactoringWithObjectsClass getRefactoringClassInstance() {
		return new RefactoringWithObjectsClass();
	}

	private static final Map<String, String[]> CAN_BE_CASTED_TO= new HashMap<>();

	static {
		CAN_BE_CASTED_TO.put(Object.class.getCanonicalName(), new String[] { Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(Cloneable.class.getCanonicalName(),
				new String[] { Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(Serializable.class.getCanonicalName(),
				new String[] { Serializable.class.getCanonicalName(), Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(Collection.class.getCanonicalName(),
				new String[] { Collection.class.getCanonicalName(), Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(List.class.getCanonicalName(),
				new String[] { List.class.getCanonicalName(), Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(AbstractList.class.getCanonicalName(),
				new String[] { AbstractList.class.getCanonicalName(), List.class.getCanonicalName(),
						Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(AbstractCollection.class.getCanonicalName(),
				new String[] { AbstractCollection.class.getCanonicalName(), Collection.class.getCanonicalName(),
						Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(LinkedList.class.getCanonicalName(),
				new String[] { LinkedList.class.getCanonicalName(), AbstractList.class.getCanonicalName(),
						List.class.getCanonicalName(), AbstractCollection.class.getCanonicalName(),
						Collection.class.getCanonicalName(), Serializable.class.getCanonicalName(),
						Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(ArrayList.class.getCanonicalName(),
				new String[] { ArrayList.class.getCanonicalName(), AbstractList.class.getCanonicalName(),
						List.class.getCanonicalName(), AbstractCollection.class.getCanonicalName(),
						Collection.class.getCanonicalName(), Serializable.class.getCanonicalName(),
						Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
	}

	private boolean isContainsMethodUsed;

	@Override
	public String getName() {
		return MultiFixMessages.SetRatherThanListCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.SetRatherThanListCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.SetRatherThanListCleanUp_reason;
	}

	@Override
	public boolean visit(final Block visited) {
		isContainsMethodUsed= false;
		return super.visit(visited);
	}

	@Override
	protected String[] getExistingClassCanonicalName() {
		return new String[] { ArrayList.class.getCanonicalName(), LinkedList.class.getCanonicalName() };
	}

	@Override
	public Set<String> getClassesToImport() {
		return new HashSet<>(Arrays.asList(HashSet.class.getCanonicalName(), Set.class.getCanonicalName()));
	}

	@Override
	protected String getSubstitutingClassName(final String origRawType) {
		if (ArrayList.class.getCanonicalName().equals(origRawType)
				|| LinkedList.class.getCanonicalName().equals(origRawType)) {
			return HashSet.class.getCanonicalName();
		}
		if (AbstractList.class.getCanonicalName().equals(origRawType)
				|| List.class.getCanonicalName().equals(origRawType)) {
			return Set.class.getCanonicalName();
		}

		return null;
	}

	@Override
	protected boolean canInvokeIterator() {
		return false;
	}

	@Override
	protected boolean canCodeBeRefactored() {
		return isContainsMethodUsed;
	}

	@Override
	protected boolean canMethodBeRefactored(final MethodInvocation methodInvocation,
			final List<MethodInvocation> methodCallsToRefactor) {
		if (ASTNodes.usesGivenSignature(methodInvocation, Collection.class.getCanonicalName(), "contains", //$NON-NLS-1$
				Object.class.getCanonicalName())) {
			isContainsMethodUsed= true;
		}

		if (ASTNodes.usesGivenSignature(methodInvocation, List.class.getCanonicalName(), "add", //$NON-NLS-1$
				int.class.getSimpleName(), Object.class.getCanonicalName())
				|| ASTNodes.usesGivenSignature(methodInvocation, List.class.getCanonicalName(), "addAll", //$NON-NLS-1$
						int.class.getSimpleName(), Collection.class.getCanonicalName())) {
			methodCallsToRefactor.add(methodInvocation);
			return true;
		}

		return ASTNodes.usesGivenSignature(methodInvocation, Collection.class.getCanonicalName(), "add", //$NON-NLS-1$
				Object.class.getCanonicalName())
				|| ASTNodes.usesGivenSignature(methodInvocation, Collection.class.getCanonicalName(), "addAll", //$NON-NLS-1$
						Collection.class.getCanonicalName())
				|| ASTNodes.usesGivenSignature(methodInvocation, Collection.class.getCanonicalName(), "clear") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Collection.class.getCanonicalName(), "contains", //$NON-NLS-1$
						Object.class.getCanonicalName())
				|| ASTNodes.usesGivenSignature(methodInvocation, Collection.class.getCanonicalName(), "isEmpty") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "finalize") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "notify") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "notifyAll") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "wait") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "wait", //$NON-NLS-1$
						long.class.getSimpleName())
				|| ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "wait", //$NON-NLS-1$
						long.class.getSimpleName(), int.class.getSimpleName());
	}

	@Override
	protected void refactorMethod(final MethodInvocation originalMi, final MethodInvocation refactoredMi) {
		if (ASTNodes.usesGivenSignature(originalMi, List.class.getCanonicalName(), "add", int.class.getSimpleName(), //$NON-NLS-1$
				Object.class.getCanonicalName())
				|| ASTNodes.usesGivenSignature(originalMi, List.class.getCanonicalName(), "addAll", //$NON-NLS-1$
						int.class.getSimpleName(), Collection.class.getCanonicalName())) {
			List<Expression> args= refactoredMi.arguments();
			Expression item= args.get(1);
			args.clear();
			args.add(item);
		}
	}

	@Override
	protected boolean isTypeCompatible(final ITypeBinding variableType, final ITypeBinding refType) {
		return super.isTypeCompatible(variableType, refType) || ASTNodes.hasType(variableType,
				CAN_BE_CASTED_TO.getOrDefault(refType.getErasure().getQualifiedName(), new String[0]));
	}
}

/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - initial API and implementation
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class CollectionsAddAllRatherThanAsListCleanUp extends NewClassImportCleanUp {
	private static final String ADD_ALL_METHOD= "addAll"; //$NON-NLS-1$
	private static final String AS_LIST_METHOD= "asList"; //$NON-NLS-1$
	private static final String OF_METHOD= "of"; //$NON-NLS-1$

	private final class RefactoringWithObjectsClass extends CleanUpWithNewClassImport {
		@Override
		public boolean visit(final MethodInvocation visited) {
			return maybeRefactorMethodInvocation(visited, getClassesToUseWithImport(), getImportsToAdd());
		}
	}

	@Override
	public String getName() {
		return MultiFixMessages.CollectionsAddAllRatherThanAsListCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CollectionsAddAllRatherThanAsListCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CollectionsAddAllRatherThanAsListCleanUp_reason;
	}

	@Override
	public boolean isJavaVersionSupported(final Release javaSeRelease) {
		return javaSeRelease.getMinorVersion() >= 5;
	}

	@Override
	public RefactoringWithObjectsClass getRefactoringClassInstance() {
		return new RefactoringWithObjectsClass();
	}

	@Override
	public Set<String> getClassesToImport() {
		return new HashSet<>(Arrays.asList(Collections.class.getCanonicalName()));
	}

	@Override
	public boolean visit(final MethodInvocation visited) {
		return maybeRefactorMethodInvocation(visited, getAlreadyImportedClasses(visited), new HashSet<String>());
	}

	private boolean maybeRefactorMethodInvocation(final MethodInvocation visited, final Set<String> classesToUseWithImport, final Set<String> importsToAdd) {
		if (visited.getExpression() != null
				&& !ASTNodes.is(visited.getExpression(), ThisExpression.class)
				&& ASTNodes.usesGivenSignature(visited, Collection.class.getCanonicalName(), ADD_ALL_METHOD, Collection.class.getCanonicalName())) {
			MethodInvocation asListMethod= ASTNodes.as((Expression) visited.arguments().get(0), MethodInvocation.class);

			if (asListMethod != null
					&& (usesGivenVarArgSignature(asListMethod, Arrays.class.getCanonicalName(), AS_LIST_METHOD) || usesGivenVarArgSignature(asListMethod, Set.class.getCanonicalName(), OF_METHOD) && ASTNodes.hasType(visited.getExpression(), Set.class.getCanonicalName()))) {
				refactorMethod(visited, asListMethod, classesToUseWithImport, importsToAdd);
				return false;
			}
		}

		return true;
	}

	private boolean usesGivenVarArgSignature(final MethodInvocation actualMethod, final String className, final String methodName) {
		IMethodBinding binding= actualMethod.resolveMethodBinding();
		return binding != null
				&& ASTNodes.hasType(binding.getDeclaringClass(), className)
				&& Utils.equalNotNull(methodName, actualMethod.getName().getIdentifier())
				&& binding.getParameterTypes() != null
				&& binding.getParameterTypes().length == 1
				&& binding.getParameterTypes()[0].isArray();
	}

	private void refactorMethod(final MethodInvocation visited, final MethodInvocation asListMethod, final Set<String> classesToUseWithImport, final Set<String> importsToAdd) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.CollectionsAddAllRatherThanAsListCleanUp_description);

		String collectionsName= addImport(Collections.class, classesToUseWithImport, importsToAdd);

		List<Expression> copyOfArguments= new ArrayList<>(asListMethod.arguments().size() + 1);
		copyOfArguments.add(ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(visited.getExpression())));

		for (Object argument : asListMethod.arguments()) {
			copyOfArguments.add(ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression((Expression) argument)));
		}

		MethodInvocation newMethodInvocation= ast.newMethodInvocation();
		newMethodInvocation.setExpression(ASTNodeFactory.newName(ast, collectionsName));
		newMethodInvocation.setName(ast.newSimpleName(ADD_ALL_METHOD));
		newMethodInvocation.arguments().addAll(copyOfArguments);

		MethodInvocation newCollectionsAddAllMethod= newMethodInvocation;
		rewrite.replace(visited, newCollectionsAddAllMethod, group);
	}
}

/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2019 Fabrice Tiercelin - initial API and implementation
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

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.ForLoops;
import org.autorefactor.jdt.internal.corext.dom.ForLoops.ContainerType;
import org.autorefactor.jdt.internal.corext.dom.ForLoops.ForLoopContent;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.eclipse.jdt.core.dom.ArrayAccess;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class FillRatherThanLoopCleanUp extends NewClassImportCleanUp {
	private final class RefactoringWithObjectsClass extends CleanUpWithNewClassImport {
		@Override
		public boolean visit(final ForStatement node) {
			return maybeRefactorForStatement(node,
					getClassesToUseWithImport(), getImportsToAdd());
		}
	}

	@Override
	public String getName() {
		return MultiFixMessages.FillRatherThanLoopCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.FillRatherThanLoopCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.FillRatherThanLoopCleanUp_reason;
	}

	@Override
	public RefactoringWithObjectsClass getRefactoringClassInstance() {
		return new RefactoringWithObjectsClass();
	}

	@Override
	public Set<String> getClassesToImport() {
		return new HashSet<>(Arrays.asList(Arrays.class.getCanonicalName()));
	}

	@Override
	public boolean isJavaVersionSupported(final Release javaSeRelease) {
		return javaSeRelease.getMinorVersion() >= 2;
	}

	@Override
	public boolean visit(final ForStatement node) {
		return maybeRefactorForStatement(node, getAlreadyImportedClasses(node), new HashSet<String>());
	}

	private boolean maybeRefactorForStatement(final ForStatement node, final Set<String> classesToUseWithImport,
			final Set<String> importsToAdd) {
		ForLoopContent loopContent= ForLoops.iterateOverContainer(node);
		List<Statement> statements= ASTNodes.asList(node.getBody());

		if (loopContent != null && loopContent.getLoopVariable() != null && loopContent.getContainerType() == ContainerType.ARRAY && statements.size() == 1) {
			Assignment assignment= ASTNodes.asExpression(statements.get(0), Assignment.class);

			if (ASTNodes.hasOperator(assignment, Assignment.Operator.ASSIGN) && ASTNodes.isHardCoded(assignment.getRightHandSide()) && ASTNodes.isPassive(assignment.getRightHandSide())) {
				ArrayAccess arrayAccess= ASTNodes.as(assignment.getLeftHandSide(), ArrayAccess.class);

				if (arrayAccess != null && isSameVariable(loopContent, arrayAccess)) {
					replaceWithArraysFill(node, classesToUseWithImport, importsToAdd, assignment, arrayAccess);
					return false;
				}
			}
		}

		return true;
	}

	private void replaceWithArraysFill(final ForStatement node, final Set<String> classesToUseWithImport,
			final Set<String> importsToAdd, final Assignment assignment, final ArrayAccess arrayAccess) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.FillRatherThanLoopCleanUp_description);

		String classname= addImport(Arrays.class, classesToUseWithImport, importsToAdd);
		ASTNodes.replaceButKeepComment(rewrite, node,
				ast.newExpressionStatement(ast.newMethodInvocation(ast.newName(classname),
						"fill", ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(arrayAccess.getArray())), //$NON-NLS-1$
						ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(assignment.getRightHandSide())))), group);
	}

	private boolean isSameVariable(final ForLoopContent loopContent, final ArrayAccess arrayAccess) {
		return arrayAccess != null && ASTNodes.isSameVariable(arrayAccess.getArray(), loopContent.getContainerVariable())
				&& ASTNodes.isSameLocalVariable(arrayAccess.getIndex(), loopContent.getLoopVariable());
	}
}

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

import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.InterruptibleVisitor;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Annotation;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IExtendedModifier;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class StaticInnerClassThanNonStaticCleanUp extends AbstractCleanUpRule {
	private static class TopLevelClassMemberVisitor extends InterruptibleVisitor {
		private final TypeDeclaration innerClass;
		private boolean isTopLevelClassMemberUsed;

		public TopLevelClassMemberVisitor(final TypeDeclaration innerClass) {
			this.innerClass= innerClass;
		}

		public boolean isTopLevelClassMemberUsed() {
			return isTopLevelClassMemberUsed;
		}

		@Override
		public boolean visit(final SimpleName node) {
			if (innerClass.getName() == node) {
				return true;
			}

			IBinding binding= node.resolveBinding();
			ASTNode root= node.getRoot();

			if (binding == null || !(root instanceof CompilationUnit)) {
				isTopLevelClassMemberUsed= true;
				return interruptVisit();
			}

			if (!Modifier.isStatic(binding.getModifiers()) && (binding.getKind() == IBinding.VARIABLE || binding.getKind() == IBinding.METHOD)) {
				ASTNode declaration= ((CompilationUnit) root).findDeclaringNode(binding);

				if (!ASTNodes.isParent(declaration, innerClass)) {
					isTopLevelClassMemberUsed= true;
					return interruptVisit();
				}
			}

			return true;
		}
	}

	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_StaticInnerClassThanNonStaticCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_StaticInnerClassThanNonStaticCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_StaticInnerClassThanNonStaticCleanUp_reason;
	}

	@Override
	public boolean visit(final TypeDeclaration node) {
		if (!node.isInterface()) {
			TypeDeclaration parent= ASTNodes.getTypedAncestor(node, TypeDeclaration.class);
			TypeDeclaration topLevelClass= null;

			while (parent != null) {
				topLevelClass= parent;
				parent= ASTNodes.getTypedAncestor(topLevelClass, TypeDeclaration.class);

				if (parent != null && !Modifier.isStatic(topLevelClass.getModifiers())) {
					return true;
				}
			}

			if (topLevelClass != null && !Modifier.isStatic(node.getModifiers())) {
				TopLevelClassMemberVisitor topLevelClassMemberVisitor= new TopLevelClassMemberVisitor(node);
				topLevelClassMemberVisitor.visitNode(node);

				if (!topLevelClassMemberVisitor.isTopLevelClassMemberUsed()) {
					makeStatic(node);
					return false;
				}
			}
		}

		return true;
	}

	private void makeStatic(final TypeDeclaration node) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.CleanUpRefactoringWizard_StaticInnerClassThanNonStaticCleanUp_name);

		List<?> modifiers= node.modifiers();
		Modifier static0= ast.static0();

		if (modifiers.isEmpty()) {
			rewrite.insertBefore(static0, node, group);
		} else {
			IExtendedModifier lastModifier= (IExtendedModifier) Utils.getLast(modifiers);

			if (lastModifier.isModifier()) {
				if (((Modifier) lastModifier).isFinal()) {
					rewrite.insertBefore(static0, (Modifier) lastModifier, group);
				} else {
					rewrite.insertAfter(static0, (Modifier) lastModifier, group);
				}
			} else {
				rewrite.insertAfter(static0, (Annotation) lastModifier, group);
			}
		}
	}
}

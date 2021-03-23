/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2018 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2018 Andrei Paikin - Remove protected modifier for final class not inherited members.
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
import java.util.Comparator;
import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.util.NotImplementedException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Annotation;
import org.eclipse.jdt.core.dom.AnnotationTypeDeclaration;
import org.eclipse.jdt.core.dom.AnnotationTypeMemberDeclaration;
import org.eclipse.jdt.core.dom.BodyDeclaration;
import org.eclipse.jdt.core.dom.EnumDeclaration;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.IExtendedModifier;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.Modifier.ModifierKeyword;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class ObsoleteRedundantModifiersCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.ObsoleteRedundantModifiersCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ObsoleteRedundantModifiersCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ObsoleteRedundantModifiersCleanUp_reason;
	}

	private static final class ModifierOrderComparator implements Comparator<IExtendedModifier> {
		@Override
		public int compare(final IExtendedModifier o1, final IExtendedModifier o2) {
			if (o1.isAnnotation()) {
				if (o2.isAnnotation()) {
					return 0;
				}

				return -1;
			}

			if (o2.isAnnotation()) {
				return 1;
			}

			int i1= ORDERED_MODIFIERS.indexOf(((Modifier) o1).getKeyword());
			int i2= ORDERED_MODIFIERS.indexOf(((Modifier) o2).getKeyword());

			if (i1 == -1) {
				throw new NotImplementedException((Modifier) o1, "cannot determine order for modifier " + o1); //$NON-NLS-1$
			}

			if (i2 == -1) {
				throw new NotImplementedException((Modifier) o2, "cannot compare modifier " + o2); //$NON-NLS-1$
			}

			return i1 - i2;
		}
	}

	private static final List<ModifierKeyword> ORDERED_MODIFIERS= Collections.unmodifiableList(Arrays.asList(
			ModifierKeyword.PUBLIC_KEYWORD, ModifierKeyword.PROTECTED_KEYWORD, ModifierKeyword.PRIVATE_KEYWORD,
			ModifierKeyword.ABSTRACT_KEYWORD, ModifierKeyword.STATIC_KEYWORD, ModifierKeyword.FINAL_KEYWORD,
			ModifierKeyword.TRANSIENT_KEYWORD, ModifierKeyword.VOLATILE_KEYWORD, ModifierKeyword.SYNCHRONIZED_KEYWORD,
			ModifierKeyword.NATIVE_KEYWORD, ModifierKeyword.STRICTFP_KEYWORD));

	@Override
	public boolean visit(final FieldDeclaration visited) {
		if (isInterface(visited.getParent())) {
			return maybeRemovePublicStaticFinalModifiers(visited);
		}

		if (Modifier.isProtected(visited.getModifiers()) && isFinalClass(visited.getParent())) {
			return removeProtectedModifier(visited);
		}

		return ensureModifiersOrder(visited);
	}

	private boolean maybeRemovePublicStaticFinalModifiers(final FieldDeclaration visited) {
		// Remove modifiers implied by the context
		boolean result= true;
		List<IExtendedModifier> modifiers= visited.modifiers();

		for (Modifier modifier : getModifiersOnly(modifiers)) {
			if (modifier.isPublic()
					|| modifier.isStatic()
					|| modifier.isFinal()) {
				TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteRedundantModifiersCleanUp_description);
				ASTRewrite rewrite= cuRewrite.getASTRewrite();

				rewrite.remove(modifier, group);
				result= false;
			}
		}

		return result;
	}

	private boolean isInterface(final ASTNode visited) {
		return visited instanceof TypeDeclaration && ((TypeDeclaration) visited).isInterface();
	}

	private boolean isFinalClass(final ASTNode visited) {
		return visited instanceof TypeDeclaration && Modifier.isFinal(((TypeDeclaration) visited).getModifiers());
	}

	@Override
	public boolean visit(final MethodDeclaration visited) {
		if (isInterface(visited.getParent())) {
			// Remove modifiers implied by the context
			return removePublicAbstractModifiers(visited);
		}

		if (visited.getParent() instanceof EnumDeclaration) {
			return removePrivateModifier(visited);
		}

		int modifiers= visited.getModifiers();

		if (Modifier.isFinal(modifiers) && (isFinalClass(visited.getParent()) || Modifier.isPrivate(modifiers))) {
			return removeFinalModifier(visited.modifiers());
		}

		if (Modifier.isProtected(visited.getModifiers()) && (visited.isConstructor() ? isFinalClass(visited.getParent())
				: isFinalClassWithoutInheritance(visited.getParent()))) {
			return removeProtectedModifier(visited);
		}

		return ensureModifiersOrder(visited);
	}

	private boolean isFinalClassWithoutInheritance(final ASTNode visited) {
		if (visited instanceof TypeDeclaration) {
			TypeDeclaration klass= (TypeDeclaration) visited;
			return isFinalClass(klass)
					&& klass.superInterfaceTypes().isEmpty()
					&& (klass.getSuperclassType() == null || ASTNodes.hasType(klass.getSuperclassType().resolveBinding(), Object.class.getCanonicalName()));
		}

		return false;
	}

	private boolean removeProtectedModifier(final BodyDeclaration visited) {
		List<IExtendedModifier> modifiers= visited.modifiers();

		for (Modifier modifier : getModifiersOnly(modifiers)) {
			if (modifier.isProtected()) {
				TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteRedundantModifiersCleanUp_description);
				ASTRewrite rewrite= cuRewrite.getASTRewrite();

				rewrite.remove(modifier, group);
				return false;
			}
		}

		return true;
	}

	private boolean removePrivateModifier(final BodyDeclaration visited) {
		List<IExtendedModifier> modifiers= visited.modifiers();

		for (Modifier modifier : getModifiersOnly(modifiers)) {
			if (modifier.isPrivate()) {
				TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteRedundantModifiersCleanUp_description);
				ASTRewrite rewrite= cuRewrite.getASTRewrite();

				rewrite.remove(modifier, group);
				return false;
			}
		}

		return true;
	}

	private boolean removePublicAbstractModifiers(final BodyDeclaration visited) {
		List<IExtendedModifier> modifiers= visited.modifiers();
		boolean result= true;

		for (Modifier modifier : getModifiersOnly(modifiers)) {
			if (modifier.isPublic() || modifier.isAbstract()) {
				TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteRedundantModifiersCleanUp_description);
				ASTRewrite rewrite= cuRewrite.getASTRewrite();

				rewrite.remove(modifier, group);
				result= false;
			}
		}

		return result;
	}

	@Override
	public boolean visit(final AnnotationTypeDeclaration visited) {
		return ensureModifiersOrder(visited);
	}

	@Override
	public boolean visit(final AnnotationTypeMemberDeclaration visited) {
		return removePublicAbstractModifiers(visited);
	}

	@Override
	public boolean visit(final EnumDeclaration visited) {
		return removeStaticAbstractModifier(visited.modifiers()) && ensureModifiersOrder(visited);
	}

	@Override
	public boolean visit(final TryStatement visited) {
		boolean result= true;

		for (VariableDeclarationExpression resource : (List<VariableDeclarationExpression>) visited.resources()) {
			result&= removeFinalModifier(resource.modifiers());
		}

		return result;
	}

	@Override
	public boolean visit(final TypeDeclaration visited) {
		return (!isInterface(visited) || removeStaticAbstractModifier(visited.modifiers())) && ensureModifiersOrder(visited);
	}

	private boolean ensureModifiersOrder(final BodyDeclaration visited) {
		List<IExtendedModifier> extendedModifiers= visited.modifiers();
		List<IExtendedModifier> reorderedModifiers= new ArrayList<>(extendedModifiers);
		Collections.sort(reorderedModifiers, new ModifierOrderComparator());

		if (!extendedModifiers.equals(reorderedModifiers)) {
			reorderModifiers(reorderedModifiers);
			return false;
		}

		return true;
	}

	private void reorderModifiers(final List<IExtendedModifier> reorderedModifiers) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteRedundantModifiersCleanUp_description);

		for (int i= 0; i < reorderedModifiers.size(); i++) {
			IExtendedModifier extendedModifier= reorderedModifiers.get(i);

			if (extendedModifier.isModifier()) {
				rewrite.moveToIndex((Modifier) extendedModifier, i, ASTNodes.createMoveTarget(rewrite, (Modifier) extendedModifier), group);
			} else {
				rewrite.moveToIndex((Annotation) extendedModifier, i, ASTNodes.createMoveTarget(rewrite, (Annotation) extendedModifier), group);
			}
		}
	}

	private boolean removeStaticAbstractModifier(final List<IExtendedModifier> modifiers) {
		boolean result= true;

		for (Modifier modifier : getModifiersOnly(modifiers)) {
			if (modifier.isStatic() || modifier.isAbstract()) {
				TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteRedundantModifiersCleanUp_description);
				ASTRewrite rewrite= cuRewrite.getASTRewrite();

				rewrite.remove(modifier, group);
				result= false;
			}
		}

		return result;
	}

	@Override
	public boolean visit(final SingleVariableDeclaration visited) {
		ASTNode parent= visited.getParent();

		if (parent instanceof MethodDeclaration) {
			MethodDeclaration method= (MethodDeclaration) parent;

			if (method.getBody() == null) {
				return removeFinalModifier(visited.modifiers());
			}
		}

		return true;
	}

	private boolean removeFinalModifier(final List<IExtendedModifier> modifiers) {
		boolean result= true;

		for (Modifier modifier : getModifiersOnly(modifiers)) {
			if (modifier.isFinal()) {
				TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteRedundantModifiersCleanUp_description);
				ASTRewrite rewrite= cuRewrite.getASTRewrite();

				rewrite.remove(modifier, group);
				result= false;
			}
		}

		return result;
	}

	private List<Modifier> getModifiersOnly(final Collection<IExtendedModifier> modifiers) {
		List<Modifier> results= new ArrayList<>();

		for (IExtendedModifier extendedModifier : modifiers) {
			if (extendedModifier.isModifier()) {
				results.add((Modifier) extendedModifier);
			}
		}

		return results;
	}
}

/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.refactoring.rules;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import org.autorefactor.refactoring.ASTHelper;
import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.IExtendedModifier;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.TypeDeclaration;

/**
 * Remove modifiers implied by the context:
 * <ul>
 * <li><code>public</code>, <code>static</code> and <code>final</code> for
 * interfaces fields</li>
 * <li><code>public</code> and <code>abstract</code> for interfaces methods</li>
 * <li><code>final</code> for parameters in interface method declarations</li>
 * </ul>
 * TODO JNR can we also fix modifiers order? (for example
 * <code>public static final</code> instead of <code>final static public</code>)
 */
public class RemoveUselessModifiersRefactoring extends ASTVisitor implements
		IJavaRefactoring {

	private RefactoringContext ctx;

	public RemoveUselessModifiersRefactoring() {
		super();
	}

	public void setRefactoringContext(RefactoringContext ctx) {
		this.ctx = ctx;
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean visit(TypeDeclaration node) {
		if (node.isInterface()) {
			for (FieldDeclaration fd : node.getFields()) {
				// remove modifiers implied by the context
				for (Modifier m : getModifiersOnly(fd.modifiers())) {
					if (m.isPublic() || m.isStatic() || m.isFinal()) {
						this.ctx.getRefactorings().remove(m);
					}
				}
			}
			for (MethodDeclaration md : node.getMethods()) {
				// remove modifiers implied by the context
				for (Modifier m : getModifiersOnly(md.modifiers())) {
					if (m.isPublic() || m.isAbstract()) {
						this.ctx.getRefactorings().remove(m);
					}
				}

				// remove useless "final" from method parameters
				for (SingleVariableDeclaration svd : (List<SingleVariableDeclaration>) md
						.parameters()) {
					for (Modifier m : getModifiersOnly(svd.modifiers())) {
						if (m.isFinal()) {
							this.ctx.getRefactorings().remove(m);
						}
					}
				}
			}
		}
		return ASTHelper.DO_NOT_VISIT_SUBTREE;
	}


	private List<Modifier> getModifiersOnly(
			Collection<IExtendedModifier> modifiers) {
		final List<Modifier> results = new LinkedList<Modifier>();
		for (IExtendedModifier em : modifiers) {
			if (em.isModifier()) {
				results.add((Modifier) em);
			}
		}
		return results;
	}

	public Refactorings getRefactorings(CompilationUnit astRoot) {
		astRoot.accept(this);
		return this.ctx.getRefactorings();
	}
}

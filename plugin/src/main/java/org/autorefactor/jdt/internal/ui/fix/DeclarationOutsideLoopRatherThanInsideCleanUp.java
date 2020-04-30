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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.Dimension;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IExtendedModifier;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.jdt.core.dom.WhileStatement;

/** See {@link #getDescription()} method. */
public class DeclarationOutsideLoopRatherThanInsideCleanUp extends AbstractCleanUpRule {
	/**
	 * Get the name.
	 *
	 * @return the name.
	 */
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_DeclarationOutsideLoopRatherThanInsideCleanUp_name;
	}

	/**
	 * Get the description.
	 *
	 * @return the description.
	 */
	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_DeclarationOutsideLoopRatherThanInsideCleanUp_description;
	}

	/**
	 * Get the reason.
	 *
	 * @return the reason.
	 */
	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_DeclarationOutsideLoopRatherThanInsideCleanUp_reason;
	}

	@Override
	public boolean visit(final Block node) {
		List<Statement> blockStatement= ASTNodes.asList(node);
		boolean result= true;

		List<Statement> forStatements;
		for (int i= 0; i < blockStatement.size(); i++) {
			Statement statement= blockStatement.get(i);
			ForStatement forStatement= ASTNodes.as(statement, ForStatement.class);
			EnhancedForStatement enhancedForStatement= ASTNodes.as(statement, EnhancedForStatement.class);
			WhileStatement whileStatement= ASTNodes.as(statement, WhileStatement.class);
			DoStatement doStatement= ASTNodes.as(statement, DoStatement.class);
			forStatements= null;

			if (forStatement != null) {
				forStatements= ASTNodes.asList(forStatement.getBody());
			} else if (enhancedForStatement != null) {
				forStatements= ASTNodes.asList(enhancedForStatement.getBody());
			} else if (whileStatement != null) {
				forStatements= ASTNodes.asList(whileStatement.getBody());
			} else if (doStatement != null) {
				forStatements= ASTNodes.asList(doStatement.getBody());
			}

			if (forStatements != null) {
				Set<SimpleName> varNames= new HashSet<>();

				for (int j= 0; j < i; j++) {
					if (!(blockStatement.get(j) instanceof Block)) {
						varNames.addAll(ASTNodes.getLocalVariableIdentifiers(blockStatement.get(j), false));
					}
				}
				for (int j= i + 1; j < blockStatement.size(); j++) {
					varNames.addAll(ASTNodes.getLocalVariableIdentifiers(blockStatement.get(j), true));
				}

				List<VariableDeclarationStatement> candidates= new ArrayList<>();

				for (Statement declarationStatement : forStatements) {
					VariableDeclarationStatement decl= ASTNodes.as(declarationStatement, VariableDeclarationStatement.class);

					if (decl != null && !Modifier.isFinal(decl.getModifiers()) && !hasAnnotation(decl.modifiers())
							&& decl.fragments() != null && decl.fragments().size() == 1) {
						VariableDeclarationFragment fragment= (VariableDeclarationFragment) decl.fragments()
								.get(0);
						SimpleName name= fragment.getName();
						boolean isFound= false;

						for (SimpleName varName : varNames) {
							if (Utils.equalNotNull(varName.getIdentifier(), name.getIdentifier())) {
								isFound= true;
								break;
							}
						}

						if (!isFound) {
							candidates.add(decl);
							varNames.add(name);
						}
					}
				}

				ASTRewrite rewrite= cuRewrite.getASTRewrite();
				ASTNodeFactory ast= cuRewrite.getASTBuilder();

				for (VariableDeclarationStatement candidate : candidates) {
					moveDeclaration(ast, rewrite, statement, candidate);
					result= false;
				}
			}
		}

		return result;
	}

	@SuppressWarnings("unchecked")
	private boolean hasAnnotation(final List<?> modifiers) {
		for (IExtendedModifier em : (List<IExtendedModifier>) modifiers) {
			if (em.isAnnotation()) {
				return true;
			}
		}

		return false;
	}

	private void moveDeclaration(final ASTNodeFactory ast, final ASTRewrite rewrite, final Statement statement,
			final VariableDeclarationStatement varToMove) {
		VariableDeclarationFragment fragment= (VariableDeclarationFragment) varToMove.fragments().get(0);

		if (fragment.getInitializer() != null) {
			Type copyOfType= ast.createCopyTarget(varToMove.getType());
			SimpleName name= fragment.getName();
			VariableDeclarationFragment newFragment= ast.declareFragment(ast.createCopyTarget(name));
			@SuppressWarnings("unchecked")
			List<Dimension> extraDimensions= fragment.extraDimensions();
			@SuppressWarnings("unchecked")
			List<Dimension> newExtraDimensions= newFragment.extraDimensions();
			newExtraDimensions.addAll(rewrite.createMoveTarget(extraDimensions));
			VariableDeclarationStatement newDeclareStatement= ast.declareStatement(copyOfType, newFragment);
			@SuppressWarnings("unchecked")
			List<IExtendedModifier> modifiers= varToMove.modifiers();
			@SuppressWarnings("unchecked")
			List<IExtendedModifier> newModifiers= newDeclareStatement.modifiers();

			for (IExtendedModifier iExtendedModifier : modifiers) {
				Modifier modifier= (Modifier) iExtendedModifier;

				if (!modifier.isPrivate() && !modifier.isStatic()) {
					newModifiers.add(ASTNodes.createMoveTarget(rewrite, modifier));
				}
			}

			rewrite.insertBefore(newDeclareStatement, statement, null);
			rewrite.replace(varToMove,
					ast.toStatement(ast.assign(ast.createCopyTarget(name), Assignment.Operator.ASSIGN, ASTNodes.createMoveTarget(rewrite, fragment.getInitializer()))), null);
		} else {
			rewrite.insertBefore(ASTNodes.createMoveTarget(rewrite, varToMove), statement, null);
			rewrite.remove(varToMove, null);
		}
	}
}

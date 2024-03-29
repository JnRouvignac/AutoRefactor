/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016-2017 Fabrice Tiercelin - initial API and implementation
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
import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Dimension;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.IExtendedModifier;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class ObsoleteLocalVariableRatherThanFieldCleanUp extends AbstractCleanUpRule {
	private static final class FieldUseVisitor extends ASTVisitor {
		private final SimpleName field;
		private final List<SimpleName> occurrences= new ArrayList<>();

		private FieldUseVisitor(final SimpleName field) {
			this.field= field;
		}

		@Override
		public boolean visit(final SimpleName aVariable) {
			if (field != aVariable
					&& ASTNodes.isSameVariable(field, aVariable)) {
				occurrences.add(aVariable);
			}

			return true;
		}

		private List<SimpleName> getOccurrences() {
			return occurrences;
		}
	}

	@Override
	public String getName() {
		return MultiFixMessages.ObsoleteLocalVariableRatherThanFieldCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ObsoleteLocalVariableRatherThanFieldCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ObsoleteLocalVariableRatherThanFieldCleanUp_reason;
	}

	@Override
	public boolean visit(final TypeDeclaration visited) {
		for (FieldDeclaration field : visited.getFields()) {
			if (!maybeReplaceFieldByLocalVariable(visited, field)) {
				return false;
			}
		}

		return true;
	}

	private boolean maybeReplaceFieldByLocalVariable(final TypeDeclaration visited, final FieldDeclaration field) {
		if (Modifier.isPrivate(field.getModifiers())
				&& !Modifier.isFinal(field.getModifiers())
				&& !hasAnnotation(field)
				&& field.getType().isPrimitiveType()) {
			for (Object object : field.fragments()) {
				VariableDeclarationFragment fragment= (VariableDeclarationFragment) object;

				if (!maybeReplaceFragmentByLocalVariable(visited, field, fragment)) {
					return false;
				}
			}
		}

		return true;
	}

	private boolean maybeReplaceFragmentByLocalVariable(final TypeDeclaration visited, final FieldDeclaration field,
			final VariableDeclarationFragment fragment) {
		if (fragment.getInitializer() != null && !ASTNodes.isPassiveWithoutFallingThrough(fragment.getInitializer())) {
			return true;
		}

		FieldUseVisitor fieldUseVisitor= new FieldUseVisitor(fragment.getName());
		visited.accept(fieldUseVisitor);
		List<SimpleName> occurrences= fieldUseVisitor.getOccurrences();

		MethodDeclaration oneMethodDeclaration= null;

		for (SimpleName occurrence : occurrences) {
			MethodDeclaration currentMethodDeclaration= ASTNodes.getTypedAncestor(occurrence, MethodDeclaration.class);

			if (isVariableDeclaration(occurrence)
					|| isExternalField(occurrence)
					|| currentMethodDeclaration == null
					|| oneMethodDeclaration != null && currentMethodDeclaration != oneMethodDeclaration) {
				return true;
			}

			oneMethodDeclaration= currentMethodDeclaration;
		}

		if (oneMethodDeclaration == null) {
			return true;
		}

		boolean isReassigned= isAlwaysErased(occurrences);

		if (isReassigned) {
			SimpleName reassignment= findReassignment(occurrences);

			if (reassignment != null && reassignment.getParent() instanceof Assignment) {
				replaceFieldByLocalVariable(field, fragment, reassignment);
				return false;
			}
		}

		return true;
	}

	private void replaceFieldByLocalVariable(final FieldDeclaration field, final VariableDeclarationFragment fragment, final SimpleName reassignment) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteLocalVariableRatherThanFieldCleanUp_description);

		boolean isFieldKept= field.fragments().size() != 1;

		Assignment reassignmentAssignment= (Assignment) reassignment.getParent();
		VariableDeclarationFragment newFragment= ast.newVariableDeclarationFragment(ASTNodes.createMoveTarget(rewrite, reassignment), ASTNodes.createMoveTarget(rewrite, reassignmentAssignment.getRightHandSide()));
		List<Dimension> extraDimensions= fragment.extraDimensions();
		List<Dimension> newExtraDimensions= newFragment.extraDimensions();
		newExtraDimensions.addAll(ASTNodes.createMoveTarget(rewrite, extraDimensions));
		VariableDeclarationStatement newDeclareStatement= ast.newVariableDeclarationStatement(isFieldKept ? ASTNodes.createMoveTarget(rewrite, field.getType()) : ast.createCopyTarget(field.getType()), newFragment);
		List<IExtendedModifier> modifiers= field.modifiers();
		List<IExtendedModifier> newModifiers= newDeclareStatement.modifiers();

		for (IExtendedModifier iExtendedModifier : modifiers) {
			Modifier modifier= (Modifier) iExtendedModifier;

			if (!modifier.isPrivate() && !modifier.isStatic()) {
				newModifiers.add(isFieldKept ? ASTNodes.createMoveTarget(rewrite, modifier) : ast.createCopyTarget(modifier));
			}
		}

		ASTNodes.replaceButKeepComment(rewrite, ASTNodes.getTypedAncestor(reassignmentAssignment, Statement.class),
				newDeclareStatement, group);

		if (isFieldKept) {
			rewrite.remove(fragment, group);
			ASTNodes.replaceButKeepComment(rewrite, field.getType(), ast.createCopyTarget(field.getType()), group);
		} else {
			rewrite.remove(field, group);
		}
	}

	private SimpleName findReassignment(final List<SimpleName> occurrences) {
		for (SimpleName reassignment : occurrences) {
			if (isReassigned(reassignment) && isReassignmentForAll(reassignment, occurrences)) {
				return reassignment;
			}
		}

		return null;
	}

	private boolean isReassignmentForAll(final SimpleName reassignment, final List<SimpleName> occurrences) {
		for (SimpleName occurrence : occurrences) {
			if (reassignment != occurrence) {
				Statement statement= ASTNodes.getTypedAncestor(occurrence, Statement.class);
				boolean isReassigned= false;

				while (statement != null) {
					Assignment assignment= ASTNodes.asExpression(statement, Assignment.class);

					if (assignment != null
							&& ASTNodes.hasOperator(assignment, Assignment.Operator.ASSIGN)
							&& assignment.getLeftHandSide() == reassignment) {
						isReassigned= true;
						break;
					}

					statement= ASTNodes.getPreviousStatement(statement);
				}

				if (!isReassigned) {
					return false;
				}
			}
		}

		return true;
	}

	private boolean isAlwaysErased(final List<SimpleName> occurrences) {
		for (SimpleName occurrence : occurrences) {
			if (!isReassigned(occurrence)) {
				Statement statement= ASTNodes.getTypedAncestor(occurrence, Statement.class);
				boolean isReassigned= false;

				while (statement != null) {
					statement= ASTNodes.getPreviousStatement(statement);
					Assignment assignment= ASTNodes.asExpression(statement, Assignment.class);

					if (assignment != null
							&& ASTNodes.hasOperator(assignment, Assignment.Operator.ASSIGN)
							&& ASTNodes.areSameVariables(assignment.getLeftHandSide(), occurrence)) {
						isReassigned= true;
						break;
					}
				}

				if (!isReassigned) {
					return false;
				}
			}
		}

		return true;
	}

	private boolean isReassigned(final SimpleName occurrence) {
		return occurrence.getParent() instanceof Assignment
				&& occurrence.getLocationInParent() == Assignment.LEFT_HAND_SIDE_PROPERTY
				&& ASTNodes.hasOperator((Assignment) occurrence.getParent(), Assignment.Operator.ASSIGN);
	}

	private static boolean isExternalField(final SimpleName occurrence) {
		FieldAccess fieldAccess= ASTNodes.as(occurrence, FieldAccess.class);

		if (fieldAccess != null && ASTNodes.is(fieldAccess.getExpression(), ThisExpression.class)) {
			return true;
		}

		QualifiedName qualifiedName= ASTNodes.as(occurrence, QualifiedName.class);

		return qualifiedName != null;
	}

	private static boolean isVariableDeclaration(final SimpleName occurrence) {
		switch (occurrence.getParent().getNodeType()) {
		case ASTNode.SINGLE_VARIABLE_DECLARATION:
		case ASTNode.VARIABLE_DECLARATION_STATEMENT:
			return occurrence.getLocationInParent() == SingleVariableDeclaration.NAME_PROPERTY;

		case ASTNode.VARIABLE_DECLARATION_EXPRESSION:
			return occurrence.getLocationInParent() == VariableDeclarationExpression.FRAGMENTS_PROPERTY;

		case ASTNode.VARIABLE_DECLARATION_FRAGMENT:
			return occurrence.getLocationInParent() == VariableDeclarationFragment.NAME_PROPERTY;

		default:
			return false;
		}
	}

	private boolean hasAnnotation(final FieldDeclaration field) {
		List<IExtendedModifier> modifiers= field.modifiers();

		for (IExtendedModifier em : modifiers) {
			if (em.isAnnotation()) {
				return true;
			}
		}

		return false;
	}
}

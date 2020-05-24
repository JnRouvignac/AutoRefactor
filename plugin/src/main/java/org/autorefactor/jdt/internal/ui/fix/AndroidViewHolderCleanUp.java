/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Luis Cruz - Android Refactoring
 * Copyright (C) 2016 Jean-Noël Rouvignac - code cleanups
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
import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.TypeNameDecider;
import org.autorefactor.jdt.internal.corext.dom.Variable;
import org.autorefactor.preferences.Preferences;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BodyDeclaration;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SwitchStatement;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/**
 * TODO when findViewById is reusing a local variable, the viewHolderItem will
 * create a new field with duplicate name.
 * <P>
 * Possible solution: use the id names instead of var names
 * <P>
 * TODO sometimes convertView is a final param in getView.
 * <P>
 * Possible solution: remove final modifier
 *
 * @see {@link #getDescription()} method.
 */
public class AndroidViewHolderCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_AndroidViewHolderCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_AndroidViewHolderCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_AndroidViewHolderCleanUp_reason;
	}

	@Override
	public boolean isEnabled(final Preferences preferences) {
		// FIXME enable only when android libraries are detected
		return super.isEnabled(preferences);
	}

	@Override
	public boolean visit(final MethodDeclaration node) {
		Block body= node.getBody();
		if (body != null && ASTNodes.usesGivenSignature(node, "android.widget.Adapter", "getView", int.class.getSimpleName(), "android.view.View", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				"android.view.ViewGroup")) { //$NON-NLS-1$
			GetViewVariableVisitor visitor= new GetViewVariableVisitor();
			body.accept(visitor);
			if (visitor.canApplyRefactoring()) {
				ASTRewrite rewrite= cuRewrite.getASTRewrite();
				ASTNodeFactory ast= cuRewrite.getASTBuilder();

				TypeNameDecider typeNameDecider= new TypeNameDecider(visitor.viewVariableName);

				// Transform tree

				// Create If statement
				SingleVariableDeclaration viewArg= (SingleVariableDeclaration) node.parameters().get(1);
				Variable convertViewVar= new Variable(viewArg.getName().getIdentifier(), ast);
				InfixExpression condition= ast.infixExpression(convertViewVar.varName(), InfixExpression.Operator.EQUALS, ast.null0());
				Block thenBlock= ast.block();
				IfStatement ifStatement= ast.if0(condition, thenBlock);
				rewrite.insertBefore(ifStatement, visitor.viewAssignmentStatement, null);
				@SuppressWarnings("unchecked")
				List<Statement> thenStatements= thenBlock.statements();

				thenStatements.add(ast.toStatement(ast.assign(convertViewVar.varName(), Assignment.Operator.ASSIGN, ast.createCopyTarget(visitor.getInflateExpression()))));

				// Assign to local view variable when necessary
				if (!"convertView".equals(visitor.viewVariableName.getIdentifier())) { //$NON-NLS-1$
					Statement assignConvertViewToView= null;
					if (visitor.viewVariableDeclFragment != null) {
						assignConvertViewToView= ast.declareStatement(ast.copyType(visitor.viewVariableName, typeNameDecider),
								ast.createCopyTarget(visitor.viewVariableName), convertViewVar.varName());
					} else if (visitor.viewVariableAssignment != null) {
						assignConvertViewToView= ast
								.toStatement(ast.assign(ast.createCopyTarget(visitor.viewVariableName), Assignment.Operator.ASSIGN, convertViewVar.varName()));
					}
					if (assignConvertViewToView != null) {
						rewrite.insertBefore(assignConvertViewToView, visitor.viewAssignmentStatement, null);
					}
				}

				// Make sure method returns the view to be reused
				if (visitor.returnStatement != null) {
					rewrite.insertAfter(ast.return0(ast.createCopyTarget(visitor.viewVariableName)), visitor.returnStatement, null);
					rewrite.remove(visitor.returnStatement, null);
				}

				// Optimize findViewById calls
				FindViewByIdVisitor findViewByIdVisitor= new FindViewByIdVisitor(visitor.viewVariableName);
				body.accept(findViewByIdVisitor);
				if (!findViewByIdVisitor.items.isEmpty()) {
					// TODO JNR name conflict? Use VariableNameDecider
					Variable viewHolderItemVar= new Variable("ViewHolderItem", "viewHolderItem", ast); //$NON-NLS-1$ //$NON-NLS-2$

					// Create ViewHolderItem class
					rewrite.insertBefore(createViewHolderItemClass(findViewByIdVisitor, viewHolderItemVar.typeName(),
							typeNameDecider), node, null);

					// Declare viewhHolderItem object
					rewrite.insertFirst(body, Block.STATEMENTS_PROPERTY, viewHolderItemVar.declareStatement(), null);
					// Initialize viewHolderItem
					thenStatements.add(
							ast.toStatement(ast.assign(viewHolderItemVar.varName(), Assignment.Operator.ASSIGN, ast.new0(viewHolderItemVar.type()))));
					// Assign findViewById to ViewHolderItem
					for (FindViewByIdVisitor.FindViewByIdItem item : findViewByIdVisitor.items) {
						// Ensure we are accessing convertView object
						FieldAccess fieldAccess= ast.fieldAccess(viewHolderItemVar.varName(),
								ast.simpleName(item.variable.getIdentifier()));
						// FIXME This does not work: not sure why??
						// rewrite.set(item.findViewByIdInvocation,
						// MethodInvocation.EXPRESSION_PROPERTY, convertViewVar.varName());
						item.findViewByIdInvocation.setExpression(convertViewVar.varName());
						// FIXME For some reason ast.copy() does not do what we would like
						thenStatements.add(ast.toStatement(ast.assign(fieldAccess, Assignment.Operator.ASSIGN, ast.copySubtree(item.findViewByIdExpression))));

						// Replace previous findViewById with accesses to viewHolderItem
						rewrite.replace(item.findViewByIdExpression, ast.createCopyTarget(fieldAccess), null);
					}
					// Store viewHolderItem in convertView
					thenStatements.add(ast.toStatement(ast.newMethodInvocation("convertView", "setTag", viewHolderItemVar.varName()))); //$NON-NLS-1$ //$NON-NLS-2$

					// Retrieve viewHolderItem from convertView
					ifStatement.setElseStatement(ast.block(ast.toStatement(ast.assign(viewHolderItemVar.varName(), Assignment.Operator.ASSIGN,
							ast.cast(viewHolderItemVar.type(), ast.newMethodInvocation("convertView", "getTag")))))); //$NON-NLS-1$ //$NON-NLS-2$
				}
				rewrite.remove(visitor.viewAssignmentStatement, null);
				return false;
			}
		}

		return true;
	}

	@SuppressWarnings("unchecked")
	private TypeDeclaration createViewHolderItemClass(final FindViewByIdVisitor findViewByIdVisitor, final SimpleName typeName,
			final TypeNameDecider typeNameDecider) {
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		TypeDeclaration result= ast.getAST().newTypeDeclaration();
		result.modifiers().addAll(Arrays.asList(ast.private0(), ast.static0()));
		result.setName(typeName);
		List<BodyDeclaration> viewItemsFieldDecls= result.bodyDeclarations();

		for (FindViewByIdVisitor.FindViewByIdItem item : findViewByIdVisitor.items) {
			viewItemsFieldDecls.add(item.toFieldDecl(ast, typeNameDecider));
		}

		return result;
	}

	/** Visitor that returns all the interesting accesses to the view variable. */
	private static final class GetViewVariableVisitor extends ASTVisitor {
		private SimpleName viewVariableName;
		/** {@code null} when {@link #viewVariableAssignment} is not null. */
		private VariableDeclarationFragment viewVariableDeclFragment;
		/** {@code null} when {@link #viewVariableDeclFragment} is not null. */
		private Assignment viewVariableAssignment;
		/** Statement which is the ancestor node of the assignment node. */
		private Statement viewAssignmentStatement;
		private ReturnStatement returnStatement;

		private void resetData() {
			viewVariableName= null;
			viewVariableDeclFragment= null;
			viewVariableAssignment= null;
			viewAssignmentStatement= null;
			returnStatement= null;
		}

		@Override
		public boolean visit(final MethodInvocation node) {
			if (isInflateMethod(node)) {
				ASTNode ancestor= ASTNodes.getFirstAncestorOrNull(node, VariableDeclarationFragment.class, Assignment.class);
				if (ancestor instanceof VariableDeclarationFragment) {
					viewVariableDeclFragment= (VariableDeclarationFragment) ancestor;
					viewVariableName= viewVariableDeclFragment.getName();
					viewAssignmentStatement= ASTNodes.getAncestorOrNull(viewVariableDeclFragment, VariableDeclarationStatement.class);
					if (viewAssignmentStatement == null) {
						resetData();
						return true;
					}
				} else if (ancestor instanceof Assignment) {
					viewVariableAssignment= (Assignment) ancestor;
					Expression lhs= viewVariableAssignment.getLeftHandSide();
					if (lhs.getNodeType() != ASTNode.SIMPLE_NAME) {
						resetData();
						return true;
					}
					viewVariableName= (SimpleName) lhs;
					viewAssignmentStatement= ASTNodes.getAncestorOrNull(viewVariableAssignment, ExpressionStatement.class);
				}

				return false;
			}

			return true;
		}

		@Override
		public boolean visit(final ReturnStatement node) {
			this.returnStatement= node;
			return true;
		}

		private boolean canApplyRefactoring() {
			// We found a suitable variable to replace
			return viewVariableName != null && !isInflateInsideIf();
		}

		private boolean isInflateInsideIf() {
			if (this.viewAssignmentStatement != null) {
				Expression inflateExpression= getInflateExpression();
				return ASTNodes.getFirstAncestorOrNull(this.viewAssignmentStatement, IfStatement.class, SwitchStatement.class) != null
						// Check whether inflate is inside a conditional assignment
						|| inflateExpression != null && inflateExpression.getNodeType() == ASTNode.CONDITIONAL_EXPRESSION;
			}

			return false;
		}

		private Expression getInflateExpression() {
			if (this.viewVariableDeclFragment != null) {
				return this.viewVariableDeclFragment.getInitializer();
			}
			if (this.viewVariableAssignment != null) {
				return this.viewVariableAssignment.getRightHandSide();
			}

			return null;
		}

		private boolean isInflateMethod(final MethodInvocation node) {
			String inflaterType= "android.view.LayoutInflater"; //$NON-NLS-1$
			String viewGroupType= "android.view.ViewGroup"; //$NON-NLS-1$
			return ASTNodes.usesGivenSignature(node, inflaterType, "inflate", int.class.getSimpleName(), viewGroupType) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(node, inflaterType, "inflate", int.class.getSimpleName(), viewGroupType, boolean.class.getSimpleName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(node, inflaterType, "inflate", "org.xmlpull.v1.XmlPullParser", viewGroupType) //$NON-NLS-1$ //$NON-NLS-2$
					|| ASTNodes.usesGivenSignature(node, inflaterType, "inflate", "org.xmlpull.v1.XmlPullParser", viewGroupType, //$NON-NLS-1$ //$NON-NLS-2$
							boolean.class.getSimpleName());
		}
	}

	/** Finds calls to {@code android.view.View#findViewById(int)}. */
	private static final class FindViewByIdVisitor extends ASTVisitor {
		static class FindViewByIdItem {
			private SimpleName variable;
			private Expression findViewByIdExpression;
			private MethodInvocation findViewByIdInvocation;

			private boolean setAssignment(final MethodInvocation node) {
				this.findViewByIdInvocation= node;
				ASTNode ancestor= ASTNodes.getFirstAncestorOrNull(node, VariableDeclarationFragment.class, Assignment.class);
				if (ancestor instanceof VariableDeclarationFragment) {
					VariableDeclarationFragment fragment= (VariableDeclarationFragment) ancestor;
					variable= fragment.getName();
					findViewByIdExpression= fragment.getInitializer();
				} else if (ancestor instanceof Assignment) {
					Assignment as= (Assignment) ancestor;
					Expression lhs= as.getLeftHandSide();
					if (lhs.getNodeType() != ASTNode.SIMPLE_NAME) {
						// Only simple names are handled.
						// Using anything else than simple name is unexpected,
						// and even so, it is unexpected that simple names
						// would not mixed with qualified names, etc.
						return false;
					}
					variable= (SimpleName) lhs;
					findViewByIdExpression= as.getRightHandSide();
				} else {
					return false;
				}

				return true;
			}

			@SuppressWarnings("unchecked")
			private FieldDeclaration toFieldDecl(final ASTNodeFactory ast, final TypeNameDecider typeNameDecider) {
				FieldDeclaration field= ast.declareField(ast.copyType(variable, typeNameDecider),
						ast.declareFragment(ast.createCopyTarget(variable)));
				field.modifiers().add(ast.private0());
				return field;
			}
		}

		private List<FindViewByIdItem> items= new ArrayList<>();
		private SimpleName viewVariableName;

		private FindViewByIdVisitor(final SimpleName viewVariableName) {
			this.viewVariableName= viewVariableName;
		}

		@Override
		public boolean visit(final MethodInvocation node) {
			if (ASTNodes.usesGivenSignature(node, "android.view.View", "findViewById", int.class.getSimpleName()) //$NON-NLS-1$ //$NON-NLS-2$
					&& ASTNodes.isSameVariable(viewVariableName, node.getExpression())) {
				FindViewByIdItem item= new FindViewByIdItem();
				if (item.setAssignment(node)) {
					items.add(item);
				}
			}

			return true;
		}
	}
}

/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Separate the code.
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
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.ForLoops;
import org.autorefactor.jdt.internal.corext.dom.ForLoops.ForLoopContent;
import org.autorefactor.jdt.internal.corext.dom.VarDefinitionsUsesVisitor;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ArrayAccess;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SuperFieldAccess;
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class AddAllRatherThanLoopCleanUp extends NewClassImportCleanUp {
	private final class RefactoringWithObjectsClass extends CleanUpWithNewClassImport {
		@Override
		public boolean visit(final EnhancedForStatement node) {
			return maybeRefactorEnhancedForStatement(node, getClassesToUseWithImport(), getImportsToAdd());
		}

		@Override
		public boolean visit(final ForStatement node) {
			return maybeRefactorForStatement(node,
					getClassesToUseWithImport(), getImportsToAdd());
		}
	}

	@Override
	public String getName() {
		return MultiFixMessages.AddAllRatherThanLoopCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.AddAllRatherThanLoopCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.AddAllRatherThanLoopCleanUp_reason;
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
	public boolean visit(final EnhancedForStatement node) {
		return maybeRefactorEnhancedForStatement(node, getAlreadyImportedClasses(node), new HashSet<String>());
	}

	private boolean maybeRefactorEnhancedForStatement(final EnhancedForStatement node,
			final Set<String> classesToUseWithImport, final Set<String> importsToAdd) {
		Expression iterable= node.getExpression();
		MethodInvocation methodInvocation= ASTNodes.asExpression(node.getBody(), MethodInvocation.class);

		IVariableBinding foreachVariable= node.getParameter().resolveBinding();
		// We should remove all the loop variable occurrences
		// As we replace only one, there should be no more than one occurrence
		if (methodInvocation != null
				&& getVariableUseCount(foreachVariable, node.getBody()) == 1
				&& methodInvocation != null && methodInvocation.arguments().size() == 1) {
			if (ASTNodes.instanceOf(iterable, Collection.class.getCanonicalName())) {
				if (ASTNodes.isSameLocalVariable(node.getParameter(), (Expression) methodInvocation.arguments().get(0))) {
					return maybeReplaceForCollection(node, methodInvocation, iterable);
				}
			} else if (ASTNodes.isArray(iterable) && ASTNodes.isSameLocalVariable(foreachVariable, (Expression) methodInvocation.arguments().get(0))) {
				return maybeReplaceForArray(node, classesToUseWithImport, importsToAdd, iterable, methodInvocation);
			}
		}

		return true;
	}

	@Override
	public boolean visit(final ForStatement node) {
		return maybeRefactorForStatement(node, getAlreadyImportedClasses(node), new HashSet<String>());
	}

	private boolean maybeRefactorForStatement(final ForStatement node, final Set<String> classesToUseWithImport,
			final Set<String> importsToAdd) {
		ForLoopContent loopContent= ForLoops.iterateOverContainer(node);
		MethodInvocation methodInvocation= ASTNodes.asExpression(node.getBody(), MethodInvocation.class);

		if (loopContent != null
				&& loopContent.getLoopVariable() != null
				&& methodInvocation != null) {
			Name loopVariable= loopContent.getLoopVariable();
			IVariableBinding loopVariableName= (IVariableBinding) loopVariable.resolveBinding();

			// We should remove all the loop variable occurrences
			// As we replace only one, there should be no more than one occurrence
			if (methodInvocation != null && methodInvocation.arguments().size() == 1 && getVariableUseCount(loopVariableName, node.getBody()) == 1
					&& (loopContent.isLoopingForward() || methodInvocation.resolveMethodBinding() != null && ASTNodes.hasType(methodInvocation.resolveMethodBinding().getDeclaringClass(), Set.class.getCanonicalName()))) {
				Expression addArg0= (Expression) methodInvocation.arguments().get(0);

				switch (loopContent.getContainerType()) {
				case COLLECTION:
					MethodInvocation getMI= ASTNodes.as(addArg0, MethodInvocation.class);

					if (getMI != null && getMI.arguments().size() == 1 && isSameVariable(loopContent, getMI)) {
						return maybeReplaceForCollection(node, methodInvocation, getMI.getExpression());
					}
					break;

				case ARRAY:
					ArrayAccess arrayAccess= ASTNodes.as(addArg0, ArrayAccess.class);

					if (isSameVariable(loopContent, arrayAccess)) {
						return maybeReplaceForArray(node, classesToUseWithImport, importsToAdd, loopContent.getContainerVariable(), methodInvocation);
					}
					break;
				}
			}
		}

		return true;
	}

	private boolean maybeReplaceForArray(final Statement node, final Set<String> classesToUseWithImport,
			final Set<String> importsToAdd, final Expression iterable, final MethodInvocation addMethod) {
		if (addMethod.getExpression() != null
				&& !ASTNodes.is(addMethod.getExpression(), ThisExpression.class)
				&& ASTNodes.usesGivenSignature(addMethod, Collection.class.getCanonicalName(), "add", Object.class.getCanonicalName()) //$NON-NLS-1$
				&& areTypeCompatible(ASTNodes.getCalledType(addMethod), iterable.resolveTypeBinding())) {
			replaceWithCollectionsAddAll(node, classesToUseWithImport, importsToAdd, iterable, addMethod);
			return false;
		}

		return true;
	}

	private void replaceWithCollectionsAddAll(final Statement node, final Set<String> classesToUseWithImport,
			final Set<String> importsToAdd, final Expression iterable, final MethodInvocation addMethod) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.AddAllRatherThanLoopCleanUp_description);

		String classname= addImport(Collections.class, classesToUseWithImport, importsToAdd);
		MethodInvocation newMethodInvocation= ast.newMethodInvocation();
		newMethodInvocation.setExpression(ASTNodeFactory.newName(ast, classname));
		newMethodInvocation.setName(ast.newSimpleName("addAll")); //$NON-NLS-1$
		newMethodInvocation.arguments().add(addMethod.getExpression() != null ? ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(addMethod.getExpression())) : ast.newThisExpression());
		newMethodInvocation.arguments().add(ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(iterable)));

		ASTNodes.replaceButKeepComment(rewrite, node,
				ast.newExpressionStatement(newMethodInvocation), group);
	}

	private int getVariableUseCount(final IVariableBinding variableBinding, final Statement toVisit) {
		if (variableBinding != null) {
			VarDefinitionsUsesVisitor variableUseVisitor= new VarDefinitionsUsesVisitor(variableBinding,
			toVisit, true);
			return variableUseVisitor.getReads().size();
		}

		return 0;
	}

	private boolean isSameVariable(final ForLoopContent loopContent, final ArrayAccess arrayAccess) {
		return arrayAccess != null
				&& ASTNodes.isSameVariable(arrayAccess.getArray(), loopContent.getContainerVariable())
				&& ASTNodes.isSameLocalVariable(arrayAccess.getIndex(), loopContent.getLoopVariable());
	}

	private boolean areTypeCompatible(final ITypeBinding colTypeBinding, final ITypeBinding arrayTypeBinding) {
		if (arrayTypeBinding != null && colTypeBinding != null) {
			ITypeBinding jucTypeBinding= ASTNodes.findImplementedType(colTypeBinding, Collection.class.getCanonicalName());

			if (jucTypeBinding.isRawType()) {
				return true;
			}

			ITypeBinding componentType= arrayTypeBinding.getComponentType();
			ITypeBinding colTypeArgument= jucTypeBinding.getTypeArguments()[0];
			return componentType.isSubTypeCompatible(colTypeArgument);
		}

		return false;
	}

	private boolean maybeReplaceForCollection(final ASTNode node, final MethodInvocation addOrRemoveMethod,
			final Expression data) {
		if (addOrRemoveMethod.getExpression() == null
				|| ASTNodes.is(addOrRemoveMethod.getExpression(), ThisExpression.class)) {
			return true;
		}

		if (ASTNodes.usesGivenSignature(addOrRemoveMethod, Collection.class.getCanonicalName(), "add", Object.class.getCanonicalName())) { //$NON-NLS-1$
			replaceWithCollectionMethod(node, "addAll", addOrRemoveMethod.getExpression(), data); //$NON-NLS-1$
			return false;
		}
		if (ASTNodes.usesGivenSignature(addOrRemoveMethod, Set.class.getCanonicalName(), "remove", Object.class.getCanonicalName())) { //$NON-NLS-1$
			replaceWithCollectionMethod(node, "removeAll", addOrRemoveMethod.getExpression(), data); //$NON-NLS-1$
			return false;
		}

		return true;
	}

	private boolean isSameVariable(final ForLoopContent loopContent, final MethodInvocation getMI) {
		return (getMI.getExpression() instanceof Name || getMI.getExpression() instanceof FieldAccess || getMI.getExpression() instanceof SuperFieldAccess)
				&& ASTNodes.usesGivenSignature(getMI, List.class.getCanonicalName(), "get", int.class.getSimpleName()) //$NON-NLS-1$
				&& ASTNodes.isSameLocalVariable((Expression) getMI.arguments().get(0), loopContent.getLoopVariable())
				&& ASTNodes.isSameVariable(loopContent.getContainerVariable(), getMI.getExpression());
	}

	private void replaceWithCollectionMethod(final ASTNode toReplace, final String methodName,
			final Expression affectedCollection,
			final Expression data) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.AddAllRatherThanLoopCleanUp_description);

		MethodInvocation newMethod= ast.newMethodInvocation();
		newMethod.setName(ast.newSimpleName(methodName));
		newMethod.arguments().add(ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(data)));

		if (affectedCollection != null) {
			newMethod.setExpression(ASTNodes.createMoveTarget(rewrite, affectedCollection));
		}

		ASTNodes.replaceButKeepComment(rewrite, toReplace, ast.newExpressionStatement(newMethod), group);
	}
}

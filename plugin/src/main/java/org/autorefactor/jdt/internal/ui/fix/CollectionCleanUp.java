/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Annoying remaining loop variable occurrence
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

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.PriorityQueue;
import java.util.TreeSet;
import java.util.Vector;
import java.util.concurrent.ConcurrentLinkedDeque;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.concurrent.DelayQueue;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.LinkedTransferQueue;
import java.util.concurrent.PriorityBlockingQueue;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class CollectionCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_CollectionCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_CollectionCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_CollectionCleanUp_reason;
	}

	@Override
	public boolean visit(final Block node) {
		NewAndAddAllMethodVisitor newAndAddAllMethodVisitor= new NewAndAddAllMethodVisitor();
		newAndAddAllMethodVisitor.visitNode(node);
		return newAndAddAllMethodVisitor.result;
	}

	private final class NewAndAddAllMethodVisitor extends BlockSubVisitor {
		@Override
		public boolean visit(final ExpressionStatement node) {
			MethodInvocation methodInvocation= ASTNodes.asExpression(node, MethodInvocation.class);

			if (result && ASTNodes.usesGivenSignature(methodInvocation, Collection.class.getCanonicalName(), "addAll", Collection.class.getCanonicalName())) { //$NON-NLS-1$
				Expression arg0= (Expression) methodInvocation.arguments().get(0);
				Statement previousStatement= ASTNodes.getPreviousSibling(node);

				Assignment as= ASTNodes.asExpression(previousStatement, Assignment.class);
				if (ASTNodes.hasOperator(as, Assignment.Operator.ASSIGN)) {
					SimpleName lhs= ASTNodes.as(as.getLeftHandSide(), SimpleName.class);

					if (lhs != null && ASTNodes.isSameLocalVariable(lhs, methodInvocation.getExpression())) {
						return maybeReplaceInitializer(as.getRightHandSide(), arg0, node);
					}
				} else if (previousStatement instanceof VariableDeclarationStatement) {
					VariableDeclarationFragment fragment= ASTNodes.getUniqueFragment(
							previousStatement);
					if (fragment != null && ASTNodes.isSameLocalVariable(fragment, methodInvocation.getExpression())) {
						return maybeReplaceInitializer(fragment.getInitializer(), arg0, node);
					}
				}
			}

			return true;
		}

		private boolean maybeReplaceInitializer(final Expression nodeToReplace, final Expression arg0,
				final ExpressionStatement nodeToRemove) {
			ClassInstanceCreation cic= ASTNodes.as(nodeToReplace, ClassInstanceCreation.class);

			if (canReplaceInitializer(cic, arg0) && ASTNodes.isCastCompatible(nodeToReplace, arg0)) {
				ASTRewrite rewrite= cuRewrite.getASTRewrite();
				ASTNodeFactory ast= cuRewrite.getASTBuilder();
				TextEditGroup group= new TextEditGroup(MultiFixMessages.CleanUpRefactoringWizard_CollectionCleanUp_name);

				rewrite.replace(nodeToReplace, ast.new0(ASTNodes.createMoveTarget(rewrite, cic.getType()), ASTNodes.createMoveTarget(rewrite, arg0)), group);
				rewrite.remove(nodeToRemove, group);
				result= false;
				return false;
			}

			return true;
		}

		private boolean canReplaceInitializer(final ClassInstanceCreation cic, final Expression sourceCollection) {
			if (cic == null || cic.getAnonymousClassDeclaration() != null) {
				return false;
			}

			@SuppressWarnings("unchecked")
			List<Expression> args= cic.arguments();
			boolean noArgsCtor= args.isEmpty();

			if (noArgsCtor && ASTNodes.hasType(cic, ConcurrentLinkedDeque.class.getCanonicalName(),
					ConcurrentLinkedQueue.class.getCanonicalName(), ConcurrentSkipListSet.class.getCanonicalName(),
					CopyOnWriteArrayList.class.getCanonicalName(), CopyOnWriteArraySet.class.getCanonicalName(),
					DelayQueue.class.getCanonicalName(), LinkedBlockingDeque.class.getCanonicalName(),
					LinkedBlockingQueue.class.getCanonicalName(), LinkedTransferQueue.class.getCanonicalName(),
					PriorityBlockingQueue.class.getCanonicalName(), ArrayDeque.class.getCanonicalName(), ArrayList.class.getCanonicalName(),
					HashSet.class.getCanonicalName(), LinkedHashSet.class.getCanonicalName(), LinkedList.class.getCanonicalName(), PriorityQueue.class.getCanonicalName(),
					TreeSet.class.getCanonicalName(), Vector.class.getCanonicalName())) {
				return true;
			}

			boolean colCapacityCtor= isValidCapacityParameter(sourceCollection, args);

			return colCapacityCtor && ASTNodes.hasType(cic, LinkedBlockingDeque.class.getCanonicalName(),
					LinkedBlockingQueue.class.getCanonicalName(), PriorityBlockingQueue.class.getCanonicalName(),
					ArrayDeque.class.getCanonicalName(), ArrayList.class.getCanonicalName(), HashSet.class.getCanonicalName(),
					LinkedHashSet.class.getCanonicalName(), PriorityQueue.class.getCanonicalName(), Vector.class.getCanonicalName());
		}

		private boolean isValidCapacityParameter(final Expression sourceCollection, final List<Expression> args) {
			if (args.size() == 1 && ASTNodes.isPrimitive(args.get(0), int.class.getSimpleName())) {
				Long constant= ASTNodes.getIntegerLiteral(args.get(0));

				if (constant != null) {
					return Long.valueOf(0).equals(constant);
				}

				MethodInvocation methodInvocation= ASTNodes.as(args.get(0), MethodInvocation.class);

				return ASTNodes.usesGivenSignature(methodInvocation, Collection.class.getCanonicalName(), "size") && ASTNodes.match(methodInvocation.getExpression(), sourceCollection); //$NON-NLS-1$
			}

			return false;
		}
	}
}

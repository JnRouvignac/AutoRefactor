/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015-2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Make sure we do not visit again modified nodes
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

import java.util.HashMap;
import java.util.Hashtable;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.WeakHashMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentSkipListMap;

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
public class ObsoleteMapCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.ObsoleteMapCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ObsoleteMapCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ObsoleteMapCleanUp_reason;
	}

	@Override
	public boolean visit(final Block node) {
		NewAndPutAllMethodVisitor newAndPutAllMethodVisitor= new NewAndPutAllMethodVisitor();
		newAndPutAllMethodVisitor.visitNode(node);
		return newAndPutAllMethodVisitor.result;
	}

	private final class NewAndPutAllMethodVisitor extends BlockSubVisitor {
		@Override
		public boolean visit(final ExpressionStatement node) {
			MethodInvocation methodInvocation= ASTNodes.asExpression(node, MethodInvocation.class);

			if (result && ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "putAll", Map.class.getCanonicalName())) { //$NON-NLS-1$
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
			ClassInstanceCreation classInstanceCreation= ASTNodes.as(nodeToReplace, ClassInstanceCreation.class);
			if (canReplaceInitializer(classInstanceCreation, arg0) && ASTNodes.isCastCompatible(nodeToReplace, arg0)) {
				ASTRewrite rewrite= cuRewrite.getASTRewrite();
				ASTNodeFactory ast= cuRewrite.getASTBuilder();
				TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteMapCleanUp_description);

				ASTNodes.replaceButKeepComment(rewrite, nodeToReplace, ast.newClassInstanceCreation(ASTNodes.createMoveTarget(rewrite, classInstanceCreation.getType()), ASTNodes.createMoveTarget(rewrite, arg0)), group);
				rewrite.remove(nodeToRemove, group);
				result= false;
				return false;
			}

			return true;
		}

		private boolean canReplaceInitializer(final ClassInstanceCreation classInstanceCreation, final Expression sourceMap) {
			if (classInstanceCreation == null || classInstanceCreation.getAnonymousClassDeclaration() != null) {
				return false;
			}
			List<Expression> args= classInstanceCreation.arguments();
			boolean noArgsCtor= args.isEmpty();
			boolean mapCapacityCtor= isValidCapacityParameter(sourceMap, args);
			return noArgsCtor && ASTNodes.hasType(classInstanceCreation, ConcurrentHashMap.class.getCanonicalName(),
					ConcurrentSkipListMap.class.getCanonicalName(), Hashtable.class.getCanonicalName(), HashMap.class.getCanonicalName(),
					IdentityHashMap.class.getCanonicalName(), LinkedHashMap.class.getCanonicalName(), TreeMap.class.getCanonicalName(),
					WeakHashMap.class.getCanonicalName())
					|| mapCapacityCtor && ASTNodes.hasType(classInstanceCreation, ConcurrentHashMap.class.getCanonicalName(), Hashtable.class.getCanonicalName(),
							HashMap.class.getCanonicalName(), IdentityHashMap.class.getCanonicalName(), LinkedHashMap.class.getCanonicalName(),
							WeakHashMap.class.getCanonicalName());
		}

		private boolean isValidCapacityParameter(final Expression sourceMap, final List<Expression> args) {
			if (args.size() == 1 && ASTNodes.isPrimitive(args.get(0), int.class.getSimpleName())) {
				Long constant= ASTNodes.getIntegerLiteral(args.get(0));

				if (constant != null) {
					return Long.valueOf(0L).equals(constant);
				}

				MethodInvocation methodInvocation= ASTNodes.as(args.get(0), MethodInvocation.class);

				return ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "size") && ASTNodes.match(methodInvocation.getExpression(), sourceMap); //$NON-NLS-1$
			}

			return false;
		}
	}
}

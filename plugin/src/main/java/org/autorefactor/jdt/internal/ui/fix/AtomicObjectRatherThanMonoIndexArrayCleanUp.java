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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.   See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program under LICENSE-GNUGPL.   If not, see
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
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.autorefactor.jdt.internal.corext.dom.VarDefinitionsUsesVisitor;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.ArrayAccess;
import org.eclipse.jdt.core.dom.ArrayCreation;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.LambdaExpression;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/** See {@link #getDescription()} method. */
public class AtomicObjectRatherThanMonoIndexArrayCleanUp extends NewClassImportCleanUp {
	private final class RefactoringWithObjectsClass extends CleanUpWithNewClassImport {
		@Override
		public boolean visit(final Block node) {
			return maybeRefactorBlock(node, getClassesToUseWithImport(), getImportsToAdd());
		}
	}

	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_AtomicObjectRatherThanMonoIndexArrayCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_AtomicObjectRatherThanMonoIndexArrayCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_AtomicObjectRatherThanMonoIndexArrayCleanUp_reason;
	}

	@Override
	public boolean isJavaVersionSupported(final Release javaSeRelease) {
		return javaSeRelease.getMinorVersion() >= 5;
	}

	@Override
	public RefactoringWithObjectsClass getRefactoringClassInstance() {
		return new RefactoringWithObjectsClass();
	}

	@Override
	public Set<String> getClassesToImport() {
		return new HashSet<>(Arrays.asList(AtomicReference.class.getCanonicalName(), AtomicBoolean.class.getCanonicalName(), AtomicInteger.class.getCanonicalName(), AtomicLong.class.getCanonicalName()));
	}

	@Override
	public boolean visit(final Block node) {
		return maybeRefactorBlock(node, getAlreadyImportedClasses(node), new HashSet<String>());
	}

	private boolean maybeRefactorBlock(final Block node, final Set<String> classesToUseWithImport, final Set<String> importsToAdd) {
		ArrayOccurrencesVisitor stringOccurrencesVisitor= new ArrayOccurrencesVisitor(classesToUseWithImport, importsToAdd);
		stringOccurrencesVisitor.visitNode(node);
		return stringOccurrencesVisitor.result;
	}

	private final class ArrayOccurrencesVisitor extends BlockSubVisitor {
		private final Set<String> classesToUseWithImport;
		private final Set<String> importsToAdd;

		/**
		 * @param classesToUseWithImport
		 * @param importsToAdd
		 */
		private ArrayOccurrencesVisitor(Set<String> classesToUseWithImport, Set<String> importsToAdd) {
			this.classesToUseWithImport= classesToUseWithImport;
			this.importsToAdd= importsToAdd;
		}

		@Override
		public boolean visit(final VariableDeclarationStatement node) {
			if (node.fragments().size() != 1) {
				return true;
			}

			VariableDeclarationFragment fragment= (VariableDeclarationFragment) node.fragments().get(0);
			return visitVariable(node.getType(), fragment.resolveBinding(), fragment.extraDimensions(), fragment.getName(), fragment.getInitializer());
		}

		@Override
		public boolean visit(final VariableDeclarationExpression node) {
			if (node.fragments().size() != 1) {
				return true;
			}

			VariableDeclarationFragment fragment= (VariableDeclarationFragment) node.fragments().get(0);
			return visitVariable(node.getType(), fragment.resolveBinding(), fragment.extraDimensions(), fragment.getName(), fragment.getInitializer());
		}

		@Override
		public boolean visit(final SingleVariableDeclaration node) {
			return visitVariable(node.getType(), node.resolveBinding(), node.extraDimensions(), node.getName(), node.getInitializer());
		}

		private boolean visitVariable(final Type type, final IVariableBinding variableBinding, final List<?> variableDimensions, final SimpleName declaration, final Expression initializer) {
			ArrayCreation arrayCreation= ASTNodes.as(initializer, ArrayCreation.class);

			if (result
					&& arrayCreation != null
					&& (arrayCreation.getInitializer() != null
							? arrayCreation.getInitializer().expressions().size() == 1
							: arrayCreation.dimensions().size() == 1 && Long.valueOf(1).equals(ASTNodes.getIntegerLiteral((Expression) arrayCreation.dimensions().get(0))))
					&& (type.resolveBinding().isArray()
							? variableDimensions.isEmpty() && type.resolveBinding().getDimensions() == 1 && Utils.equalNotNull(type.resolveBinding().getElementType(), arrayCreation.getType().getElementType().resolveBinding())
									: variableDimensions.size() == 1 && Utils.equalNotNull(type.resolveBinding(), arrayCreation.getType().getElementType().resolveBinding()))
					&& !ASTNodes.hasType(arrayCreation.getType().getElementType().resolveBinding(),
							double.class.getCanonicalName(),
							float.class.getCanonicalName(),
							short.class.getCanonicalName(),
							char.class.getCanonicalName(),
							byte.class.getCanonicalName())) {
				VarDefinitionsUsesVisitor varOccurrencesVisitor= new VarDefinitionsUsesVisitor(variableBinding,
						startNode, true).find();

				List<SimpleName> reads= varOccurrencesVisitor.getReads();
				List<SimpleName> writes= varOccurrencesVisitor.getWrites();
				writes.remove(declaration);

				if (writes.isEmpty()) {
					Set<Assignment> assignmentReads= new HashSet<>();
					Set<ArrayAccess> accessReads= new HashSet<>();

					for (SimpleName simpleName : reads) {
						if (!isReadValid(simpleName, assignmentReads, accessReads)) {
							return true;
						}
					}

					boolean hasOneWriteInDynamicCode= false;

					for (Assignment assignmentRead : assignmentReads) {
						ASTNode dynamicCode= ASTNodes.getASTNodeAncestor(assignmentRead, LambdaExpression.class, AnonymousClassDeclaration.class);

						if (ASTNodes.isParent(dynamicCode, startNode)) {
							hasOneWriteInDynamicCode= true;
							break;
						}
					}

					if (hasOneWriteInDynamicCode) {
						replaceArray(type, variableDimensions, arrayCreation, assignmentReads, accessReads);

						result= false;
						return false;
					}
				}
			}

			return true;
		}

		private boolean isReadValid(final SimpleName simpleName, final Set<Assignment> assignmentReads, final Set<ArrayAccess> accessReads) {
			if (simpleName.getParent() instanceof ArrayAccess
					&& simpleName.getLocationInParent() == ArrayAccess.ARRAY_PROPERTY) {
				ArrayAccess arrayAccess= (ArrayAccess) simpleName.getParent();

				if (Long.valueOf(0).equals(ASTNodes.getIntegerLiteral(arrayAccess.getIndex()))) {
					if (arrayAccess.getParent() instanceof Assignment
							&& arrayAccess.getLocationInParent() == Assignment.LEFT_HAND_SIDE_PROPERTY) {
						Assignment assignment= (Assignment) arrayAccess.getParent();

						if (ASTNodes.hasOperator(assignment, Assignment.Operator.ASSIGN)
								&& (assignment.getParent() instanceof ExpressionStatement
								|| assignment.getParent() instanceof LambdaExpression && assignment.getLocationInParent() == LambdaExpression.BODY_PROPERTY)) {
							assignmentReads.add(assignment);
							return true;
						}
					} else if ((!(arrayAccess.getParent() instanceof PrefixExpression)
							|| !ASTNodes.hasOperator((PrefixExpression) arrayAccess.getParent(), PrefixExpression.Operator.INCREMENT, PrefixExpression.Operator.DECREMENT))
							&& (!(arrayAccess.getParent() instanceof PostfixExpression)
									|| !ASTNodes.hasOperator((PostfixExpression) arrayAccess.getParent(), PostfixExpression.Operator.INCREMENT, PostfixExpression.Operator.DECREMENT))) {
						accessReads.add(arrayAccess);
						return true;
					}
				}
			}

			return false;
		}

		private void replaceArray(final Type type, final List<?> variableDimensions, final ArrayCreation arrayCreation, final Set<Assignment> assignmentReads, final Set<ArrayAccess> accessReads) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			ASTNodeFactory ast= cuRewrite.getASTBuilder();

			Class<?> atomicClass;
			Type objectClass= null;
			if (ASTNodes.hasType(arrayCreation.getType().getElementType().resolveBinding(), boolean.class.getCanonicalName())) {
				atomicClass= AtomicBoolean.class;
			} else if (ASTNodes.hasType(arrayCreation.getType().getElementType().resolveBinding(), int.class.getCanonicalName())) {
				atomicClass= AtomicInteger.class;
			} else if (ASTNodes.hasType(arrayCreation.getType().getElementType().resolveBinding(), long.class.getCanonicalName())) {
				atomicClass= AtomicLong.class;
			} else {
				atomicClass= AtomicReference.class;
				objectClass= arrayCreation.getType().getElementType();
			}

			String atomicClassName= addImport(atomicClass, classesToUseWithImport, importsToAdd);

			Type atomicType;
			if (objectClass == null) {
				atomicType= ast.type(atomicClassName);
			} else if (getJavaMinorVersion() >= 7) {
				atomicType= ast.genericType(atomicClassName);
			} else {
				atomicType= ast.genericType(atomicClassName, rewrite.createCopyTarget(objectClass));
			}

			ClassInstanceCreation newAtomicObject= ast.new0(atomicType);

			if (arrayCreation.getInitializer() != null) {
				@SuppressWarnings("unchecked")
				List<Expression> arguments= newAtomicObject.arguments();
				arguments.add(ASTNodes.createMoveTarget(rewrite, (Expression) arrayCreation.getInitializer().expressions().get(0)));
			}

			rewrite.replace(arrayCreation, newAtomicObject, null);

			for (Object variableDimension : variableDimensions) {
				rewrite.remove((ASTNode) variableDimension, null);
			}

			if (objectClass == null) {
				atomicType= ast.type(atomicClassName);
			} else {
				atomicType= ast.genericType(atomicClassName, ASTNodes.createMoveTarget(rewrite, objectClass));
			}

			rewrite.replace(type, atomicType, null);

			for (ArrayAccess accessRead : accessReads) {
				rewrite.replace(accessRead, ast.newMethodInvocation(ASTNodes.createMoveTarget(rewrite, accessRead.getArray()), "get"), null); //$NON-NLS-1$
			}

			for (Assignment assignmentRead : assignmentReads) {
				rewrite.replace(assignmentRead, ast.newMethodInvocation(ASTNodes.createMoveTarget(rewrite, ((ArrayAccess) assignmentRead.getLeftHandSide()).getArray()),
						"set", ASTNodes.createMoveTarget(rewrite, assignmentRead.getRightHandSide())), null); //$NON-NLS-1$
			}
		}
	}
}

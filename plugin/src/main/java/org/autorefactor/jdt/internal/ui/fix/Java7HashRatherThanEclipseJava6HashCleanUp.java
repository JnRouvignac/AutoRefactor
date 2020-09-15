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
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.OrderedInfixExpression;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.NullLiteral;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SuperFieldAccess;
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class Java7HashRatherThanEclipseJava6HashCleanUp extends NewClassImportCleanUp {
	private static final String HASH_CODE_METHOD= "hashCode"; //$NON-NLS-1$

	private static final class CollectedData {
		private List<Expression> fields= new ArrayList<>();
		private SimpleName primeId;
		private SimpleName resultId;
		private Iterator<Statement> stmtIterator;
		private SimpleName tempVar;
		private boolean tempValueUsed= true;
		private boolean hasReturnStatement;

		/**
		 * Get the current value.
		 *
		 * @return the current value
		 */
		public SimpleName getTempVar() {
			return tempVar;
		}

		/**
		 * Set to the given value.
		 *
		 * @param tempVar the new value
		 */
		public void setTempVar(final SimpleName tempVar) {
			this.tempVar= tempVar;
		}

		/**
		 * @return the primeId
		 */
		public SimpleName getPrimeId() {
			return primeId;
		}

		/**
		 * @param primeId the primeId to set
		 */
		public void setPrimeId(final SimpleName primeId) {
			this.primeId= primeId;
		}

		/**
		 * @return the resultId
		 */
		public SimpleName getResultId() {
			return resultId;
		}

		/**
		 * @param resultId the resultId to set
		 */
		public void setResultId(final SimpleName resultId) {
			this.resultId= resultId;
		}

		/**
		 * @return the stmtIterator
		 */
		public Iterator<Statement> getStmtIterator() {
			return stmtIterator;
		}

		/**
		 * @param stmtIterator the stmtIterator to set
		 */
		public void setStmtIterator(final Iterator<Statement> stmtIterator) {
			this.stmtIterator= stmtIterator;
		}

		/**
		 * @return the hasReturnStatement
		 */
		public boolean isHasReturnStatement() {
			return hasReturnStatement;
		}

		/**
		 * @param hasReturnStatement the hasReturnStatement to set
		 */
		public void setHasReturnStatement(final boolean hasReturnStatement) {
			this.hasReturnStatement= hasReturnStatement;
		}

		/**
		 * @return the tempValueUsed
		 */
		public boolean isTempValueUsed() {
			return tempValueUsed;
		}

		/**
		 * @param tempValueUsed the tempValueUsed to set
		 */
		public void setTempValueUsed(final boolean tempValueUsed) {
			this.tempValueUsed= tempValueUsed;
		}

		/**
		 * @return the fields
		 */
		public List<Expression> getFields() {
			return fields;
		}
	}

	private final class RefactoringWithObjectsClass extends CleanUpWithNewClassImport {
		@Override
		public boolean visit(final MethodDeclaration node) {
			return maybeRefactorMethodDeclaration(node, getClassesToUseWithImport(), getImportsToAdd());
		}
	}

	@Override
	public String getName() {
		return MultiFixMessages.Java7HashRatherThanEclipseJava6HashCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.Java7HashRatherThanEclipseJava6HashCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.Java7HashRatherThanEclipseJava6HashCleanUp_reason;
	}

	@Override
	public Set<String> getClassesToImport() {
		return new HashSet<>(Arrays.asList(Objects.class.getCanonicalName()));
	}

	@Override
	public CleanUpWithNewClassImport getRefactoringClassInstance() {
		return new RefactoringWithObjectsClass();
	}

	@Override
	public boolean isJavaVersionSupported(final Release javaSeRelease) {
		return javaSeRelease.getMinorVersion() >= 7;
	}

	@Override
	public boolean visit(final MethodDeclaration node) {
		return maybeRefactorMethodDeclaration(node, getAlreadyImportedClasses(node), new HashSet<String>());
	}

	private boolean maybeRefactorMethodDeclaration(final MethodDeclaration node,
			final Set<String> classesToUseWithImport, final Set<String> importsToAdd) {
		Block body= node.getBody();

		if (ASTNodes.usesGivenSignature(node, Object.class.getCanonicalName(), HASH_CODE_METHOD) && body != null) {
			@SuppressWarnings("unchecked")
			List<Statement> statements= body.statements();

			if (statements.size() > 2) {
				CollectedData data= new CollectedData();
				data.setStmtIterator(statements.iterator());

				data.setPrimeId(isVariableValid(data, 31));
				data.setResultId(isVariableValid(data, 1));

				if (data.getPrimeId() != null && data.getResultId() != null && data.getStmtIterator().hasNext()) {
					while (!data.isHasReturnStatement() && data.getStmtIterator().hasNext()) {
						if (!isStmtValid(data)) {
							return true;
						}
					}

					if (data.isHasReturnStatement() && !data.getStmtIterator().hasNext()) {
						refactorHash(node, classesToUseWithImport, importsToAdd, data);
						return false;
					}
				}
			}
		}

		return true;
	}

	private SimpleName isVariableValid(final CollectedData data, final int initValue) {
		Statement statement= data.getStmtIterator().next();
		VariableDeclarationStatement varDecl= ASTNodes.as(statement, VariableDeclarationStatement.class);

		if (varDecl != null && ASTNodes.hasType(varDecl.getType().resolveBinding(), int.class.getSimpleName()) && varDecl.fragments().size() == 1) {
			VariableDeclarationFragment varFragment= (VariableDeclarationFragment) varDecl.fragments().get(0);

			if (Long.valueOf(initValue).equals(ASTNodes.getIntegerLiteral(varFragment.getInitializer()))) {
				return varFragment.getName();
			}
		}

		return null;
	}

	private boolean isStmtValid(final CollectedData data) {
		Statement statement= data.getStmtIterator().next();
		ExpressionStatement exprStatement= ASTNodes.as(statement, ExpressionStatement.class);
		VariableDeclarationStatement varStatement= ASTNodes.as(statement, VariableDeclarationStatement.class);
		ReturnStatement returnStatement= ASTNodes.as(statement, ReturnStatement.class);

		if (exprStatement != null) {
			return isAssignmentValid(data, exprStatement);
		}
		if (varStatement != null && data.getTempVar() == null) {
			@SuppressWarnings("unchecked")
			List<VariableDeclarationFragment> fragments= varStatement.fragments();

			if (ASTNodes.hasType(varStatement.getType().resolveBinding(), long.class.getSimpleName()) && fragments != null && fragments.size() == 1) {
				VariableDeclarationFragment fragment= fragments.get(0);
				data.setTempVar(fragment.getName());
				Expression initializer= fragment.getInitializer();

				if (fragment.getExtraDimensions() == 0) {
					if (initializer != null) {
						SimpleName fieldToFind= isDoubleToLongBitsMethod(data, initializer);
						data.setTempValueUsed(false);

						if (fieldToFind != null && data.getStmtIterator().hasNext()) {
							boolean assignmentValid= isStmtValid(data);

							if (assignmentValid) {
								data.getFields().add(ASTNodes.getUnparenthesedExpression(fieldToFind));
								return true;
							}
						}
					} else if (data.getStmtIterator().hasNext()) {
						return isStmtValid(data);
					}
				}
			}
		} else if (returnStatement != null) {
			data.setHasReturnStatement(true);
			Expression expression= returnStatement.getExpression();

			return returnStatement != null && (isGivenVariable(expression, data.getResultId()) || isHashValid(data, expression));
		}

		return false;
	}

	private boolean isAssignmentValid(final CollectedData data, final ExpressionStatement statement) {
		Assignment assignment= ASTNodes.as(statement.getExpression(), Assignment.class);

		if (assignment != null && ASTNodes.hasOperator(assignment, Assignment.Operator.ASSIGN)) {
			Expression field= assignment.getLeftHandSide();
			Expression resultComputation= assignment.getRightHandSide();

			if (isGivenVariable(field, data.getResultId())) {
				return isHashValid(data, resultComputation);
			}
			if (data.getTempVar() != null && isGivenVariable(field, data.getTempVar())) {
				SimpleName fieldToFind= isDoubleToLongBitsMethod(data, resultComputation);

				if (fieldToFind != null && data.getStmtIterator().hasNext()) {
					data.setTempValueUsed(false);
					boolean assignmentValid= isStmtValid(data);

					if (assignmentValid) {
						data.getFields().add(ASTNodes.getUnparenthesedExpression(fieldToFind));
						return true;
					}
				}
			}
		}

		return false;
	}

	private SimpleName isDoubleToLongBitsMethod(final CollectedData data, final Expression initializer) {
		SimpleName fieldToFind= null;
		MethodInvocation doubleToLongBits= ASTNodes.as(initializer, MethodInvocation.class);

		if (doubleToLongBits != null && ASTNodes.usesGivenSignature(doubleToLongBits, Double.class.getCanonicalName(), "doubleToLongBits", double.class.getSimpleName())) { //$NON-NLS-1$
			SimpleName fieldName= ASTNodes.as((Expression) doubleToLongBits.arguments().get(0), SimpleName.class);

			if (fieldName != null && !ASTNodes.isSameVariable(fieldName, data.getPrimeId())
					&& !ASTNodes.isSameVariable(fieldName, data.getResultId())) {
				fieldToFind= fieldName;
			}
		}

		return fieldToFind;
	}

	private boolean isHashValid(final CollectedData data, final Expression hashComputation) {
		InfixExpression hashAddition= ASTNodes.as(hashComputation, InfixExpression.class);

		if (hashAddition != null) {
			InfixExpression primeTimesResult= ASTNodes.as(hashAddition.getLeftOperand(), InfixExpression.class);
			Expression newHash= hashAddition.getRightOperand();

			if (!hashAddition.hasExtendedOperands() && ASTNodes.hasOperator(hashAddition, InfixExpression.Operator.PLUS)
					&& primeTimesResult != null && !primeTimesResult.hasExtendedOperands()
					&& ASTNodes.hasOperator(primeTimesResult, InfixExpression.Operator.TIMES)
					&& ((isGivenVariable(primeTimesResult.getLeftOperand(), data.getPrimeId())
							&& isGivenVariable(primeTimesResult.getRightOperand(), data.getResultId()))
							|| (isGivenVariable(primeTimesResult.getLeftOperand(), data.getResultId())
									&& isGivenVariable(primeTimesResult.getRightOperand(), data.getPrimeId())))) {
				return isNewHashValid(data, newHash);
			}
		}

		return false;
	}

	private boolean isNewHashValid(final CollectedData data, final Expression newHash) {
		if (newHash instanceof ParenthesizedExpression) {
			ParenthesizedExpression newHashWithoutBrackets= (ParenthesizedExpression) newHash;

			return isNewHashValid(data, newHashWithoutBrackets.getExpression());
		}

		if ((newHash instanceof Name || newHash instanceof FieldAccess || newHash instanceof SuperFieldAccess) && data.isTempValueUsed()) {
			SimpleName fieldName= getField(newHash);

			if (!ASTNodes.isSameVariable(data.getPrimeId(), fieldName)
					&& !ASTNodes.isSameVariable(data.getResultId(), fieldName)) {
				data.getFields().add(ASTNodes.getUnparenthesedExpression(fieldName));
				return true;
			}
		} else if (newHash instanceof ConditionalExpression && data.isTempValueUsed()) {
			ConditionalExpression condition= (ConditionalExpression) newHash;
			return isObjectValid(data, condition) || isBooleanValid(data, condition);
		} else if (newHash instanceof MethodInvocation && data.isTempValueUsed()) {
			MethodInvocation specificMethod= (MethodInvocation) newHash;
			TypeDeclaration innerClass= ASTNodes.getTypedAncestor(newHash, TypeDeclaration.class);
			TypeDeclaration topLevelClass= ASTNodes.getTypedAncestor(innerClass, TypeDeclaration.class);

			if (ASTNodes.usesGivenSignature(specificMethod, Float.class.getCanonicalName(), "floatToIntBits", float.class.getSimpleName())) { //$NON-NLS-1$
				SimpleName fieldName= getField((Expression) specificMethod.arguments().get(0));

				if (fieldName != null && !ASTNodes.isSameVariable(fieldName, data.getPrimeId())
						&& !ASTNodes.isSameVariable(fieldName, data.getResultId())) {
					data.getFields().add(ASTNodes.getUnparenthesedExpression(fieldName));
					return true;
				}
			} else if (ASTNodes.usesGivenSignature(specificMethod, Arrays.class.getCanonicalName(), HASH_CODE_METHOD, boolean[].class.getCanonicalName())
					|| ASTNodes.usesGivenSignature(specificMethod, Arrays.class.getCanonicalName(), HASH_CODE_METHOD, byte[].class.getCanonicalName())
					|| ASTNodes.usesGivenSignature(specificMethod, Arrays.class.getCanonicalName(), HASH_CODE_METHOD, char[].class.getCanonicalName())
					|| ASTNodes.usesGivenSignature(specificMethod, Arrays.class.getCanonicalName(), HASH_CODE_METHOD, double[].class.getCanonicalName())
					|| ASTNodes.usesGivenSignature(specificMethod, Arrays.class.getCanonicalName(), HASH_CODE_METHOD, float[].class.getCanonicalName())
					|| ASTNodes.usesGivenSignature(specificMethod, Arrays.class.getCanonicalName(), HASH_CODE_METHOD, int[].class.getCanonicalName())
					|| ASTNodes.usesGivenSignature(specificMethod, Arrays.class.getCanonicalName(), HASH_CODE_METHOD, Object[].class.getCanonicalName())
					|| ASTNodes.usesGivenSignature(specificMethod, Arrays.class.getCanonicalName(), HASH_CODE_METHOD, long[].class.getCanonicalName())
					|| ASTNodes.usesGivenSignature(specificMethod, Arrays.class.getCanonicalName(), HASH_CODE_METHOD, short[].class.getCanonicalName())) {
				SimpleName fieldName= getField((Expression) specificMethod.arguments().get(0));

				if (fieldName != null && !ASTNodes.isSameVariable(fieldName, data.getPrimeId())
						&& !ASTNodes.isSameVariable(fieldName, data.getResultId())) {
					data.getFields().add(ASTNodes.getUnparenthesedExpression(specificMethod));
					return true;
				}
			} else if (innerClass != null
					&& innerClass.resolveBinding() != null
					&& topLevelClass != null
					&& topLevelClass.resolveBinding() != null
							&& ASTNodes.usesGivenSignature(specificMethod, topLevelClass.resolveBinding().getQualifiedName(), HASH_CODE_METHOD)) {
				return isEnclosingHashCode(data, specificMethod, innerClass, topLevelClass);
			}
		} else if (newHash instanceof CastExpression) {
			return isGreatNumberValid(data, (CastExpression) newHash);
		}

		return false;
	}

	private boolean isEnclosingHashCode(final CollectedData data, final MethodInvocation specificMethod,
			final TypeDeclaration innerClass, final TypeDeclaration topLevelClass) {
		MethodInvocation getEnclosingInstanceMethod= ASTNodes.as(specificMethod.getExpression(), MethodInvocation.class);

		if (ASTNodes.usesGivenSignature(getEnclosingInstanceMethod, innerClass.resolveBinding().getQualifiedName(), "getEnclosingInstance")) { //$NON-NLS-1$
			MethodDeclaration getEnclosingInstanceDeclaration= null;

			for (MethodDeclaration innerMethod : innerClass.getMethods()) {
				if ("getEnclosingInstance".equals(innerMethod.getName().getIdentifier()) //$NON-NLS-1$
						&& Utils.isEmpty(innerMethod.parameters())
						&& !innerMethod.isConstructor() && innerMethod.resolveBinding() != null
						&& ASTNodes.hasType(innerMethod.resolveBinding().getReturnType(), topLevelClass.resolveBinding().getQualifiedName())) {
					getEnclosingInstanceDeclaration= innerMethod;
					break;
				}
			}

			if (getEnclosingInstanceDeclaration != null) {
				List<Statement> methodStatements= ASTNodes.asList(getEnclosingInstanceDeclaration.getBody());

				if (methodStatements != null && methodStatements.size() == 1) {
					ReturnStatement returnStatement= ASTNodes.as(methodStatements.get(0), ReturnStatement.class);

					if (returnStatement != null) {
						ThisExpression thisExpression= ASTNodes.as(returnStatement.getExpression(),
								ThisExpression.class);

						if (thisExpression != null) {
							SimpleName topLevelClassReference= ASTNodes.as(thisExpression.getQualifier(),
									SimpleName.class);

							if (topLevelClassReference != null
									&& topLevelClass.getName().getIdentifier().equals(topLevelClassReference.getIdentifier())) {
								data.getFields().add(ASTNodes.getUnparenthesedExpression(specificMethod));
								return true;
							}
						}
					}
				}
			}
		}

		return false;
	}

	private SimpleName getField(final Expression expression) {
		SimpleName simpleName= ASTNodes.as(expression, SimpleName.class);

		if (simpleName != null) {
			return simpleName;
		}

		FieldAccess fieldName= ASTNodes.as(expression, FieldAccess.class);

		if (fieldName != null) {
			ThisExpression te= ASTNodes.as(fieldName.getExpression(), ThisExpression.class);

			if (te != null) {
				if (te.getQualifier() == null) {
					return fieldName.getName();
				}

				if (te.getQualifier().isSimpleName()) {
					SimpleName qualifier= (SimpleName) te.getQualifier();
					TypeDeclaration visitedClass= ASTNodes.getTypedAncestor(expression, TypeDeclaration.class);

					if (visitedClass != null
							&& ASTNodes.isSameVariable(visitedClass.getName(), qualifier)) {
						return fieldName.getName();
					}
				}
			}
		}

		SuperFieldAccess superFieldAccess= ASTNodes.as(expression, SuperFieldAccess.class);

		if (superFieldAccess != null) {
			if (superFieldAccess.getQualifier() == null) {
				return superFieldAccess.getName();
			}

			if (superFieldAccess.getQualifier().isSimpleName()) {
				SimpleName qualifier= (SimpleName) superFieldAccess.getQualifier();
				TypeDeclaration visitedClass= ASTNodes.getTypedAncestor(expression, TypeDeclaration.class);

				if (visitedClass != null
						&& ASTNodes.isSameVariable(visitedClass.getName(), qualifier)) {
					return superFieldAccess.getName();
				}
			}
		}

		return null;
	}

	private boolean isGreatNumberValid(final CollectedData data, final CastExpression newHash) {
		OrderedInfixExpression<Expression, InfixExpression> orderedBitwise= ASTNodes.orderedInfix(newHash.getExpression(), Expression.class, InfixExpression.class);

		if (ASTNodes.hasType(newHash, int.class.getSimpleName())
				&& orderedBitwise != null
				&& ASTNodes.hasType(newHash.getExpression(), long.class.getSimpleName(), double.class.getSimpleName())
				&& InfixExpression.Operator.XOR.equals(orderedBitwise.getOperator())) {
			SimpleName field= getField(orderedBitwise.getFirstOperand());
			InfixExpression moveExpression= orderedBitwise.getSecondOperand();

			if (field != null && moveExpression != null && !ASTNodes.isSameVariable(field, data.getPrimeId())
					&& !ASTNodes.isSameVariable(field, data.getResultId()) && ASTNodes.hasOperator(moveExpression, InfixExpression.Operator.RIGHT_SHIFT_UNSIGNED)) {
				SimpleName againFieldName= getField(moveExpression.getLeftOperand());
				Long hash= ASTNodes.getIntegerLiteral(moveExpression.getRightOperand());

				if (againFieldName != null && ASTNodes.isSameVariable(againFieldName, field) && Long.valueOf(32).equals(hash)) {
					if (data.isTempValueUsed()) {
						data.getFields().add(ASTNodes.getUnparenthesedExpression(againFieldName));
						return true;
					}

					if (ASTNodes.isSameVariable(data.getTempVar(), field)) {
						data.setTempValueUsed(true);
						return true;
					}
				}
			}
		}

		return false;
	}

	private boolean isBooleanValid(final CollectedData data, final ConditionalExpression newHash) {
		SimpleName booleanField= getField(newHash.getExpression());
		Long hashForTrue= ASTNodes.getIntegerLiteral(newHash.getThenExpression());
		Long hashForFalse= ASTNodes.getIntegerLiteral(newHash.getElseExpression());

		if (booleanField != null && hashForTrue != null
				&& hashForFalse != null && ASTNodes.hasType(booleanField, boolean.class.getSimpleName())
				&& !ASTNodes.isSameVariable(booleanField, data.getPrimeId())
				&& !ASTNodes.isSameVariable(booleanField, data.getResultId()) && Long.valueOf(1231).equals(hashForTrue)
				&& Long.valueOf(1237).equals(hashForFalse)) {
			data.getFields().add(ASTNodes.getUnparenthesedExpression(booleanField));
			return true;
		}

		return false;
	}

	private boolean isObjectValid(final CollectedData data, final ConditionalExpression condition) {
		OrderedInfixExpression<Expression, NullLiteral> orderedIsFieldNull= ASTNodes.orderedInfix(condition.getExpression(), Expression.class, NullLiteral.class);

		if (orderedIsFieldNull != null
				&& Arrays.asList(InfixExpression.Operator.EQUALS, InfixExpression.Operator.NOT_EQUALS).contains(orderedIsFieldNull.getOperator())) {
			SimpleName field= getField(orderedIsFieldNull.getFirstOperand());

			if (field != null) {
				Long zero;
				MethodInvocation hashOnField;

				if (InfixExpression.Operator.EQUALS.equals(orderedIsFieldNull.getOperator())) {
					zero= ASTNodes.getIntegerLiteral(condition.getThenExpression());
					hashOnField= ASTNodes.as(condition.getElseExpression(), MethodInvocation.class);
				} else {
					hashOnField= ASTNodes.as(condition.getThenExpression(), MethodInvocation.class);
					zero= ASTNodes.getIntegerLiteral(condition.getElseExpression());
				}

				if (zero != null && zero.longValue() == 0 && hashOnField != null && hashOnField.getExpression() != null
						&& HASH_CODE_METHOD.equals(hashOnField.getName().getIdentifier())
						&& Utils.isEmpty(hashOnField.arguments())) {
					SimpleName fieldToHash= getField(hashOnField.getExpression());

					if (fieldToHash != null
							&& ASTNodes.isSameVariable(field, fieldToHash)) {
						data.getFields().add(ASTNodes.getUnparenthesedExpression(fieldToHash));
						return true;
					}
				}
			}
		}

		return false;
	}

	private boolean isGivenVariable(final Expression expression, final SimpleName varId) {
		SimpleName field= getField(expression);
		return field != null && ASTNodes.isSameVariable(varId, field);
	}

	private void refactorHash(final MethodDeclaration node, final Set<String> classesToUseWithImport,
			final Set<String> importsToAdd, final CollectedData data) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.Java7HashRatherThanEclipseJava6HashCleanUp_description);

		@SuppressWarnings("unchecked")
		List<Statement> statements= node.getBody().statements();
		String classname= addImport(Objects.class, classesToUseWithImport, importsToAdd);
		Name objectsClassName= ast.name(classname);

		rewrite.replace(statements.get(0),
				ast.return0(ast.newMethodInvocation(objectsClassName, "hash", rewrite.createMoveTarget(data.getFields()))), group); //$NON-NLS-1$

		for (int i= 1; i < statements.size(); i++) {
			rewrite.remove(statements.get(i), group);
		}
	}
}

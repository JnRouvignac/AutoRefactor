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

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.autorefactor.jdt.internal.corext.dom.Release;
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
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/** See {@link #getDescription()} method. */
public class Java7HashRatherThanEclipseJava6HashCleanUp extends NewClassImportCleanUp {
    private final class CollectedData {
        private List<Expression> fields= new ArrayList<>();
        private String primeId;
        private String resultId;
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
        public String getPrimeId() {
            return primeId;
        }

        /**
         * @param primeId the primeId to set
         */
        public void setPrimeId(final String primeId) {
            this.primeId= primeId;
        }

        /**
         * @return the resultId
         */
        public String getResultId() {
            return resultId;
        }

        /**
         * @param resultId the resultId to set
         */
        public void setResultId(final String resultId) {
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
            return Java7HashRatherThanEclipseJava6HashCleanUp.this
                    .maybeRefactorMethodDeclaration(node, getClassesToUseWithImport(), getImportsToAdd());
        }
    }

    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_Java7HashRatherThanEclipseJava6HashCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_Java7HashRatherThanEclipseJava6HashCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_Java7HashRatherThanEclipseJava6HashCleanUp_reason;
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
        final Block body= node.getBody();

        if (ASTNodes.usesGivenSignature(node, Object.class.getCanonicalName(), "hashCode") && body != null) { //$NON-NLS-1$
            @SuppressWarnings("unchecked")
            final List<Statement> statements= body.statements();

            if (statements.size() > 2) {
                final CollectedData data= new CollectedData();
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
                        refactorHash(node, classesToUseWithImport, data);
                        importsToAdd.add(Objects.class.getCanonicalName());
                        return false;
                    }
                }
            }
        }

        return true;
    }

    private String isVariableValid(final CollectedData data, final int initValue) {
        final Statement statement= data.getStmtIterator().next();
        final VariableDeclarationStatement varDecl= ASTNodes.as(statement, VariableDeclarationStatement.class);

        if (varDecl != null && ASTNodes.hasType(varDecl.getType().resolveBinding(), int.class.getSimpleName()) && varDecl.fragments().size() == 1) {
            final VariableDeclarationFragment varFragment= (VariableDeclarationFragment) varDecl.fragments().get(0);
            final String varId= varFragment.getName().getIdentifier();
            final NumberLiteral varLiteral= ASTNodes.as(varFragment.getInitializer(), NumberLiteral.class);

            if (varFragment.getExtraDimensions() == 0 && varLiteral != null) {
                final Object varValue= varLiteral.resolveConstantExpressionValue();

                if (varValue instanceof Number && ((Number) varValue).intValue() == initValue) {
                    return varId;
                }
            }
        }

        return null;
    }

    private boolean isStmtValid(final CollectedData data) {
        final Statement statement= data.getStmtIterator().next();
        final ExpressionStatement exprStatement= ASTNodes.as(statement, ExpressionStatement.class);
        final VariableDeclarationStatement varStatement= ASTNodes.as(statement, VariableDeclarationStatement.class);
        final ReturnStatement returnStatement= ASTNodes.as(statement, ReturnStatement.class);

        if (exprStatement != null) {
            return isAssignmentValid(data, exprStatement);
        }
        if (varStatement != null && data.getTempVar() == null) {
            @SuppressWarnings("unchecked")
            final List<VariableDeclarationFragment> fragments= varStatement.fragments();

            if (ASTNodes.hasType(varStatement.getType().resolveBinding(), long.class.getSimpleName()) && fragments != null && fragments.size() == 1) {
                final VariableDeclarationFragment fragment= fragments.get(0);
                data.setTempVar(fragment.getName());
                final Expression initializer= fragment.getInitializer();

                if (fragment.getExtraDimensions() == 0) {
                    if (initializer != null) {
                        final SimpleName fieldToFind= isDoubleToLongBitsMethod(data, initializer);
                        data.setTempValueUsed(false);

                        if (fieldToFind != null && data.getStmtIterator().hasNext()) {
                            final boolean assignmentValid= isStmtValid(data);

                            if (assignmentValid) {
                                data.getFields().add(fieldToFind);
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
            final Expression expression= returnStatement.getExpression();

            return returnStatement != null && (isGivenVariable(expression, data.getResultId()) || isHashValid(data, expression));
        }

        return false;
    }

    private boolean isAssignmentValid(final CollectedData data, final ExpressionStatement statement) {
        final Assignment assignment= ASTNodes.as(statement.getExpression(), Assignment.class);

        if (assignment != null && ASTNodes.hasOperator(assignment, Assignment.Operator.ASSIGN)) {
            final Expression field= assignment.getLeftHandSide();
            final Expression resultComputation= assignment.getRightHandSide();

            if (isGivenVariable(field, data.getResultId())) {
                return isHashValid(data, resultComputation);
            }
            if (data.getTempVar() != null && isGivenVariable(field, data.getTempVar().getIdentifier())) {
                final SimpleName fieldToFind= isDoubleToLongBitsMethod(data, resultComputation);

                if (fieldToFind != null && data.getStmtIterator().hasNext()) {
                    data.setTempValueUsed(false);
                    final boolean assignmentValid= isStmtValid(data);

                    if (assignmentValid) {
                        data.getFields().add(fieldToFind);
                        return true;
                    }
                }
            }
        }

        return false;
    }

    private SimpleName isDoubleToLongBitsMethod(final CollectedData data, final Expression initializer) {
        SimpleName fieldToFind= null;
        final MethodInvocation doubleToLongBits= ASTNodes.as(initializer, MethodInvocation.class);

        if (doubleToLongBits != null && ASTNodes.usesGivenSignature(doubleToLongBits, Double.class.getCanonicalName(), "doubleToLongBits", double.class.getSimpleName())) { //$NON-NLS-1$
            final SimpleName fieldName= ASTNodes.as((Expression) doubleToLongBits.arguments().get(0), SimpleName.class);

            if (fieldName != null && !fieldName.getIdentifier().equals(data.getPrimeId())
                    && !fieldName.getIdentifier().equals(data.getResultId())) {
                fieldToFind= fieldName;
            }
        }

        return fieldToFind;
    }

    private boolean isHashValid(final CollectedData data, final Expression hashComputation) {
        final InfixExpression hashAddition= ASTNodes.as(hashComputation, InfixExpression.class);

        if (hashAddition != null) {
            final InfixExpression primeTimesResult= ASTNodes.as(hashAddition.getLeftOperand(), InfixExpression.class);
            final Expression newHash= hashAddition.getRightOperand();

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
            final ParenthesizedExpression newHashWithoutBrackets= (ParenthesizedExpression) newHash;

            return isNewHashValid(data, newHashWithoutBrackets.getExpression());
        }
        if ((newHash instanceof Name || newHash instanceof FieldAccess) && data.isTempValueUsed()) {
            final SimpleName fieldName= getField(newHash);

            if (!data.getPrimeId().equals(fieldName.getIdentifier())
                    && !data.getResultId().equals(fieldName.getIdentifier())) {
                data.getFields().add(fieldName);
                return true;
            }
        } else if (newHash instanceof ConditionalExpression && data.isTempValueUsed()) {
            return isConditionValid(data, newHash);
        } else if (newHash instanceof MethodInvocation && data.isTempValueUsed()) {
            final MethodInvocation specificMethod= (MethodInvocation) newHash;

            if (ASTNodes.usesGivenSignature(specificMethod, Float.class.getCanonicalName(), "floatToIntBits", float.class.getSimpleName())) { //$NON-NLS-1$
                final SimpleName fieldName= getField((Expression) specificMethod.arguments().get(0));

                if (fieldName != null && !fieldName.getIdentifier().equals(data.getPrimeId())
                        && !fieldName.getIdentifier().equals(data.getResultId())) {
                    data.getFields().add(fieldName);
                    return true;
                }
            } else if (ASTNodes.usesGivenSignature(specificMethod, Arrays.class.getCanonicalName(), "hashCode", "boolean[]") //$NON-NLS-1$ //$NON-NLS-2$
                    || ASTNodes.usesGivenSignature(specificMethod, Arrays.class.getCanonicalName(), "hashCode", "byte[]") //$NON-NLS-1$ //$NON-NLS-2$
                    || ASTNodes.usesGivenSignature(specificMethod, Arrays.class.getCanonicalName(), "hashCode", "char[]") //$NON-NLS-1$ //$NON-NLS-2$
                    || ASTNodes.usesGivenSignature(specificMethod, Arrays.class.getCanonicalName(), "hashCode", "double[]") //$NON-NLS-1$ //$NON-NLS-2$
                    || ASTNodes.usesGivenSignature(specificMethod, Arrays.class.getCanonicalName(), "hashCode", "float[]") //$NON-NLS-1$ //$NON-NLS-2$
                    || ASTNodes.usesGivenSignature(specificMethod, Arrays.class.getCanonicalName(), "hashCode", "int[]") //$NON-NLS-1$ //$NON-NLS-2$
                    || ASTNodes.usesGivenSignature(specificMethod, Arrays.class.getCanonicalName(), "hashCode", Object[].class.getCanonicalName()) //$NON-NLS-1$
                    || ASTNodes.usesGivenSignature(specificMethod, Arrays.class.getCanonicalName(), "hashCode", "long[]") //$NON-NLS-1$ //$NON-NLS-2$
                    || ASTNodes.usesGivenSignature(specificMethod, Arrays.class.getCanonicalName(), "hashCode", "short[]")) { //$NON-NLS-1$ //$NON-NLS-2$
                final SimpleName fieldName= getField((Expression) specificMethod.arguments().get(0));

                if (fieldName != null && !fieldName.getIdentifier().equals(data.getPrimeId())
                        && !fieldName.getIdentifier().equals(data.getResultId())) {
                    data.getFields().add(specificMethod);
                    return true;
                }
            }
        } else if (newHash instanceof CastExpression) {
            return isGreatNumberValid(data, newHash);
        }

        return false;
    }

    private SimpleName getField(final Expression expression) {
        final SimpleName simpleName= ASTNodes.as(expression, SimpleName.class);
        final FieldAccess fieldName= ASTNodes.as(expression, FieldAccess.class);

        if (simpleName != null) {
            return simpleName;
        }
        if (fieldName != null) {
            final ThisExpression te= ASTNodes.as(fieldName.getExpression(), ThisExpression.class);

            if (te != null) {
                if (te.getQualifier() == null) {
                    return fieldName.getName();
                }
                if (te.getQualifier().isSimpleName()) {
                    SimpleName qualifier= (SimpleName) te.getQualifier();
                    TypeDeclaration visitedClass= ASTNodes.getAncestorOrNull(expression, TypeDeclaration.class);

                    if (visitedClass != null
                            && visitedClass.getName().getIdentifier().equals(qualifier.getIdentifier())) {
                        return fieldName.getName();
                    }
                }
            }
        }

        return null;
    }

    private boolean isGreatNumberValid(final CollectedData data, final Expression newHash) {
        final CastExpression castExpression= (CastExpression) newHash;
        final InfixExpression bitwise= ASTNodes.as(castExpression.getExpression(), InfixExpression.class);

        if (ASTNodes.hasType(castExpression, int.class.getSimpleName()) && bitwise != null && ASTNodes.hasType(bitwise, long.class.getSimpleName(), double.class.getSimpleName())
                && ASTNodes.hasOperator(bitwise, InfixExpression.Operator.XOR)) {
            final Expression operand1= bitwise.getLeftOperand();
            final Expression operand2= bitwise.getRightOperand();

            final SimpleName field1= getField(operand1);
            final InfixExpression moveExpr1= ASTNodes.as(operand2, InfixExpression.class);
            final InfixExpression moveExpr2= ASTNodes.as(operand1, InfixExpression.class);
            final SimpleName field2= getField(operand2);

            final String fieldName;
            final InfixExpression moveExpression;

            if (field1 != null && moveExpr1 != null && !field1.getIdentifier().equals(data.getPrimeId())
                    && !field1.getIdentifier().equals(data.getResultId())) {
                fieldName= field1.getIdentifier();
                moveExpression= moveExpr1;
            } else if (field2 != null && moveExpr2 != null && !field2.getIdentifier().equals(data.getPrimeId())
                    && !field2.getIdentifier().equals(data.getResultId())) {
                fieldName= field2.getIdentifier();
                moveExpression= moveExpr2;
            } else {
                fieldName= null;
                moveExpression= null;
            }

            if (fieldName != null && moveExpression != null && ASTNodes.hasOperator(moveExpression, InfixExpression.Operator.RIGHT_SHIFT_UNSIGNED)) {
                final SimpleName againFieldName= getField(moveExpression.getLeftOperand());
                final NumberLiteral hash= ASTNodes.as(moveExpression.getRightOperand(), NumberLiteral.class);

                if (againFieldName != null && againFieldName.getIdentifier().equals(fieldName) && hash != null) {
                    final Object numberForHash= hash.resolveConstantExpressionValue();

                    if (numberForHash instanceof Number && ((Number) numberForHash).intValue() == 32) {
                        if (data.isTempValueUsed()) {
                            data.getFields().add(againFieldName);
                            return true;
                        }
                        if (data.getTempVar().getIdentifier().equals(fieldName)) {
                            data.setTempValueUsed(true);
                            return true;
                        }
                    }
                }
            }
        }

        return false;
    }

    private boolean isConditionValid(final CollectedData data, final Expression newHash) {
        final ConditionalExpression condition= (ConditionalExpression) newHash;
        final InfixExpression isFieldNull= ASTNodes.as(condition.getExpression(), InfixExpression.class);

        final SimpleName booleanField= getField(condition.getExpression());
        final NumberLiteral hashForTrue= ASTNodes.as(condition.getThenExpression(), NumberLiteral.class);
        final NumberLiteral hashForFalse= ASTNodes.as(condition.getElseExpression(), NumberLiteral.class);

        if (isFieldNull != null && !isFieldNull.hasExtendedOperands()
                && ASTNodes.hasOperator(isFieldNull, InfixExpression.Operator.EQUALS, InfixExpression.Operator.NOT_EQUALS)) {
            return isObjectValid(data, condition, isFieldNull);
        }

        return booleanField != null && ASTNodes.hasType(booleanField, boolean.class.getSimpleName())
                && !booleanField.getIdentifier().equals(data.getPrimeId())
                && !booleanField.getIdentifier().equals(data.getResultId()) && hashForTrue != null
                && hashForFalse != null && isBooleanValid(data, booleanField, hashForTrue, hashForFalse);
    }

    private boolean isBooleanValid(final CollectedData data, final SimpleName booleanField,
            final NumberLiteral hashForTrue, final NumberLiteral hashForFalse) {
        final Object numberForTrue= hashForTrue.resolveConstantExpressionValue();
        final Object numberForFalse= hashForFalse.resolveConstantExpressionValue();

        if (numberForTrue instanceof Number && ((Number) numberForTrue).intValue() == 1231
                && numberForFalse instanceof Number && ((Number) numberForFalse).intValue() == 1237) {
            data.getFields().add(booleanField);
            return true;
        }

        return false;
    }

    private boolean isObjectValid(final CollectedData data, final ConditionalExpression condition,
            final InfixExpression isFieldNull) {
        final Expression operand1= isFieldNull.getLeftOperand();
        final Expression operand2= isFieldNull.getRightOperand();

        final SimpleName field1= getField(operand1);
        final NullLiteral nullLiteral1= ASTNodes.as(operand2, NullLiteral.class);
        final NullLiteral nullLiteral2= ASTNodes.as(operand1, NullLiteral.class);
        final SimpleName field2= getField(operand2);

        final String fieldName;

        if (field1 != null && nullLiteral1 != null && !field1.getIdentifier().equals(data.getPrimeId())
                && !field1.getIdentifier().equals(data.getResultId())) {
            fieldName= field1.getIdentifier();
        } else if (field2 != null && nullLiteral2 != null && !field2.getIdentifier().equals(data.getPrimeId())
                && !field2.getIdentifier().equals(data.getResultId())) {
            fieldName= field2.getIdentifier();
        } else {
            fieldName= null;
        }

        if (fieldName != null) {
            final NumberLiteral zero;
            final MethodInvocation hashOnField;

            if (ASTNodes.hasOperator(isFieldNull, InfixExpression.Operator.EQUALS)) {
                zero= ASTNodes.as(condition.getThenExpression(), NumberLiteral.class);
                hashOnField= ASTNodes.as(condition.getElseExpression(), MethodInvocation.class);
            } else {
                hashOnField= ASTNodes.as(condition.getThenExpression(), MethodInvocation.class);
                zero= ASTNodes.as(condition.getElseExpression(), NumberLiteral.class);
            }

            if (zero != null && hashOnField != null && hashOnField.getExpression() != null
                    && "hashCode".equals(hashOnField.getName().getIdentifier()) //$NON-NLS-1$
                    && (hashOnField.arguments() == null || hashOnField.arguments().isEmpty())) {
                final Object zeroValue= zero.resolveConstantExpressionValue();
                final SimpleName fieldToHash= getField(hashOnField.getExpression());

                if (zeroValue instanceof Number && ((Number) zeroValue).intValue() == 0 && fieldToHash != null
                        && fieldName.equals(fieldToHash.getIdentifier())) {
                    data.getFields().add(fieldToHash);
                    return true;
                }
            }
        }

        return false;
    }

    private boolean isGivenVariable(final Expression expression, final String varId) {
        final SimpleName field= getField(expression);
        return field != null && varId.equals(field.getIdentifier());
    }

    private void refactorHash(final MethodDeclaration node, final Set<String> classesToUseWithImport,
            final CollectedData data) {
        final ASTNodeFactory b= this.ctx.getASTBuilder();
        final Refactorings r= this.ctx.getRefactorings();

        @SuppressWarnings("unchecked")
        final List<Statement> statements= node.getBody().statements();
        final Name objectsClassName= b.name(classesToUseWithImport.contains(Objects.class.getCanonicalName()) ? Objects.class.getSimpleName() : Objects.class.getCanonicalName());

        r.replace(statements.get(0),
                b.return0(b.invoke(objectsClassName, "hash", b.createMoveTarget(data.getFields())))); //$NON-NLS-1$

        for (int i= 1; i < statements.size(); i++) {
            r.remove(statements.get(i));
        }
    }
}

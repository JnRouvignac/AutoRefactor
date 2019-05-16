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
package org.autorefactor.refactoring.rules;

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.as;
import static org.autorefactor.refactoring.ASTHelper.getAncestorOrNull;
import static org.autorefactor.refactoring.ASTHelper.hasType;
import static org.autorefactor.refactoring.ASTHelper.isMethod;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.Release;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
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
public class Java7HashRatherThanEclipseJava6HashRefactoring extends NewClassImportRefactoring {
    private final class CollectedData {
        private List<Expression> fields = new ArrayList<Expression>();
        private String primeId;
        private String resultId;
        private Iterator<Statement> stmtIterator;
        private SimpleName tempVar;
        private boolean tempValueUsed = true;
        private boolean hasReturnStmt;

        /**
         * Get the current value.
         *
         * @return the current value
         */
        public final SimpleName getTempVar() {
            return tempVar;
        }

        /**
         * Set to the given value.
         *
         * @param tempVar the new value
         */
        public final void setTempVar(final SimpleName tempVar) {
            this.tempVar = tempVar;
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
            this.primeId = primeId;
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
            this.resultId = resultId;
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
            this.stmtIterator = stmtIterator;
        }

        /**
         * @return the hasReturnStmt
         */
        public boolean isHasReturnStmt() {
            return hasReturnStmt;
        }

        /**
         * @param hasReturnStmt the hasReturnStmt to set
         */
        public void setHasReturnStmt(final boolean hasReturnStmt) {
            this.hasReturnStmt = hasReturnStmt;
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
            this.tempValueUsed = tempValueUsed;
        }

        /**
         * @return the fields
         */
        public List<Expression> getFields() {
            return fields;
        }
    }

    private final class RefactoringWithObjectsClass extends RefactoringWithNewClassImport {
        public RefactoringWithObjectsClass(final RefactoringContext context) {
            ctx = context;
        }

        @Override
        public boolean visit(final MethodDeclaration node) {
            final boolean isSubTreeToVisit =
                    Java7HashRatherThanEclipseJava6HashRefactoring.this.maybeRefactorMethodDeclaration(node,
                            getClassesToUseWithImport(), getImportsToAdd());

            return isSubTreeToVisit;
        }
    }

    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Java 7 hash rather than Eclipse Java 6 hash";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Rewrites Eclipse-autogenerated hashcode method by Eclipse-autogenerated hashcode method for Java 7.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves readibility. "
                + "It does not improve performance.";
    }

    @Override
    public Set<String> getClassesToImport() {
        return new HashSet<String>(Arrays.asList("java.util.Objects"));
    }

    @Override
    public RefactoringWithNewClassImport getRefactoringClassInstance() {
        return new RefactoringWithObjectsClass(ctx);
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
            final Set<String> classesToUseWithImport,
            final Set<String> importsToAdd) {
        final Block body = node.getBody();

        if (isMethod(node,
                "java.lang.Object", "hashCode")
                && body != null) {
            @SuppressWarnings("unchecked")
            final List<Statement> stmts = body.statements();

            if (stmts.size() > 2) {
                final CollectedData data = new CollectedData();
                data.setStmtIterator(stmts.iterator());

                data.setPrimeId(isVariableValid(data, 31));
                data.setResultId(isVariableValid(data, 1));

                if (data.getPrimeId() != null && data.getResultId() != null && data.getStmtIterator().hasNext()) {
                    while (!data.isHasReturnStmt() && data.getStmtIterator().hasNext()) {
                        if (!isStmtValid(data)) {
                            return VISIT_SUBTREE;
                        }
                    }

                    if (data.isHasReturnStmt() && !data.getStmtIterator().hasNext()) {
                        refactorHash(node, classesToUseWithImport, data);
                        importsToAdd.add("java.util.Objects");
                        return DO_NOT_VISIT_SUBTREE;
                    }
                }
            }
        }

        return VISIT_SUBTREE;
    }

    private String isVariableValid(final CollectedData data, final int initValue) {
        final Statement stmt = data.getStmtIterator().next();
        final VariableDeclarationStatement varDecl = as(stmt, VariableDeclarationStatement.class);

        if (varDecl != null && hasType(varDecl.getType().resolveBinding(), "int")
                && varDecl.fragments().size() == 1) {
            final VariableDeclarationFragment varFragment =
                    (VariableDeclarationFragment) varDecl.fragments().get(0);
            final String varId = varFragment.getName().getIdentifier();
            final NumberLiteral varLiteral = as(varFragment.getInitializer(), NumberLiteral.class);

            if (varFragment.getExtraDimensions() == 0 && varLiteral != null) {
                final Object varValue = varLiteral.resolveConstantExpressionValue();

                if ((varValue instanceof Number)
                        && ((Number) varValue).intValue() == initValue) {
                    return varId;
                }
            }
        }

        return null;
    }

    private boolean isStmtValid(final CollectedData data) {
        final Statement stmt = data.getStmtIterator().next();
        final ExpressionStatement exprStmt = as(stmt, ExpressionStatement.class);
        final VariableDeclarationStatement varStmt = as(stmt, VariableDeclarationStatement.class);
        final ReturnStatement returnStmt = as(stmt, ReturnStatement.class);

        if (exprStmt != null) {
            return isAssignmentValid(data, exprStmt);
        } else if (varStmt != null && data.getTempVar() == null) {
            @SuppressWarnings("unchecked")
            final List<VariableDeclarationFragment> fragments = varStmt.fragments();

            if (hasType(varStmt.getType().resolveBinding(), "long") && fragments != null && fragments.size() == 1) {
                final VariableDeclarationFragment fragment = fragments.get(0);
                data.setTempVar(fragment.getName());
                final Expression initializer = fragment.getInitializer();

                if (fragment.getExtraDimensions() == 0) {
                    if (initializer != null) {
                        final SimpleName fieldToFind = isDoubleToLongBitsMethod(data, initializer);
                        data.setTempValueUsed(false);

                        if (fieldToFind != null && data.getStmtIterator().hasNext()) {
                            final boolean assignmentValid = isStmtValid(data);

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
        } else if (returnStmt != null) {
            data.setHasReturnStmt(true);
            final Expression expr = returnStmt.getExpression();

            return returnStmt != null && (isGivenVariable(expr, data.getResultId()) || isHashValid(data, expr));
        }

        return false;
    }

    private boolean isAssignmentValid(final CollectedData data, final ExpressionStatement stmt) {
        final Expression resultExpr = stmt.getExpression();

        if (resultExpr instanceof Assignment) {
            final Assignment asgmnt = (Assignment) resultExpr;
            final Expression field = asgmnt.getLeftHandSide();
            final Expression resultComputation = asgmnt.getRightHandSide();

            if (isGivenVariable(field, data.getResultId())) {
                return isHashValid(data, resultComputation);
            } else if (data.getTempVar() != null
                    && isGivenVariable(field, data.getTempVar().getIdentifier())) {
                final SimpleName fieldToFind = isDoubleToLongBitsMethod(data, resultComputation);

                if (fieldToFind != null && data.getStmtIterator().hasNext()) {
                    data.setTempValueUsed(false);
                    final boolean assignmentValid = isStmtValid(data);

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
        SimpleName fieldToFind = null;
        final MethodInvocation doubleToLongBits = as(initializer, MethodInvocation.class);

        if (doubleToLongBits != null
                && isMethod(doubleToLongBits, "java.lang.Double", "doubleToLongBits", "double")) {
            final SimpleName fieldName = as((Expression) doubleToLongBits.arguments().get(0),
                    SimpleName.class);

            if (fieldName != null
                    && !fieldName.getIdentifier().equals(data.getPrimeId())
                    && !fieldName.getIdentifier().equals(data.getResultId())) {
                fieldToFind = fieldName;
            }
        }
        return fieldToFind;
    }

    private boolean isHashValid(final CollectedData data, final Expression hashComputation) {
        if (hashComputation instanceof InfixExpression) {
            final InfixExpression hashAddition = (InfixExpression) hashComputation;
            final InfixExpression primeTimesResult = as(hashAddition.getLeftOperand(), InfixExpression.class);
            final Expression newHash = hashAddition.getRightOperand();

            if (!hashAddition.hasExtendedOperands()
                    && Operator.PLUS.equals(hashAddition.getOperator())
                    && primeTimesResult != null
                    && !primeTimesResult.hasExtendedOperands()
                    && Operator.TIMES.equals(primeTimesResult.getOperator())
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
            final ParenthesizedExpression newHashWithoutBrackets = (ParenthesizedExpression) newHash;

            return isNewHashValid(data, newHashWithoutBrackets.getExpression());
        } else if ((newHash instanceof Name || newHash instanceof FieldAccess) && data.isTempValueUsed()) {
            final SimpleName fieldName = getField(newHash);

            if (!data.getPrimeId().equals(fieldName.getIdentifier())
                    && !data.getResultId().equals(fieldName.getIdentifier())) {
                data.getFields().add(fieldName);
                return true;
            }
        } else if (newHash instanceof ConditionalExpression && data.isTempValueUsed()) {
            return isConditionValid(data, newHash);
        } else if (newHash instanceof MethodInvocation && data.isTempValueUsed()) {
            final MethodInvocation specificMethod = (MethodInvocation) newHash;

            if (isMethod(specificMethod,
                    "java.lang.Float", "floatToIntBits", "float")) {
                final SimpleName fieldName = getField((Expression) specificMethod.arguments().get(0));

                if (fieldName != null && !fieldName.getIdentifier().equals(data.getPrimeId())
                        && !fieldName.getIdentifier().equals(data.getResultId())) {
                    data.getFields().add(fieldName);
                    return true;
                }
            } else if (isMethod(specificMethod, "java.util.Arrays", "hashCode", "boolean[]")
                    || isMethod(specificMethod, "java.util.Arrays", "hashCode", "byte[]")
                    || isMethod(specificMethod, "java.util.Arrays", "hashCode", "char[]")
                    || isMethod(specificMethod, "java.util.Arrays", "hashCode", "double[]")
                    || isMethod(specificMethod, "java.util.Arrays", "hashCode", "float[]")
                    || isMethod(specificMethod, "java.util.Arrays", "hashCode", "int[]")
                    || isMethod(specificMethod, "java.util.Arrays", "hashCode", "java.lang.Object[]")
                    || isMethod(specificMethod, "java.util.Arrays", "hashCode", "long[]")
                    || isMethod(specificMethod, "java.util.Arrays", "hashCode", "short[]")) {
                final SimpleName fieldName = getField((Expression) specificMethod.arguments().get(0));

                if (fieldName != null
                        && !fieldName.getIdentifier().equals(data.getPrimeId())
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

    private SimpleName getField(final Expression expr) {
        final SimpleName simpleName = as(expr, SimpleName.class);
        final FieldAccess fieldName = as(expr, FieldAccess.class);

        if (simpleName != null) {
            return simpleName;
        } else if (fieldName != null) {
            final ThisExpression te = as(fieldName.getExpression(), ThisExpression.class);

            if (te != null) {
                if (te.getQualifier() == null) {
                    return fieldName.getName();
                } else if (te.getQualifier().isSimpleName()) {
                    SimpleName qualifier = (SimpleName) te.getQualifier();
                    TypeDeclaration visitedClass = getAncestorOrNull(expr, TypeDeclaration.class);

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
        final CastExpression castExpression = (CastExpression) newHash;
        final InfixExpression bitwise = as(castExpression.getExpression(), InfixExpression.class);

        if (hasType(castExpression, "int")
                && bitwise != null
                && hasType(bitwise, "long", "double")
                && Operator.XOR.equals(bitwise.getOperator())) {
            final Expression operand1 = bitwise.getLeftOperand();
            final Expression operand2 = bitwise.getRightOperand();

            final SimpleName field1 = getField(operand1);
            final InfixExpression moveExpr1 = as(operand2, InfixExpression.class);
            final InfixExpression moveExpr2 = as(operand1, InfixExpression.class);
            final SimpleName field2 = getField(operand2);

            final String fieldName;
            final InfixExpression moveExpr;

            if (field1 != null && moveExpr1 != null
                    && !field1.getIdentifier().equals(data.getPrimeId())
                    && !field1.getIdentifier().equals(data.getResultId())) {
                fieldName = field1.getIdentifier();
                moveExpr = moveExpr1;
            } else if (field2 != null && moveExpr2 != null
                    && !field2.getIdentifier().equals(data.getPrimeId())
                    && !field2.getIdentifier().equals(data.getResultId())) {
                fieldName = field2.getIdentifier();
                moveExpr = moveExpr2;
            } else {
                fieldName = null;
                moveExpr = null;
            }

            if (fieldName != null
                    && moveExpr != null
                    && Operator.RIGHT_SHIFT_UNSIGNED.equals(moveExpr.getOperator())) {
                final SimpleName againFieldName = getField(moveExpr.getLeftOperand());
                final NumberLiteral hash = as(moveExpr.getRightOperand(), NumberLiteral.class);

                if (againFieldName != null
                        && againFieldName.getIdentifier().equals(fieldName)
                                && hash != null) {
                    final Object numberForHash = hash.resolveConstantExpressionValue();

                    if ((numberForHash instanceof Number)
                                    && ((Number) numberForHash).intValue() == 32) {
                        if (data.isTempValueUsed()) {
                            data.getFields().add(againFieldName);
                            return true;
                        } else if (data.getTempVar().getIdentifier().equals(fieldName)) {
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
        final ConditionalExpression condition = (ConditionalExpression) newHash;
        final InfixExpression isFieldNull = as(condition.getExpression(), InfixExpression.class);

        final SimpleName booleanField = getField(condition.getExpression());
        final NumberLiteral hashForTrue = as(condition.getThenExpression(), NumberLiteral.class);
        final NumberLiteral hashForFalse = as(condition.getElseExpression(), NumberLiteral.class);

        if (isFieldNull != null
                && !isFieldNull.hasExtendedOperands()
                && Arrays.asList(Operator.EQUALS, Operator.NOT_EQUALS).contains(isFieldNull.getOperator())) {
            return isObjectValid(data, condition, isFieldNull);
        } else if (booleanField != null
                && hasType(booleanField, "boolean")
                && !booleanField.getIdentifier().equals(data.getPrimeId())
                && !booleanField.getIdentifier().equals(data.getResultId())
                && hashForTrue != null
                && hashForFalse != null) {
            return isBooleanValid(data, booleanField, hashForTrue, hashForFalse);
        }

        return false;
    }

    private boolean isBooleanValid(final CollectedData data, final SimpleName booleanField,
            final NumberLiteral hashForTrue, final NumberLiteral hashForFalse) {
        final Object numberForTrue = hashForTrue.resolveConstantExpressionValue();
        final Object numberForFalse = hashForFalse.resolveConstantExpressionValue();

        if ((numberForTrue instanceof Number)
                && ((Number) numberForTrue).intValue() == 1231
                        && (numberForFalse instanceof Number)
                && ((Number) numberForFalse).intValue() == 1237) {
            data.getFields().add(booleanField);
            return true;
        }

        return false;
    }

    private boolean isObjectValid(final CollectedData data, final ConditionalExpression condition,
            final InfixExpression isFieldNull) {
        final Expression operand1 = isFieldNull.getLeftOperand();
        final Expression operand2 = isFieldNull.getRightOperand();

        final SimpleName field1 = getField(operand1);
        final NullLiteral nullLiteral1 = as(operand2, NullLiteral.class);
        final NullLiteral nullLiteral2 = as(operand1, NullLiteral.class);
        final SimpleName field2 = getField(operand2);

        final String fieldName;

        if (field1 != null && nullLiteral1 != null
                && !field1.getIdentifier().equals(data.getPrimeId())
                && !field1.getIdentifier().equals(data.getResultId())) {
            fieldName = field1.getIdentifier();
        } else if (field2 != null && nullLiteral2 != null
                && !field2.getIdentifier().equals(data.getPrimeId())
                && !field2.getIdentifier().equals(data.getResultId())) {
            fieldName = field2.getIdentifier();
        } else {
            fieldName = null;
        }

        if (fieldName != null) {
            final NumberLiteral zero;
            final MethodInvocation hashOnField;

            if (Operator.EQUALS.equals(isFieldNull.getOperator())) {
                zero = as(condition.getThenExpression(), NumberLiteral.class);
                hashOnField = as(condition.getElseExpression(), MethodInvocation.class);
            } else {
                hashOnField = as(condition.getThenExpression(), MethodInvocation.class);
                zero = as(condition.getElseExpression(), NumberLiteral.class);
            }

            if (zero != null
                    && hashOnField != null
                    && hashOnField.getExpression() != null
                    && "hashCode".equals(hashOnField.getName().getIdentifier())
                    && (hashOnField.arguments() == null || hashOnField.arguments().isEmpty())) {
                final Object zeroValue = zero.resolveConstantExpressionValue();
                final SimpleName fieldToHash = getField(hashOnField.getExpression());

                if ((zeroValue instanceof Number)
                        && ((Number) zeroValue).intValue() == 0
                        && fieldToHash != null
                        && fieldName.equals(fieldToHash.getIdentifier())) {
                    data.getFields().add(fieldToHash);
                    return true;
                }
            }
        }

        return false;
    }

    private boolean isGivenVariable(final Expression expression, final String varId) {
        final SimpleName expr = getField(expression);
        return expr != null && varId.equals(expr.getIdentifier());
    }

    private void refactorHash(final MethodDeclaration node, final Set<String> classesToUseWithImport,
            final CollectedData data) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        final Refactorings r = this.ctx.getRefactorings();

        @SuppressWarnings("unchecked")
        final List<Statement> stmts = node.getBody().statements();

        final List<Expression> copyOfFields = new ArrayList<Expression>(data.getFields().size());

        for (final Expression simpleName : data.getFields()) {
            copyOfFields.add(b.copy(simpleName));
        }

        r.replace(stmts.get(0), b.return0(b.invoke(classesToUseWithImport
                .contains("java.util.Objects") ? b.name("Objects") : b.name("java", "util", "Objects"),
                "hash",
                copyOfFields)));

        for (int i = 1; i < stmts.size(); i++) {
            r.remove(stmts.get(i));
        }
    }
}

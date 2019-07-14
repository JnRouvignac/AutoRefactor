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

import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.VISIT_SUBTREE;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.as;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.asList;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.internal.corext.dom.ASTBuilder;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.ASTSemanticMatcher;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.BooleanLiteral;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.NullLiteral;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.Statement;

/** See {@link #getDescription()} method. */
public class ObjectsEqualsRatherThanEqualsAndNullCheckCleanUp extends NewClassImportCleanUp {
    private final class RefactoringWithObjectsClass extends CleanUpWithNewClassImport {

        @Override
        public boolean visit(final IfStatement node) {
            final boolean isSubTreeToVisit =
                    ObjectsEqualsRatherThanEqualsAndNullCheckCleanUp.this.maybeRefactorIfStatement(node,
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
        return MultiFixMessages.CleanUpRefactoringWizard_ObjectsEqualsRatherThanEqualsAndNullCheckCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_ObjectsEqualsRatherThanEqualsAndNullCheckCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_ObjectsEqualsRatherThanEqualsAndNullCheckCleanUp_reason;
    }

    @Override
    public boolean isJavaVersionSupported(Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 7;
    }

    @Override
    public Set<String> getClassesToImport() {
        return new HashSet<String>(Arrays.asList("java.util.Objects"));
    }

    @Override
    public CleanUpWithNewClassImport getRefactoringClassInstance() {
        return new RefactoringWithObjectsClass();
    }

    @Override
    public boolean visit(final IfStatement node) {
        return maybeRefactorIfStatement(node, getAlreadyImportedClasses(node), new HashSet<String>());
    }

    private boolean maybeRefactorIfStatement(final IfStatement node,
            final Set<String> classesToUseWithImport,
            final Set<String> importsToAdd) {
        if (node.getElseStatement() != null) {
            final InfixExpression condition = as(node.getExpression(), InfixExpression.class);
            final List<Statement> thenStmts = asList(node.getThenStatement());
            final List<Statement> elseStmts = asList(node.getElseStatement());

            if (condition != null
                    && !condition.hasExtendedOperands()
                    && Arrays.asList(Operator.EQUALS, Operator.NOT_EQUALS).contains(condition.getOperator())
                    && thenStmts != null
                    && thenStmts.size() == 1
                    && elseStmts != null
                    && elseStmts.size() == 1) {
                final Expression operand1 = condition.getLeftOperand();
                final Expression operand2 = condition.getRightOperand();

                final Name field1 = as(operand1, Name.class);
                final NullLiteral nullLiteral1 = as(operand2, NullLiteral.class);
                final NullLiteral nullLiteral2 = as(operand1, NullLiteral.class);
                final Name field2 = as(operand2, Name.class);

                final Name firstField;

                if (field1 != null && nullLiteral1 != null) {
                    firstField = field1;
                } else if (field2 != null && nullLiteral2 != null) {
                    firstField = field2;
                } else {
                    firstField = null;
                }

                if (firstField != null) {
                    return maybeReplaceCode(node, condition, thenStmts, elseStmts, firstField, classesToUseWithImport,
                            importsToAdd);
                }
            }
        }

        return VISIT_SUBTREE;
    }

    private boolean maybeReplaceCode(final IfStatement node, final InfixExpression condition,
            final List<Statement> thenStmts,
            final List<Statement> elseStmts, final Name firstField,
            final Set<String> classesToUseWithImport,
            final Set<String> importsToAdd) {
        final IfStatement checkNullityStmt;
        final IfStatement checkEqualsStmt;

        if (Operator.EQUALS.equals(condition.getOperator())) {
            checkNullityStmt = as(thenStmts.get(0), IfStatement.class);
            checkEqualsStmt = as(elseStmts.get(0), IfStatement.class);
        } else {
            checkEqualsStmt = as(thenStmts.get(0), IfStatement.class);
            checkNullityStmt = as(elseStmts.get(0), IfStatement.class);
        }

        if (checkNullityStmt != null && checkNullityStmt.getElseStatement() == null
                && checkEqualsStmt != null && checkEqualsStmt.getElseStatement() == null) {
            final InfixExpression nullityCondition = as(checkNullityStmt.getExpression(),
                    InfixExpression.class);
            final List<Statement> nullityStmts = asList(checkNullityStmt.getThenStatement());

            final PrefixExpression equalsCondition = as(checkEqualsStmt.getExpression(),
                    PrefixExpression.class);
            final List<Statement> equalsStmts = asList(checkEqualsStmt.getThenStatement());

            if (nullityCondition != null
                    && !nullityCondition.hasExtendedOperands()
                    && Operator.NOT_EQUALS.equals(nullityCondition.getOperator())
                    && nullityStmts != null
                    && nullityStmts.size() == 1
                    && equalsCondition != null
                    && PrefixExpression.Operator.NOT.equals(equalsCondition.getOperator())
                    && equalsStmts != null
                    && equalsStmts.size() == 1) {
                return maybeReplaceEquals(node, firstField, nullityCondition, nullityStmts, equalsCondition,
                        equalsStmts, classesToUseWithImport,
                        importsToAdd);
            }
        }

        return VISIT_SUBTREE;
    }

    private boolean maybeReplaceEquals(final IfStatement node, final Name firstField,
            final InfixExpression nullityCondition, final List<Statement> nullityStmts,
            final PrefixExpression equalsCondition, final List<Statement> equalsStmts,
            final Set<String> classesToUseWithImport,
            final Set<String> importsToAdd) {
        final Expression nullityOperand1 = nullityCondition.getLeftOperand();
        final Expression nullityOperand2 = nullityCondition.getRightOperand();

        final Name nullityField1 = as(nullityOperand1, Name.class);
        final NullLiteral nullityLiteral1 = as(nullityOperand2, NullLiteral.class);
        final NullLiteral nullityLiteral2 = as(nullityOperand1, NullLiteral.class);
        final Name nullityField2 = as(nullityOperand2, Name.class);

        final Name secondField;

        if (nullityField1 != null && nullityLiteral1 != null) {
            secondField = nullityField1;
        } else if (nullityField2 != null && nullityLiteral2 != null) {
            secondField = nullityField2;
        } else {
            secondField = null;
        }

        final ReturnStatement returnStmt1 = as(nullityStmts.get(0), ReturnStatement.class);
        final ReturnStatement returnStmt2 = as(equalsStmts.get(0), ReturnStatement.class);
        final MethodInvocation equalsMethod = as(equalsCondition.getOperand(), MethodInvocation.class);

        if (secondField != null
                && returnStmt1 != null
                && returnStmt2 != null
                && equalsMethod != null
                && equalsMethod.getExpression() != null
                && "equals".equals(equalsMethod.getName().getIdentifier())
                && (equalsMethod.arguments() == null || equalsMethod.arguments().size() == 1)
                && (match(firstField, secondField, equalsMethod.getExpression(),
                        (ASTNode) equalsMethod.arguments().get(0))
                        || match(secondField, firstField, equalsMethod.getExpression(),
                                (ASTNode) equalsMethod.arguments().get(0)))) {
            final BooleanLiteral returnFalse1 = as(returnStmt1.getExpression(), BooleanLiteral.class);
            final BooleanLiteral returnFalse2 = as(returnStmt2.getExpression(), BooleanLiteral.class);

            if (returnFalse1 != null
                    && !returnFalse1.booleanValue()
                    && returnFalse2 != null
                    && !returnFalse2.booleanValue()) {
                replaceEquals(node, firstField, secondField, returnStmt1, classesToUseWithImport);
                importsToAdd.add("java.util.Objects");
                return DO_NOT_VISIT_SUBTREE;
            }
        }

        return VISIT_SUBTREE;
    }

    private boolean match(final Name firstField, final Name secondField, final Expression thisObject,
            final ASTNode otherObject) {
        final ASTSemanticMatcher matcher = new ASTSemanticMatcher();
        return ASTNodes.match(matcher, thisObject, firstField)
                                && ASTNodes.match(matcher, otherObject, secondField);
    }

    private void replaceEquals(final IfStatement node, final Name firstField, final Name secondField,
            final ReturnStatement returnStmt1, final Set<String> classesToUseWithImport) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        final Refactorings r = this.ctx.getRefactorings();

        r.replace(node, b.if0(b.not(b.invoke(classesToUseWithImport
                .contains("java.util.Objects") ? b.name("Objects") : b.name("java", "util", "Objects"),
                "equals",
                b.copy(firstField), b.copy(secondField))), b.block(b.copy(returnStmt1))));
    }
}

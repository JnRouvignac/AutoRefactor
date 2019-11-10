/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Fabrice Tiercelin - initial API and implementation
 * Copyright (C) 2016 Jean-Noël Rouvignac - various fixes
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
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.internal.corext.dom.ASTMatcherSameVariablesAndMethods;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.FinderVisitor;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.BreakStatement;
import org.eclipse.jdt.core.dom.CharacterLiteral;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SwitchCase;
import org.eclipse.jdt.core.dom.SwitchStatement;
import org.eclipse.jdt.core.dom.WhileStatement;

/** See {@link #getDescription()} method. */
public class SwitchCleanUp extends NoImportVisitCleanUp {
    static final class Variable {
        private final SimpleName name;
        private final List<Expression> constantValues;

        private Variable(SimpleName varName, List<Expression> constantValues) {
            this.name= varName;
            this.constantValues= constantValues;
        }

        private boolean isSameVariable(Variable other) {
            return other != null && ASTNodes.isSameVariable(name, other.name);
        }

        private Variable mergeValues(final Variable other) {
            final List<Expression> values= new ArrayList<>(constantValues);
            values.addAll(other.constantValues);
            return new Variable(name, values);
        }

        @Override
        public String toString() {
            return constantValues.size() == 1 ? name + " = " + constantValues.get(0) //$NON-NLS-1$
                    : name + " = one of " + constantValues; //$NON-NLS-1$
        }
    }

    /**
     * Represents a switch case section (cases + statements).
     * <p>
     * It can represent a switch case to build (when converting if else if
     * statements), or existing switch cases when representing the structure of a
     * whole switch.
     */
    private static final class SwitchCaseSection {
        /**
         * Must resolve to constant values. Used when representing switch cases to
         * build.
         */
        private final List<Expression> constantExprs;
        /** Used when representing the existing switch cases in a switch structure. */
        private final List<SwitchCase> existingCases;
        /** The statements executed for the switch cases. */
        private final List<Statement> statements;
        private final ASTMatcherSameVariablesAndMethods variablesAndMethodsMatcher= new ASTMatcherSameVariablesAndMethods();

        /** Used for switch structures, there is no constant expressions. */
        private SwitchCaseSection() {
            this(Collections.<Expression>emptyList(), new ArrayList<SwitchCase>(), new ArrayList<Statement>());
        }

        /** Used for if statements, only constant expressions are used. */
        private SwitchCaseSection(List<Expression> constantExprs, List<Statement> statements) {
            this(constantExprs, Collections.<SwitchCase>emptyList(), statements);
        }

        private SwitchCaseSection(List<Expression> constantExprs, List<SwitchCase> existingCases,
                List<Statement> statements) {
            this.constantExprs= constantExprs;
            this.existingCases= existingCases;
            this.statements= statements;
        }

        private boolean fallsThrough() {
            return statements.isEmpty() || !ASTNodes.fallsThrough(Utils.getLast(statements));
        }

        private boolean hasSameCode(SwitchCaseSection other) {
            if (statements.size() != other.statements.size()) {
                return false;
            }

            for (int i= 0; i < statements.size(); i++) {
                if (!ASTNodes.match(variablesAndMethodsMatcher, statements.get(i), other.statements.get(i))) {
                    return false;
                }
            }

            return true;
        }

        @Override
        public String toString() {
            final StringBuilder sb= new StringBuilder();
            for (Expression expression : constantExprs) {
                sb.append("new case ").append(expression).append(":\n"); //$NON-NLS-1$ //$NON-NLS-2$
            }
            for (SwitchCase existingCase : existingCases) {
                sb.append("existing case ").append(existingCase.getExpression()).append(":\n"); //$NON-NLS-1$ //$NON-NLS-2$
            }
            for (Statement statement : statements) {
                sb.append("    ").append(statement); //$NON-NLS-1$
            }
            sb.append("    break; // not needed if previous statement breaks control flow"); //$NON-NLS-1$
            return sb.toString();
        }
    }

    private static final class HasUnlabeledBreakVisitor extends FinderVisitor<Boolean> {
        @Override
        public boolean visit(BreakStatement node) {
            if (node.getLabel() == null) {
                setResult(true);
                return false;
            }

            return true;
        }

        @Override
        public boolean visit(DoStatement node) {
            return ignoreUnlabledBreaksInInnerBreakableStatement();
        }

        @Override
        public boolean visit(EnhancedForStatement node) {
            return ignoreUnlabledBreaksInInnerBreakableStatement();
        }

        @Override
        public boolean visit(ForStatement node) {
            return ignoreUnlabledBreaksInInnerBreakableStatement();
        }

        @Override
        public boolean visit(SwitchStatement node) {
            return ignoreUnlabledBreaksInInnerBreakableStatement();
        }

        @Override
        public boolean visit(WhileStatement node) {
            return ignoreUnlabledBreaksInInnerBreakableStatement();
        }

        private boolean ignoreUnlabledBreaksInInnerBreakableStatement() {
            // Unlabeled breaks in inner loops/switchs work ok with switch cleanup rule
            return false;
        }
    }

    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_SwitchCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_SwitchCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_SwitchCleanUp_reason;
    }

    @Override
    public boolean visit(final IfStatement node) {
        if (hasUnlabeledBreak(node)) {
            return true;
        }

        Variable variable= extractVariableAndValues(node);
        if (variable == null) {
            return true;
        }

        final SimpleName switchExpression= variable.name;
        final List<SwitchCaseSection> cases= new ArrayList<>();
        Statement remainingStatement= null;

        final Set<String> variableDeclarationIds= new HashSet<>();
        IfStatement currentNode= node;
        while (haveSameIdentifier(switchExpression, variable.name) && ASTNodes.haveSameType(switchExpression, variable.name)) {
            if (detectDeclarationConflicts(currentNode.getThenStatement(), variableDeclarationIds)) {
                // Cannot declare two variables with the same name in two cases
                return true;
            }

            cases.add(new SwitchCaseSection(variable.constantValues, ASTNodes.asList(currentNode.getThenStatement())));
            remainingStatement= currentNode.getElseStatement();

            variable= extractVariableAndValues(remainingStatement);
            if (variable == null) {
                break;
            }
            currentNode= (IfStatement) remainingStatement;
        }

        final List<SwitchCaseSection> filteredCases= filterDuplicateCaseValues(cases);
        return maybeReplaceWithSwitchStatement(node, switchExpression, filteredCases, remainingStatement);
    }

    private boolean hasUnlabeledBreak(final IfStatement node) {
        return new HasUnlabeledBreakVisitor().findOrDefault(node, false);
    }

    private boolean haveSameIdentifier(final SimpleName sn1, SimpleName sn2) {
        return sn1.getIdentifier().equals(sn2.getIdentifier());
    }

    private boolean detectDeclarationConflicts(final Statement statement, final Set<String> variableDeclarationIds) {
        final Set<String> varNames= ASTNodes.getLocalVariableIdentifiers(statement, false);
        final boolean hasConflict= containsAny(variableDeclarationIds, varNames);
        variableDeclarationIds.addAll(varNames);
        return hasConflict;
    }

    private boolean containsAny(final Set<String> variableDeclarationIds, Set<String> declIds2) {
        for (String newIdentifier : declIds2) {
            if (variableDeclarationIds.contains(newIdentifier)) {
                return true;
            }
        }

        return false;
    }

    private boolean maybeReplaceWithSwitchStatement(final IfStatement node, final Expression switchExpression,
            final List<SwitchCaseSection> cases, final Statement remainingStatement) {
        if (switchExpression != null && cases.size() > 1) {
            replaceWithSwitchStatement(node, switchExpression, cases, remainingStatement);
            return false;
        }

        return true;
    }

    /** Side-effect: removes the dead branches in a chain of if-elseif. */
    private List<SwitchCaseSection> filterDuplicateCaseValues(final List<SwitchCaseSection> sourceCases) {
        final List<SwitchCaseSection> results= new ArrayList<>();
        final Set<Object> alreadyProccessedValues= new HashSet<>();
        for (SwitchCaseSection sourceCase : sourceCases) {
            final List<Expression> filteredExprs= new ArrayList<>();
            for (Expression expression : sourceCase.constantExprs) {
                final Object constantValue= expression.resolveConstantExpressionValue();
                if (constantValue == null) {
                    throw new NotImplementedException(expression, "Cannot handle non constant expressions"); //$NON-NLS-1$
                }
                if (alreadyProccessedValues.add(constantValue)) {
                    // This is a new value (never seen before)
                    filteredExprs.add(expression);
                }
            }

            if (!filteredExprs.isEmpty()) {
                results.add(new SwitchCaseSection(filteredExprs, sourceCase.statements));
            }
        }

        return results;
    }

    private void replaceWithSwitchStatement(final IfStatement node, final Expression switchExpression,
            final List<SwitchCaseSection> cases, final Statement remainingStatement) {
        final ASTNodeFactory b= ctx.getASTBuilder();
        final SwitchStatement switchStatement= b.switch0(b.move(switchExpression));
        for (SwitchCaseSection aCase : cases) {
            addCaseWithStatements(switchStatement, aCase.constantExprs, aCase.statements);
        }
        if (remainingStatement != null) {
            addDefaultWithStatements(switchStatement, ASTNodes.asList(remainingStatement));
        }
        ctx.getRefactorings().replace(node, switchStatement);
    }

    private void addDefaultWithStatements(final SwitchStatement switchStatement, final List<Statement> remainingStatement) {
        addCaseWithStatements(switchStatement, null, remainingStatement);
    }

    private void addCaseWithStatements(final SwitchStatement switchStatement, final List<Expression> caseValues,
            final List<Statement> innerStatements) {
        final ASTNodeFactory b= ctx.getASTBuilder();
        final List<Statement> switchStatements= ASTNodes.statements(switchStatement);

        // Add the case statement(s)
        if (caseValues != null) {
            for (Expression caseValue : caseValues) {
                switchStatements.add(b.case0(b.move(caseValue)));
            }
        } else {
            switchStatements.add(b.default0());
        }

        // Add the statement(s) for this case(s)
        boolean isBreakNeeded= true;
        if (!innerStatements.isEmpty()) {
            for (Statement statement : innerStatements) {
                switchStatements.add(b.move(statement));
            }
            isBreakNeeded= !ASTNodes.fallsThrough(Utils.getLast(innerStatements));
        }
        // When required: end with a break;
        if (isBreakNeeded) {
            switchStatements.add(b.break0());
        }
    }

    private Variable extractVariableAndValues(final Statement statement) {
        if (statement instanceof IfStatement) {
            return extractVariableAndValues(((IfStatement) statement).getExpression());
        }

        return null;
    }

    private Variable extractVariableAndValues(Expression expression) {
        final Expression exprNoParen= ASTNodes.getUnparenthesedExpression(expression);
        return exprNoParen instanceof InfixExpression
                ? extractVariableAndValuesFromInfixExpression((InfixExpression) exprNoParen)
                : null;
    }

    private Variable extractVariableAndValuesFromInfixExpression(InfixExpression infixExpression) {
        final Expression leftOp= infixExpression.getLeftOperand();
        final Expression rightOp= infixExpression.getRightOperand();

        if (ASTNodes.extendedOperands(infixExpression).isEmpty() && ASTNodes.hasOperator(infixExpression, InfixExpression.Operator.CONDITIONAL_OR, InfixExpression.Operator.OR, InfixExpression.Operator.XOR)) {
            final Variable leftVar= extractVariableAndValues(leftOp);
            final Variable rightVar= extractVariableAndValues(rightOp);

            if (leftVar != null && leftVar.isSameVariable(rightVar)) {
                return leftVar.mergeValues(rightVar);
            }
        } else if (ASTNodes.hasOperator(infixExpression, InfixExpression.Operator.EQUALS)) {
            Variable variable= extractVariableWithConstantValue(leftOp, rightOp);
            return variable != null ? variable : extractVariableWithConstantValue(rightOp, leftOp);
        }

        return null;
    }

    private Variable extractVariableWithConstantValue(Expression firstOp, Expression secondOp) {
        // TODO JNR handle enums
        // TODO JNR handle strings
        if (firstOp instanceof SimpleName && ASTNodes.hasType(firstOp, char.class.getSimpleName(), byte.class.getSimpleName(), short.class.getSimpleName(), int.class.getSimpleName())
                && (secondOp instanceof NumberLiteral || secondOp instanceof CharacterLiteral)) {
            return new Variable((SimpleName) firstOp, Arrays.asList(secondOp));
        }

        return null;
    }

    @Override
    public boolean visit(final SwitchStatement node) {
        final List<SwitchCaseSection> switchStructure= getSwitchStructure(node);

        for (int referenceIndex= 0; referenceIndex < switchStructure.size() - 1; referenceIndex++) {
            final SwitchCaseSection referenceCase= switchStructure.get(referenceIndex);
            if (referenceCase.fallsThrough()) {
                continue;
            }

            for (int comparedIndex= referenceIndex + 1; comparedIndex < switchStructure.size(); comparedIndex++) {
                final SwitchCaseSection comparedCase= switchStructure.get(comparedIndex);
                if (referenceCase.hasSameCode(comparedCase)) {
                    if (!previousSectionFallsthrough(switchStructure, comparedIndex)) {
                        mergeCases(Merge.AFTER_SWITCH_CASES, referenceCase, comparedCase);
                        return false;
                    }
                    if (referenceIndex == 0 || !previousSectionFallsthrough(switchStructure, referenceIndex)) {
                        mergeCases(Merge.BEFORE_SWITCH_CASES, comparedCase, referenceCase);
                        return false;
                    }
                }
            }
        }

        return true;
    }

    private boolean previousSectionFallsthrough(final List<SwitchCaseSection> switchStructure, int idx) {
        return switchStructure.get(idx - 1).fallsThrough();
    }

    private List<SwitchCaseSection> getSwitchStructure(final SwitchStatement node) {
        final List<SwitchCaseSection> switchStructure= new ArrayList<>();

        SwitchCaseSection currentCase= new SwitchCaseSection();
        for (Statement statement : ASTNodes.statements(node)) {
            if (statement instanceof SwitchCase) {
                if (!currentCase.statements.isEmpty()) {
                    switchStructure.add(currentCase);
                    currentCase= new SwitchCaseSection();
                }

                final SwitchCase swithCase= (SwitchCase) statement;
                currentCase.existingCases.add(swithCase);
            } else {
                currentCase.statements.add(statement);
            }
        }

        if (!currentCase.existingCases.isEmpty()) {
            switchStructure.add(currentCase);
        }

        return switchStructure;
    }

    enum Merge {
        /** Insert before the first `case XX:`. */
        BEFORE_SWITCH_CASES,
        /** Insert after the last `case XX:`, i.e before the first statement. */
        AFTER_SWITCH_CASES
    }

    private void mergeCases(Merge merge, SwitchCaseSection sectionToKeep, SwitchCaseSection sectionToRemove) {
        final ASTNodeFactory b= this.ctx.getASTBuilder();
        final Refactorings r= this.ctx.getRefactorings();

        final Statement caseKept;
        if (merge == Merge.BEFORE_SWITCH_CASES) {
            caseKept= sectionToKeep.existingCases.get(0);
        } else { // move == Move.AFTER_SWITCH_CASES
            caseKept= sectionToKeep.statements.get(0);
        }

        for (SwitchCase caseToMove : sectionToRemove.existingCases) {
            r.insertBefore(b.move(caseToMove), caseKept);
        }
        r.remove(sectionToRemove.statements);
    }
}

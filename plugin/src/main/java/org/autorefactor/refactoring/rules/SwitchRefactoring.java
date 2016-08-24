/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Fabrice Tiercelin - initial API and implementation
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BreakStatement;
import org.eclipse.jdt.core.dom.CharacterLiteral;
import org.eclipse.jdt.core.dom.ContinueStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SwitchCase;
import org.eclipse.jdt.core.dom.SwitchStatement;
import org.eclipse.jdt.core.dom.ThrowStatement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

/** See {@link #getDescription()} method. */
public class SwitchRefactoring extends AbstractRefactoringRule {

    private final class VariableDeclarationIdentifierVisitor extends ASTVisitor {
        private Set<String> variableDeclarationIds = new HashSet<String>();

        public Set<String> getVariableDeclarationIds() {
            return variableDeclarationIds;
        }

        @Override
        public boolean visit(VariableDeclarationFragment node) {
            variableDeclarationIds.add(node.getName().getIdentifier());
            return VISIT_SUBTREE;
        }
    }

    @Override
    public String getDescription() {
        return "" + "Switch related refactorings:\n"
                + "- replaces if/else if/else blocks to use switch where possible.";
    }

    @Override
    public String getName() {
        return "Switch";
    }

    @Override
    public boolean visit(final IfStatement node) {
        Pair<SimpleName, List<Expression>> variableAndValues = getVariableAndValues(node);
        if (variableAndValues != null) {
            final SimpleName discriminant = variableAndValues.getFirst();
            final List<Pair<List<Expression>, Statement>> cases = new ArrayList<Pair<List<Expression>, Statement>>();
            Statement remainingStatements = null;

            final Set<String> variableDeclarationIds = new HashSet<String>();
            IfStatement currentNode = node;
            boolean hasMoreCases = true;
            while (hasMoreCases && discriminant.getIdentifier().equals(variableAndValues.getFirst().getIdentifier())
                    && discriminant.resolveTypeBinding().equals(variableAndValues.getFirst().resolveTypeBinding())) {

                // You can't declare two variables with the same name in two
                // cases
                if (detectDeclarationConflict(currentNode.getThenStatement(), variableDeclarationIds)) {
                    return VISIT_SUBTREE;
                }

                cases.add(Pair.<List<Expression>, Statement> of(variableAndValues.getSecond(),
                        currentNode.getThenStatement()));
                remainingStatements = currentNode.getElseStatement();

                variableAndValues = getVariableAndValues(remainingStatements);
                if (variableAndValues != null) {
                    currentNode = (IfStatement) remainingStatements;
                    hasMoreCases = true;
                } else {
                    hasMoreCases = false;
                }
            }

            final List<Pair<List<Expression>, Statement>> filteredCases = filterDuplicateCasesAndExpressions(cases);

            return maybeReplaceWithSwitchStatement(node, discriminant, filteredCases, remainingStatements);
        }
        return VISIT_SUBTREE;
    }

    private boolean detectDeclarationConflict(final Statement statement, final Set<String> variableDeclarationIds) {
        final VariableDeclarationIdentifierVisitor visitor = new VariableDeclarationIdentifierVisitor();

        statement.accept(visitor);

        boolean hasConflict = false;
        for (final String newIdentifier : visitor.getVariableDeclarationIds()) {
            if (variableDeclarationIds.contains(newIdentifier)) {
                hasConflict = true;
                break;
            }
        }

        variableDeclarationIds.addAll(visitor.getVariableDeclarationIds());

        return hasConflict;
    }

    private boolean maybeReplaceWithSwitchStatement(final IfStatement node, final Expression discriminant,
            final List<Pair<List<Expression>, Statement>> cases, final Statement remainingStatement) {
        if ((discriminant != null) && cases.size() > 1) {
            replaceWithSwitchStatement(node, discriminant, cases, remainingStatement);
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private List<Pair<List<Expression>, Statement>> filterDuplicateCasesAndExpressions(
            final List<Pair<List<Expression>, Statement>> sourceCases) {
        final Set<Expression> alreadyProccessedValues = new HashSet<Expression>();
        final List<Pair<List<Expression>, Statement>> filtrdCases = new ArrayList<Pair<List<Expression>, Statement>>();

        for (final Pair<List<Expression>, Statement> sourceCase : sourceCases) {
            final List<Expression> filteredExpressions = new ArrayList<Expression>();
            for (final Expression expression : sourceCase.getFirst()) {
                boolean isNew = true;
                for (final Expression alreadyProccessedValue : alreadyProccessedValues) {
                    if (expression.toString().equals(alreadyProccessedValue.toString())
                            && expression.resolveTypeBinding().equals(alreadyProccessedValue.resolveTypeBinding())) {
                        isNew = false;
                        break;
                    }
                }

                if (isNew) {
                    filteredExpressions.add(expression);
                    alreadyProccessedValues.add(expression);
                }
            }

            if (!filteredExpressions.isEmpty()) {
                filtrdCases.add(Pair.<List<Expression>, Statement> of(filteredExpressions, sourceCase.getSecond()));
            }
        }
        return filtrdCases;
    }

    private void replaceWithSwitchStatement(final IfStatement node, final Expression discriminant,
            final List<Pair<List<Expression>, Statement>> cases, final Statement remainingStatement) {
        final ASTBuilder b = this.ctx.getASTBuilder();

        // Switch
        final SwitchStatement switchStatement = b.switch0(b.copy(discriminant));

        // Cases
        for (final Pair<List<Expression>, Statement> oneCase : cases) {
            addStatementsToCase(b, switchStatement, oneCase.getFirst(), oneCase.getSecond());
        }

        // Default
        if (remainingStatement != null) {
            addStatementsToCase(b, switchStatement, null, remainingStatement);
        }
        this.ctx.getRefactorings().replace(node, switchStatement);
    }

    private void addStatementsToCase(final ASTBuilder b, final SwitchStatement switchStatement,
            final List<Expression> caseValues, final Statement innerStatement) {
        @SuppressWarnings("unchecked")
        final List<Statement> switchSubStatements = switchStatement.statements();

        // Case
        if (caseValues != null) {
            for (final Expression caseValue : caseValues) {
                SwitchCase switchCase = b.switchCase(b.copy(caseValue));
                switchSubStatements.add(switchCase);
            }
        } else {
            final SwitchCase switchCase = b.switchCase(null);
            switchSubStatements.add(switchCase);
        }

        // Process
        boolean isBreakNeeded = true;
        if (innerStatement instanceof Block) {
            final Block innerBlock = (Block) innerStatement;
            for (final Object statement : innerBlock.statements()) {
                switchSubStatements.add(b.copy((Statement) statement));
            }

            if (!innerBlock.statements().isEmpty()) {
                isBreakNeeded = !isGotoStatement(innerBlock.statements().get(innerBlock.statements().size() - 1));
            }
        } else {
            switchSubStatements.add(b.copy(innerStatement));
            isBreakNeeded = !isGotoStatement(innerStatement);
        }

        // Break
        if (isBreakNeeded) {
            switchSubStatements.add(b.break0());
        }
    }

    private boolean isGotoStatement(final Object lastStatement) {
        return lastStatement instanceof ReturnStatement || lastStatement instanceof BreakStatement
                || lastStatement instanceof ContinueStatement || lastStatement instanceof ThrowStatement;
    }

    private Pair<SimpleName, List<Expression>> getVariableAndValues(final Statement statement) {
        if ((statement != null) && statement instanceof IfStatement) {
            final IfStatement ifStatement = (IfStatement) statement;
            return getVariableAndValue(ifStatement.getExpression());
        }
        return null;
    }

    private Pair<SimpleName, List<Expression>> getVariableAndValue(final Expression expression) {
        if (expression instanceof ParenthesizedExpression) {
            return getVariableAndValue(((ParenthesizedExpression) expression).getExpression());
        } else if (expression instanceof InfixExpression) {
            return getVariableAndValueFromInfixExpression((InfixExpression) expression);
        }
        return null;
    }

    private Pair<SimpleName, List<Expression>> getVariableAndValueFromInfixExpression(
            final InfixExpression infixExpression) {
        if (InfixExpression.Operator.CONDITIONAL_OR.equals(infixExpression.getOperator())
                || InfixExpression.Operator.OR.equals(infixExpression.getOperator())
                || InfixExpression.Operator.XOR.equals(infixExpression.getOperator())) {
            final Expression firstOperand = infixExpression.getLeftOperand();
            final Expression secondOperand = infixExpression.getRightOperand();
            final Pair<SimpleName, List<Expression>> firstVariableAndValue = getVariableAndValue(firstOperand);
            final Pair<SimpleName, List<Expression>> secondVariableAndValue = getVariableAndValue(secondOperand);

            if ((firstVariableAndValue != null) && (firstVariableAndValue != null)
                    && firstVariableAndValue.getFirst().getIdentifier()
                            .equals(secondVariableAndValue.getFirst().getIdentifier())
                    && firstVariableAndValue.getFirst().resolveBinding()
                            .equals(secondVariableAndValue.getFirst().resolveBinding())) {
                final List<Expression> valueCompleteList = new ArrayList<Expression>(firstVariableAndValue.getSecond());
                valueCompleteList.addAll(secondVariableAndValue.getSecond());
                return Pair.<SimpleName, List<Expression>> of(firstVariableAndValue.getFirst(), valueCompleteList);
            }
        } else if (InfixExpression.Operator.EQUALS.equals(infixExpression.getOperator())) {
            final Expression firstOperand = infixExpression.getLeftOperand();
            final Expression secondOperand = infixExpression.getRightOperand();
            Pair<SimpleName, List<Expression>> variableAndValue = getVariableAndValue(firstOperand, secondOperand);
            if (variableAndValue == null) {
                variableAndValue = getVariableAndValue(secondOperand, firstOperand);
            }
            return variableAndValue;
        }
        return null;
    }

    private Pair<SimpleName, List<Expression>> getVariableAndValue(final Expression firstOperand,
            final Expression secondOperand) {
        if (firstOperand instanceof SimpleName && ((SimpleName) firstOperand).resolveTypeBinding().isPrimitive()
                && (secondOperand instanceof NumberLiteral || secondOperand instanceof CharacterLiteral)) {
            return Pair.<SimpleName, List<Expression>> of((SimpleName) firstOperand, Arrays.asList(secondOperand));
        }
        return null;
    }
}

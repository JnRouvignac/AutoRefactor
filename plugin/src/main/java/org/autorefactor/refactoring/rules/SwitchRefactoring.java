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
package org.autorefactor.refactoring.rules;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.ASTHelper;
import org.autorefactor.util.NotImplementedException;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.BreakStatement;
import org.eclipse.jdt.core.dom.CharacterLiteral;
import org.eclipse.jdt.core.dom.ContinueStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SwitchStatement;
import org.eclipse.jdt.core.dom.ThrowStatement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.*;

/** See {@link #getDescription()} method. */
public class SwitchRefactoring extends AbstractRefactoringRule {

    static final class Variable {
        private final SimpleName name;
        private final List<Expression> constantValues;

        private Variable(SimpleName varName, List<Expression> constantValues) {
            this.name = varName;
            this.constantValues = constantValues;
        }

        private boolean isSameVariable(Variable other) {
            return other != null && ASTHelper.isSameVariable(name, other.name);
        }

        private Variable mergeValues(final Variable other) {
            final List<Expression> values = new ArrayList<Expression>(constantValues);
            values.addAll(other.constantValues);
            return new Variable(name, values);
        }

        @Override
        public String toString() {
            return constantValues.size() == 1
                    ? name + " = " + constantValues.get(0)
                    : name + " = one of " + constantValues;
        }
    }

    private static final class RewrittenCase {
        /** Must resolve to constant values. */
        private List<Expression> expressions;
        private Statement stmt;

        public RewrittenCase(List<Expression> expressions, Statement stmt) {
            this.expressions = expressions;
            this.stmt = stmt;
        }

        @Override
        public String toString() {
            final StringBuilder sb = new StringBuilder();
            for (Expression expr : expressions) {
                sb.append("case ").append(expr).append(":\n");
            }
            for (Statement stmt : asList(stmt)) {
                sb.append("    " + stmt);
            }
            sb.append("    break; // not needed if previous statement breaks control flow");
            return sb.toString();
        }
    }

    private static final class VariableDeclarationIdentifierVisitor extends ASTVisitor {
        private Set<String> variableNames = new HashSet<String>();

        public Set<String> getVariableNames() {
            return variableNames;
        }

        @Override
        public boolean visit(VariableDeclarationFragment node) {
            variableNames.add(node.getName().getIdentifier());
            return VISIT_SUBTREE;
        }
    }

    @Override
    public String getDescription() {
        return ""
            + "Switch related refactorings:\n"
            + "- replaces if/else if/else blocks to use switch where possible.";
    }

    @Override
    public String getName() {
        return "Switch";
    }

    @Override
    public boolean visit(final IfStatement node) {
        Variable variable = extractVariableAndValues(node);
        if (variable == null) {
            return VISIT_SUBTREE;
        }

        final SimpleName switchExpr = variable.name;
        final List<RewrittenCase> cases = new ArrayList<RewrittenCase>();
        Statement remainingStmt = null;

        final Set<String> variableDeclarationIds = new HashSet<String>();
        IfStatement currentNode = node;
        while (havaSameIdentifier(switchExpr, variable.name)
                && haveSameType(switchExpr, variable.name)) {
            if (detectDeclarationConflicts(currentNode.getThenStatement(), variableDeclarationIds)) {
                // Cannot declare two variables with the same name in two cases
                return VISIT_SUBTREE;
            }

            cases.add(new RewrittenCase(variable.constantValues, currentNode.getThenStatement()));
            remainingStmt = currentNode.getElseStatement();

            variable = extractVariableAndValues(remainingStmt);
            if (variable == null) {
                break;
            }
            currentNode = (IfStatement) remainingStmt;
        }

        final List<RewrittenCase> filteredCases = filterDuplicateCaseValues(cases);
        return maybeReplaceWithSwitchStmt(node, switchExpr, filteredCases, remainingStmt);
    }

    private boolean havaSameIdentifier(final SimpleName sn1, SimpleName sn2) {
        return sn1.getIdentifier().equals(sn2.getIdentifier());
    }

    private boolean detectDeclarationConflicts(final Statement stmt, final Set<String> variableDeclarationIds) {
        final VariableDeclarationIdentifierVisitor visitor = new VariableDeclarationIdentifierVisitor();
        stmt.accept(visitor);

        final Set<String> varNames = visitor.getVariableNames();
        final boolean hasConflict = containsAny(variableDeclarationIds, varNames);
        variableDeclarationIds.addAll(varNames);
        return hasConflict;
    }

    private boolean containsAny(final Set<String> variableDeclarationIds, Set<String> declIds2) {
        for (final String newIdentifier : declIds2) {
            if (variableDeclarationIds.contains(newIdentifier)) {
                return true;
            }
        }
        return false;
    }

    private boolean maybeReplaceWithSwitchStmt(final IfStatement node, final Expression switchExpr,
            final List<RewrittenCase> cases, final Statement remainingStmt) {
        if (switchExpr != null && cases.size() > 1) {
            replaceWithSwitchStmt(node, switchExpr, cases, remainingStmt);
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    /** Side-effect: removes the dead branches in a chain of if-elseif. */
    private List<RewrittenCase> filterDuplicateCaseValues(final List<RewrittenCase> sourceCases) {
        final List<RewrittenCase> results = new ArrayList<RewrittenCase>();
        final Set<Object> alreadyProccessedValues = new HashSet<Object>();
        for (final RewrittenCase sourceCase : sourceCases) {
            final List<Expression> filteredExprs = new ArrayList<Expression>();
            for (final Expression expr : sourceCase.expressions) {
                final Object constantValue = expr.resolveConstantExpressionValue();
                if (constantValue == null) {
                    throw new NotImplementedException(expr, "Cannot handle non constant expressions");
                }
                if (alreadyProccessedValues.add(constantValue)) {
                    // this is a new value (never seen before)
                    filteredExprs.add(expr);
                }
            }

            if (!filteredExprs.isEmpty()) {
                results.add(new RewrittenCase(filteredExprs, sourceCase.stmt));
            }
        }
        return results;
    }

    private void replaceWithSwitchStmt(final IfStatement node, final Expression switchExpr,
            final List<RewrittenCase> cases, final Statement remainingStmt) {
        final ASTBuilder b = ctx.getASTBuilder();
        final SwitchStatement switchStmt = b.switch0(b.copy(switchExpr));
        for (final RewrittenCase aCase : cases) {
            addCaseWithStmts(switchStmt, aCase.expressions, aCase.stmt);
        }
        if (remainingStmt != null) {
            addDefaultWithStmts(switchStmt, remainingStmt);
        }
        ctx.getRefactorings().replace(node, switchStmt);
    }

    private void addDefaultWithStmts(final SwitchStatement switchStmt, final Statement remainingStmt) {
        addCaseWithStmts(switchStmt, null, remainingStmt);
    }

    private void addCaseWithStmts(final SwitchStatement switchStmt,
            final List<Expression> caseValues, final Statement innerStmt) {
        final ASTBuilder b = ctx.getASTBuilder();
        final List<Statement> switchStmts = statements(switchStmt);

        // Add the case statement(s)
        if (caseValues != null) {
            for (final Expression caseValue : caseValues) {
                switchStmts.add(b.case0(b.copy(caseValue)));
            }
        } else {
            switchStmts.add(b.default0());
        }

        // Add the statement(s) for this case(s)
        boolean isBreakNeeded = true;
        List<Statement> innerStmts = asList(innerStmt);
        if (!innerStmts.isEmpty()) {
            for (final Statement stmt : innerStmts) {
                switchStmts.add(b.copy(stmt));
            }
            final Statement lastStmt = innerStmts.get(innerStmts.size() - 1);
            isBreakNeeded = !breaksControlFlow(lastStmt);
        }
        //  when required: end with a break;
        if (isBreakNeeded) {
            switchStmts.add(b.break0());
        }
    }

    private boolean breaksControlFlow(final Statement stmt) {
        return stmt instanceof ReturnStatement
                || stmt instanceof BreakStatement
                || stmt instanceof ContinueStatement
                || stmt instanceof ThrowStatement;
    }

    private Variable extractVariableAndValues(final Statement stmt) {
        if (stmt instanceof IfStatement) {
            return extractVariableAndValues(((IfStatement) stmt).getExpression());
        }
        return null;
    }

    private Variable extractVariableAndValues(Expression expr) {
        final Expression exprNoParen = removeParentheses(expr);
        return exprNoParen instanceof InfixExpression
                ? extractVariableAndValuesFromInfixExpression((InfixExpression) exprNoParen)
                : null;
    }

    private Variable extractVariableAndValuesFromInfixExpression(InfixExpression infixExpr) {
        final Operator op = infixExpr.getOperator();
        final Expression leftOp = infixExpr.getLeftOperand();
        final Expression rightOp = infixExpr.getRightOperand();
        if (extendedOperands(infixExpr).isEmpty()
                && (CONDITIONAL_OR.equals(op) || OR.equals(op) || XOR.equals(op))) {
            final Variable leftVar = extractVariableAndValues(leftOp);
            final Variable rightVar = extractVariableAndValues(rightOp);

            if (leftVar != null && leftVar.isSameVariable(rightVar)) {
                return leftVar.mergeValues(rightVar);
            }
        } else if (EQUALS.equals(op)) {
            Variable variable = extractVariableWithConstantValue(leftOp, rightOp);
            return variable != null ? variable : extractVariableWithConstantValue(rightOp, leftOp);
        }
        return null;
    }

    private Variable extractVariableWithConstantValue(Expression firstOp, Expression secondOp) {
        // TODO JNR handle enums
        // TODO JNR handle strings
        if (firstOp instanceof SimpleName && isPrimitive(firstOp)
                && (secondOp instanceof NumberLiteral || secondOp instanceof CharacterLiteral)) {
            return new Variable((SimpleName) firstOp, Arrays.asList(secondOp));
        }
        return null;
    }
}

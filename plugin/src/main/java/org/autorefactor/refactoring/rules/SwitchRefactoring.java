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
import org.autorefactor.refactoring.FinderVisitor;
import org.autorefactor.util.NotImplementedException;
import org.eclipse.jdt.core.dom.ASTMatcher;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BreakStatement;
import org.eclipse.jdt.core.dom.CharacterLiteral;
import org.eclipse.jdt.core.dom.ContinueStatement;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SwitchCase;
import org.eclipse.jdt.core.dom.SwitchStatement;
import org.eclipse.jdt.core.dom.ThrowStatement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.WhileStatement;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.autorefactor.util.Utils.*;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.*;

/** See {@link #getDescription()} method. */
public class SwitchRefactoring extends AbstractRefactoringRule {

    /** ASTMatcher that matches two piece of code only if the variables in use are the same. */
    private static final class ASTMatcherSameVariables extends ASTMatcher {
        @Override
        public boolean match(SimpleName node, Object other) {
            return super.match(node, other)
                    && sameVariable(node, (SimpleName) other);
        }

        private boolean sameVariable(SimpleName node1, SimpleName node2) {
            return equalNotNull(getDeclaration(node1), getDeclaration(node2));
        }

        private IBinding getDeclaration(SimpleName node) {
            final IBinding b = node.resolveBinding();
            if (b != null) {
                switch (b.getKind()) {
                case IBinding.VARIABLE:
                    return ((IVariableBinding) b).getVariableDeclaration();
                case IBinding.METHOD:
                    return ((IMethodBinding) b).getMethodDeclaration();
                }
            }
            return null;
        }
    }

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

    private static final class SwitchCaseSection {
        /** Must resolve to constant values. */
        private List<Expression> expressions;
        private List<SwitchCase> existingCases;
        private List<Statement> stmts;

        public SwitchCaseSection() {
            this(new ArrayList<Expression>(), new ArrayList<SwitchCase>(),
                    new ArrayList<Statement>());
        }

        public SwitchCaseSection(List<Expression> expressionList, List<SwitchCase> caseList,
                List<Statement> statementList) {
            this.expressions = expressionList;
            this.stmts = statementList;
            this.existingCases = caseList;
        }

        @Override
        public String toString() {
            final StringBuilder sb = new StringBuilder();
            for (Expression anExpression : expressions) {
                sb.append("new case ").append(anExpression).append(":\n");
            }
            for (SwitchCase existingCase : existingCases) {
                sb.append("existing case ").append(existingCase.getExpression()).append(":\n");
            }
            for (Statement stmt : stmts) {
                sb.append("    ").append(stmt);
            }
            sb.append("    break; // not needed if previous statement breaks control flow");
            return sb.toString();
        }
    }

    private static final class VariableDeclarationIdentifierVisitor extends ASTVisitor {
        private Set<String> variableNames = new HashSet<String>();
        private Statement startNode;

        public Set<String> getVariableNames() {
            return variableNames;
        }

        public VariableDeclarationIdentifierVisitor(Statement node) {
            startNode = node;
        }

        @Override
        public boolean visit(VariableDeclarationFragment node) {
            variableNames.add(node.getName().getIdentifier());
            return VISIT_SUBTREE;
        }

        @Override
        public boolean visit(Block node) {
            return startNode == node ? VISIT_SUBTREE : DO_NOT_VISIT_SUBTREE;
        }
    }

    private static final class HasUnlabeledBreakVisitor extends FinderVisitor<Boolean> {
        @Override
        public boolean visit(BreakStatement node) {
            if (node.getLabel() == null) {
                setResult(true);
                return DO_NOT_VISIT_SUBTREE;
            }
            return VISIT_SUBTREE;
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
            // unlabeled breaks in inner loops/switchs work ok with switch refactoring rule
            return DO_NOT_VISIT_SUBTREE;
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
        if (hasUnlabeledBreak(node)) {
            return VISIT_SUBTREE;
        }

        Variable variable = extractVariableAndValues(node);
        if (variable == null) {
            return VISIT_SUBTREE;
        }

        final SimpleName switchExpr = variable.name;
        final List<SwitchCaseSection> cases = new ArrayList<SwitchCaseSection>();
        Statement remainingStmt = null;

        final Set<String> variableDeclarationIds = new HashSet<String>();
        IfStatement currentNode = node;
        while (havaSameIdentifier(switchExpr, variable.name)
                && haveSameType(switchExpr, variable.name)) {
            if (detectDeclarationConflicts(currentNode.getThenStatement(), variableDeclarationIds)) {
                // Cannot declare two variables with the same name in two cases
                return VISIT_SUBTREE;
            }

            cases.add(new SwitchCaseSection(variable.constantValues, new ArrayList<SwitchCase>(),
                    asList(currentNode.getThenStatement())));
            remainingStmt = currentNode.getElseStatement();

            variable = extractVariableAndValues(remainingStmt);
            if (variable == null) {
                break;
            }
            currentNode = (IfStatement) remainingStmt;
        }

        final List<SwitchCaseSection> filteredCases = filterDuplicateCaseValues(cases);
        return maybeReplaceWithSwitchStmt(node, switchExpr, filteredCases, remainingStmt);
    }

    private boolean hasUnlabeledBreak(final IfStatement node) {
        return new HasUnlabeledBreakVisitor().findOrDefault(node, false);
    }

    private boolean havaSameIdentifier(final SimpleName sn1, SimpleName sn2) {
        return sn1.getIdentifier().equals(sn2.getIdentifier());
    }

    private boolean detectDeclarationConflicts(final Statement stmt, final Set<String> variableDeclarationIds) {
        final VariableDeclarationIdentifierVisitor visitor = new VariableDeclarationIdentifierVisitor(stmt);
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
            final List<SwitchCaseSection> cases, final Statement remainingStmt) {
        if (switchExpr != null && cases.size() > 1) {
            replaceWithSwitchStmt(node, switchExpr, cases, remainingStmt);
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    /** Side-effect: removes the dead branches in a chain of if-elseif. */
    private List<SwitchCaseSection> filterDuplicateCaseValues(final List<SwitchCaseSection> sourceCases) {
        final List<SwitchCaseSection> results = new ArrayList<SwitchCaseSection>();
        final Set<Object> alreadyProccessedValues = new HashSet<Object>();
        for (final SwitchCaseSection sourceCase : sourceCases) {
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
                results.add(new SwitchCaseSection(filteredExprs, new ArrayList<SwitchCase>(), sourceCase.stmts));
            }
        }
        return results;
    }

    private void replaceWithSwitchStmt(final IfStatement node, final Expression switchExpr,
            final List<SwitchCaseSection> cases, final Statement remainingStmt) {
        final ASTBuilder b = ctx.getASTBuilder();
        final SwitchStatement switchStmt = b.switch0(b.copy(switchExpr));
        for (final SwitchCaseSection aCase : cases) {
            addCaseWithStmts(switchStmt, aCase.expressions, aCase.stmts);
        }
        if (remainingStmt != null) {
            addDefaultWithStmts(switchStmt, asList(remainingStmt));
        }
        ctx.getRefactorings().replace(node, switchStmt);
    }

    private void addDefaultWithStmts(final SwitchStatement switchStmt, final List<Statement> remainingStmt) {
        addCaseWithStmts(switchStmt, null, remainingStmt);
    }

    private void addCaseWithStmts(final SwitchStatement switchStmt,
            final List<Expression> caseValues, final List<Statement> innerStmts) {
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

    @Override
    public boolean visit(final SwitchStatement node) {
        final List<SwitchCaseSection> switchStructure = getSwitchStructure(node);

        for (int referenceIndex = 0; referenceIndex < switchStructure.size() - 1; referenceIndex++) {
            for (int comparedIndex = referenceIndex + 1; comparedIndex < switchStructure.size(); comparedIndex++) {
                final SwitchCaseSection referenceCase = switchStructure.get(referenceIndex);
                final SwitchCaseSection comparedCase = switchStructure.get(comparedIndex);

                if (!referenceCase.stmts.isEmpty()
                        && breaksControlFlow(referenceCase.stmts.get(referenceCase.stmts.size() - 1))
                        && isSameCode(referenceCase.stmts, comparedCase.stmts)) {
                    List<Statement> precedingStatements = switchStructure.get(comparedIndex - 1).stmts;

                    if (breaksControlFlow(precedingStatements.get(precedingStatements.size() - 1))) {
                        mergeCases(false, referenceCase, comparedCase);
                        return DO_NOT_VISIT_SUBTREE;
                    } else if (referenceIndex == 0) {
                        mergeCases(true, comparedCase, referenceCase);
                        return DO_NOT_VISIT_SUBTREE;
                    } else {
                        precedingStatements = switchStructure.get(referenceIndex - 1).stmts;

                        if (breaksControlFlow(precedingStatements.get(precedingStatements.size() - 1))) {
                            mergeCases(true, comparedCase, referenceCase);
                            return DO_NOT_VISIT_SUBTREE;
                        }
                    }
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private List<SwitchCaseSection> getSwitchStructure(final SwitchStatement node) {
        final List<SwitchCaseSection> switchStructure = new ArrayList<SwitchCaseSection>();

        SwitchCaseSection currentCase = new SwitchCaseSection();
        for (final Object oneStatement : node.statements()) {
            if (oneStatement instanceof SwitchCase) {
                final SwitchCase oneSwitchCase = (SwitchCase) oneStatement;
                if (!currentCase.stmts.isEmpty()) {
                    switchStructure.add(currentCase);
                    currentCase = new SwitchCaseSection();
                }
                currentCase.existingCases.add(oneSwitchCase);
            } else {
                currentCase.stmts.add((Statement) oneStatement);
            }
        }

        if (!currentCase.existingCases.isEmpty()) {
            switchStructure.add(currentCase);
        }

        return switchStructure;
    }

    private boolean isSameCode(final List<Statement> referenceStatements, final List<Statement> comparedStatements) {
        if (referenceStatements.size() == comparedStatements.size()) {
            final ASTMatcher matcher = new ASTMatcherSameVariables();

            for (int codeLine = 0; codeLine < referenceStatements.size(); codeLine++) {
                if (!ASTHelper.match(matcher, referenceStatements.get(codeLine),
                        comparedStatements.get(codeLine))) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    private void mergeCases(final boolean before, final SwitchCaseSection referenceSection,
            final SwitchCaseSection sectionToMove) {
        final ASTBuilder b = this.ctx.getASTBuilder();

        final Statement referenceCase;
        if (before) {
            referenceCase = referenceSection.existingCases.get(0);
        } else {
            referenceCase = referenceSection.stmts.get(0);
        }
        for (final SwitchCase caseToMove : sectionToMove.existingCases) {
            this.ctx.getRefactorings().insertBefore(b.move(caseToMove), referenceCase);
        }

        for (final Statement codeToMove : sectionToMove.stmts) {
            this.ctx.getRefactorings().remove(codeToMove);
        }
    }
}

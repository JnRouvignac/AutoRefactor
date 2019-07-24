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
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.fallsThrough;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.getLocalVariableIdentifiers;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.hasType;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.isPassive;
import static org.autorefactor.util.Utils.newArrayList;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.internal.corext.dom.ASTBuilder;
import org.autorefactor.jdt.internal.corext.dom.InterruptibleVisitor;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BreakStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SwitchCase;
import org.eclipse.jdt.core.dom.SwitchStatement;

/** See {@link #getDescription()} method. */
public class IfRatherThanTwoSwitchCasesCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_IfRatherThanTwoSwitchCasesCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_IfRatherThanTwoSwitchCasesCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_IfRatherThanTwoSwitchCasesCleanUp_reason;
    }

    private class VarOccurrenceVisitor extends InterruptibleVisitor {
        private final Set<String> localVarIds;
        private boolean varUsed;

        public boolean isVarUsed() {
            return varUsed;
        }

        public VarOccurrenceVisitor(final Set<String> localVarIds) {
            this.localVarIds= localVarIds;
        }

        @Override
        public boolean visit(final SimpleName aVariable) {
            if (localVarIds.contains(aVariable.getIdentifier())) {
                varUsed= true;
                return interruptVisit();
            }
            return VISIT_SUBTREE;
        }

        @Override
        public boolean visit(final Block node) {
            return DO_NOT_VISIT_SUBTREE;
        }
    }

    @Override
    public boolean visit(final SwitchStatement node) {
        if (!isPassive(node.getExpression())) {
            return VISIT_SUBTREE;
        }

        final List<?> stmts= node.statements();

        if (stmts.isEmpty()) {
            return VISIT_SUBTREE;
        }

        final Set<String> previousVarIds= new HashSet<String>();
        final Set<String> caseVarIds= new HashSet<String>();
        final List<Pair<List<Expression>, List<Statement>>> switchStructure= newArrayList();
        List<Expression> caseExprs= newArrayList();
        List<Statement> caseStmts= newArrayList();

        boolean isPreviousStmtACase= true;
        int caseNb= 0;
        int caseIndexWithDefault= -1;
        final ASTBuilder b= this.ctx.getASTBuilder();

        for (Object object : stmts) {
            Statement stmt= (Statement) object;

            if (stmt instanceof SwitchCase) {
                if (!isPreviousStmtACase) {
                    caseNb++;

                    if (caseNb > 2) {
                        return VISIT_SUBTREE;
                    }

                    previousVarIds.addAll(caseVarIds);
                    caseVarIds.clear();

                    switchStructure.add(Pair.<List<Expression>, List<Statement>>of(caseExprs, caseStmts));
                    caseExprs= newArrayList();
                    caseStmts= newArrayList();
                }

                if (((SwitchCase) stmt).isDefault()) {
                    caseIndexWithDefault= caseNb;
                } else {
                    caseExprs.add(((SwitchCase) stmt).getExpression());
                }

                isPreviousStmtACase= true;
            } else {
                final VarOccurrenceVisitor varOccurrenceVisitor= new VarOccurrenceVisitor(previousVarIds);
                varOccurrenceVisitor.visitNode(stmt);

                if (varOccurrenceVisitor.isVarUsed()) {
                    return VISIT_SUBTREE;
                }

                caseVarIds.addAll(getLocalVariableIdentifiers(stmt, false));
                caseStmts.add(stmt);

                isPreviousStmtACase= false;
            }
        }

        switchStructure.add(Pair.<List<Expression>, List<Statement>>of(caseExprs, caseStmts));
        caseNb++;

        if (caseNb > 2) {
            return VISIT_SUBTREE;
        }

        if (caseIndexWithDefault != -1) {
            Pair<List<Expression>, List<Statement>> caseWithDefault= switchStructure.remove(caseIndexWithDefault);
            switchStructure.add(caseWithDefault);
        }

        for (final Pair<List<Expression>, List<Statement>> caseStructure : switchStructure) {
            final Statement lastStmt= caseStructure.getSecond().get(caseStructure.getSecond().size() - 1);

            if (!fallsThrough(lastStmt)) {
                return VISIT_SUBTREE;
            }

            final BreakStatement bs= as(lastStmt, BreakStatement.class);

            if (bs != null && bs.getLabel() == null) {
                caseStructure.getSecond().remove(caseStructure.getSecond().size() - 1);
            }
        }

        replaceSwitch(node, switchStructure, caseIndexWithDefault, b);

        return DO_NOT_VISIT_SUBTREE;
    }

    private void replaceSwitch(final SwitchStatement node,
            final List<Pair<List<Expression>, List<Statement>>> switchStructure, final int caseIndexWithDefault,
            final ASTBuilder b) {
        int localCaseIndexWithDefault= caseIndexWithDefault;
        final Refactorings r= this.ctx.getRefactorings();

        final Expression discriminant= node.getExpression();
        Statement currentBlock= null;

        for (int i= switchStructure.size() - 1; i >= 0; i--) {
            final Pair<List<Expression>, List<Statement>> caseStructure= switchStructure.get(i);

            final Expression newCondition;
            if (caseStructure.getFirst().isEmpty()) {
                newCondition= null;
            } else if (caseStructure.getFirst().size() == 1) {
                newCondition= buildEquality(b, discriminant, caseStructure.getFirst().get(0));
            } else {
                final List<Expression> equalities= newArrayList();

                for (Expression value : caseStructure.getFirst()) {
                    equalities.add(b.parenthesizeIfNeeded(buildEquality(b, discriminant, value)));
                }
                newCondition= b.infixExpr(InfixExpression.Operator.CONDITIONAL_OR, equalities);
            }

            final Statement[] copyOfStmts= new Statement[caseStructure.getSecond().size()];

            for (int j= 0; j < caseStructure.getSecond().size(); j++) {
                copyOfStmts[j]= b.copy(caseStructure.getSecond().get(j));
            }

            final Block newBlock= b.block(copyOfStmts);

            if (currentBlock != null) {
                currentBlock= b.if0(newCondition, newBlock, currentBlock);
            } else if (copyOfStmts.length == 0) {
                localCaseIndexWithDefault= -1;
            } else if (localCaseIndexWithDefault == -1) {
                currentBlock= b.if0(newCondition, newBlock);
            } else {
                currentBlock= newBlock;
            }
        }

        r.replace(node, currentBlock);
    }

    private Expression buildEquality(final ASTBuilder b, final Expression discriminant, final Expression value) {
        final Expression equality;

        if (hasType(value, "java.lang.String", "java.lang.Boolean", "java.lang.Byte", "java.lang.Character",
                "java.lang.Double", "java.lang.Float", "java.lang.Integer", "java.lang.Long", "java.lang.Short")) {
            equality= b.invoke(b.copy(value), "equals", b.copy(discriminant));
        } else if (value.resolveTypeBinding() != null && value.resolveTypeBinding().isEnum()) {
            equality= b.infixExpr(b.copy(discriminant), InfixExpression.Operator.EQUALS, b.getAST().newQualifiedName(
                    b.name(value.resolveTypeBinding().getQualifiedName().split("\\.")), b.copy((SimpleName) value)));
        } else {
            equality= b.infixExpr(b.parenthesizeIfNeeded(b.copy(discriminant)), InfixExpression.Operator.EQUALS,
                    b.copy(value));
        }

        return equality;
    }
}

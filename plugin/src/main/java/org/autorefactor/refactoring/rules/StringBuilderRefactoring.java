/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2017 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Make sure we do not visit again modified nodes
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
import static org.autorefactor.refactoring.ASTHelper.arg0;
import static org.autorefactor.refactoring.ASTHelper.arguments;
import static org.autorefactor.refactoring.ASTHelper.as;
import static org.autorefactor.refactoring.ASTHelper.extendedOperands;
import static org.autorefactor.refactoring.ASTHelper.getBoxedTypeBinding;
import static org.autorefactor.refactoring.ASTHelper.hasOperator;
import static org.autorefactor.refactoring.ASTHelper.hasType;
import static org.autorefactor.refactoring.ASTHelper.instanceOf;
import static org.autorefactor.refactoring.ASTHelper.isMethod;
import static org.autorefactor.refactoring.ASTHelper.removeParentheses;
import static org.eclipse.jdt.core.dom.ASTNode.FIELD_ACCESS;
import static org.eclipse.jdt.core.dom.ASTNode.QUALIFIED_NAME;
import static org.eclipse.jdt.core.dom.ASTNode.SIMPLE_NAME;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.PLUS;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.concurrent.atomic.AtomicBoolean;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.StringLiteral;

/** See {@link #getDescription()} method. */
public class StringBuilderRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "StringBuilder";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Refactors to a proper use of StringBuilders:\n"
            + "- replace String concatenations using operator '+' as parameters"
            + " of StringBuffer/StringBuilder.append(),\n"
            + "- replace chained call to StringBuffer/StringBuilder constructor followed by calls to append()"
            + " and call toString() with straight String concatenation using operator '+'.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves the time and space performance. "
                + "It also improves the readibility. "
                + "String concatenation is automatically converted as StringBuilder by the compiler so it is useless.";
    }

    private boolean isEmptyString(final Expression expr) {
        return "".equals(expr.resolveConstantExpressionValue())
                // Due to a bug with ASTNode.resolveConstantExpressionValue()
                // in Eclipse 3.7.2 and 3.8.0, this second check is necessary
                || (expr instanceof StringLiteral
                        && "".equals(((StringLiteral) expr).getLiteralValue()));
    }

    @Override
    public boolean visit(MethodInvocation node) {
        if (node.getExpression() != null
                && "append".equals(node.getName().getIdentifier())
                && arguments(node).size() == 1
                // Most expensive check comes last
                && isStringBuilderOrBuffer(node.getExpression())) {
            final MethodInvocation embeddedMI = as(arg0(node), MethodInvocation.class);

            if (isMethod(embeddedMI, "java.lang.String", "substring", "int", "int")
                    || isMethod(embeddedMI, "java.lang.CharSequence", "subSequence", "int", "int")) {
                replaceWithAppendSubstring(node, embeddedMI);
                return DO_NOT_VISIT_SUBTREE;
            }

            return maybeRefactorAppending(node);
        } else if (isMethod(node, "java.lang.StringBuilder", "toString")
                || isMethod(node, "java.lang.StringBuffer", "toString")) {
            final LinkedList<Pair<ITypeBinding, Expression>> allAppendedStrings =
                    new LinkedList<Pair<ITypeBinding, Expression>>();
            final Expression lastExpr = readAppendMethod(node.getExpression(), allAppendedStrings,
                    new AtomicBoolean(false), new AtomicBoolean(false));
            if (lastExpr instanceof ClassInstanceCreation) {
                // Replace with String concatenation
                this.ctx.getRefactorings().replace(node,
                        createStringConcats(allAppendedStrings));
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    @Override
    public boolean visit(ClassInstanceCreation node) {
        if ((hasType(node, "java.lang.StringBuilder")
                || hasType(node, "java.lang.StringBuffer"))
                && arguments(node).size() == 1) {
            final Expression arg0 = arguments(node).get(0);

            if (hasType(arg0, "java.lang.String")
                    && (arg0 instanceof InfixExpression
                            || (arg0 instanceof MethodInvocation
                                    && (isToString((MethodInvocation) arg0)
                                            || isStringValueOf((MethodInvocation) arg0))))) {
                return maybeRefactorAppending(node);
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean maybeRefactorAppending(Expression node) {
        final LinkedList<Pair<ITypeBinding, Expression>> allAppendedStrings =
                new LinkedList<Pair<ITypeBinding, Expression>>();
        final AtomicBoolean isRefactoringNeeded = new AtomicBoolean(false);
        final AtomicBoolean isInstanceCreationToRewrite = new AtomicBoolean(false);
        final Expression lastExpr = readAppendMethod(node, allAppendedStrings, isRefactoringNeeded,
                isInstanceCreationToRewrite);

        if (lastExpr != null) {
            removeEmptyStrings(allAppendedStrings, isRefactoringNeeded);
            removeCallsToToString(allAppendedStrings, isRefactoringNeeded, isInstanceCreationToRewrite.get());

            if (isRefactoringNeeded.get()) {
                if (allAppendedStrings.isEmpty()
                        && isVariable(lastExpr)
                        && node.getParent() instanceof Statement) {
                    ctx.getRefactorings().remove(node.getParent());
                } else {
                    replaceWithNewStringAppends(node, allAppendedStrings, lastExpr, isInstanceCreationToRewrite.get());
                }
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    private Expression readAppendMethod(final Expression expr,
            final LinkedList<Pair<ITypeBinding, Expression>> allOperands, final AtomicBoolean isRefactoringNeeded,
            final AtomicBoolean isInstanceCreationToRewrite) {
        final Expression exp = removeParentheses(expr);
        if (isStringBuilderOrBuffer(exp)) {
            if (exp instanceof MethodInvocation) {
                final MethodInvocation mi = (MethodInvocation) exp;
                if ("append".equals(mi.getName().getIdentifier())
                        && arguments(mi).size() == 1) {
                    final Expression arg0 = arguments(mi).get(0);
                    readSubExpressions(arg0, allOperands, isRefactoringNeeded);
                    return readAppendMethod(mi.getExpression(), allOperands, isRefactoringNeeded,
                            isInstanceCreationToRewrite);
                }
            } else if (exp instanceof ClassInstanceCreation) {
                final ClassInstanceCreation cic = (ClassInstanceCreation) exp;
                if (arguments(cic).size() == 1) {
                    final Expression arg0 = arguments(cic).get(0);
                    if (isStringBuilderOrBuffer(cic)
                        && (hasType(arg0, "java.lang.String")
                                || instanceOf(arg0, "java.lang.CharSequence"))) {
                        isInstanceCreationToRewrite.set(true);
                        readSubExpressions(arg0, allOperands, isRefactoringNeeded);
                    }
                } else if (arguments(cic).isEmpty()
                        && !allOperands.isEmpty()
                        && ((allOperands.getFirst().getFirst() != null)
                        ? hasType(allOperands.getFirst().getFirst(), "java.lang.String")
                                : hasType(allOperands.getFirst().getSecond(), "java.lang.String"))) {
                    isInstanceCreationToRewrite.set(true);
                    isRefactoringNeeded.set(true);
                }
                return cic;
            } else {
                return expr;
            }
        }
        return null;
    }

    private void readSubExpressions(final Expression arg, final LinkedList<Pair<ITypeBinding, Expression>> results,
            final AtomicBoolean isRefactoringNeeded) {
        if (arg instanceof InfixExpression) {
            final InfixExpression ie = (InfixExpression) arg;
            if (isStringConcat(ie)) {
                if (ie.hasExtendedOperands()) {
                    final List<Expression> reversed = new ArrayList<Expression>(extendedOperands(ie));
                    Collections.reverse(reversed);

                    if (isValuedStringLiteralOrConstant(reversed.get(0)) && !results.isEmpty()
                            && isValuedStringLiteralOrConstant(results.get(0).getSecond())) {
                        isRefactoringNeeded.set(true);
                    }
                    for (final Expression op : reversed) {
                        if (!isValuedStringLiteralOrConstant(reversed.get(0))) {
                            isRefactoringNeeded.set(true);
                        }
                        readSubExpressions(op, results, new AtomicBoolean(false));
                    }
                }
                if (!isValuedStringLiteralOrConstant(ie.getRightOperand())
                        || !isValuedStringLiteralOrConstant(ie.getLeftOperand())) {
                    isRefactoringNeeded.set(true);
                }
                readSubExpressions(ie.getRightOperand(), results, new AtomicBoolean(false));
                readSubExpressions(ie.getLeftOperand(), results, new AtomicBoolean(false));
                return;
            }
        }
        if (isValuedStringLiteralOrConstant(arg) && !results.isEmpty()
                && isValuedStringLiteralOrConstant(results.get(0).getSecond())) {
            isRefactoringNeeded.set(true);
        }
        results.addFirst(Pair.<ITypeBinding, Expression>of(null, arg));
    }

    private boolean isStringConcat(final InfixExpression node) {
        if (!hasOperator(node, PLUS) || !hasType(node, "java.lang.String")) {
            return false;
        }
        if (!isValuedStringLiteralOrConstant(node.getLeftOperand())
                || !isValuedStringLiteralOrConstant(node.getRightOperand())) {
            return true;
        }
        for (Expression expr : extendedOperands(node)) {
            if (!isValuedStringLiteralOrConstant(expr)) {
                return true;
            }
        }
        return false;
    }

    private boolean isValuedStringLiteralOrConstant(Expression expr) {
        if (expr instanceof StringLiteral) {
            return !isEmptyString(expr);
        } else if (expr instanceof Name && hasType(expr, "java.lang.String")) {
            Name name = (Name) expr;
            return name.resolveConstantExpressionValue() != null;
        }

        return false;
    }

    private void removeEmptyStrings(final List<Pair<ITypeBinding, Expression>> allExprs,
            final AtomicBoolean isRefactoringNeeded) {
        for (Iterator<Pair<ITypeBinding, Expression>> iter = allExprs.iterator(); iter.hasNext();) {
            Pair<ITypeBinding, Expression> expr = iter.next();
            if (expr.getFirst() == null && isEmptyString(expr.getSecond())) {
                iter.remove();
                isRefactoringNeeded.set(true);
            }
        }
    }

    private void removeCallsToToString(final List<Pair<ITypeBinding, Expression>> allExprs,
            final AtomicBoolean isRefactoringNeeded, boolean isInstanceCreationToRewrite) {
        for (ListIterator<Pair<ITypeBinding, Expression>> iter = allExprs.listIterator(); iter.hasNext();) {
            final Pair<ITypeBinding, Expression> expr = iter.next();
            if (expr.getSecond().getNodeType() == ASTNode.METHOD_INVOCATION) {
                final MethodInvocation mi = (MethodInvocation) expr.getSecond();
                if (isMethod(mi, "java.lang.Object", "toString")) {
                    if (mi.getExpression() != null) {
                        iter.set(Pair.<ITypeBinding, Expression>of(null, mi.getExpression()));
                    } else {
                        iter.set(Pair.<ITypeBinding, Expression>of(null, this.ctx.getAST().newThisExpression()));
                    }
                    isRefactoringNeeded.set(true);
                } else if (isToString(mi) || isStringValueOf(mi)) {
                    iter.set(getTypeAndValue(mi));
                    isRefactoringNeeded.set(true);
                }
            }
        }
    }

    private Pair<ITypeBinding, Expression> getTypeAndValue(final MethodInvocation mi) {
        final ITypeBinding expectedType = mi.resolveMethodBinding().getParameterTypes()[0];
        if (hasType(arg0(mi), expectedType.getQualifiedName(),
                getBoxedTypeBinding(expectedType, mi.getAST()).getQualifiedName())) {
            return Pair.<ITypeBinding, Expression>of(null, arg0(mi));
        } else {
            return Pair.<ITypeBinding, Expression>of(expectedType, arg0(mi));
        }
    }

    /**
     * Rewrite the successive calls to append()
     *
     * @param node The node to replace.
     * @param allAppendedStrings All appended strings.
     * @param lastExpr The expression on which the methods are called.
     * @param isInstanceCreationToRewrite
     */
    private void replaceWithNewStringAppends(final Expression node,
            final LinkedList<Pair<ITypeBinding, Expression>> allAppendedStrings, final Expression lastExpr,
            final boolean isInstanceCreationToRewrite) {
        final ASTBuilder b = this.ctx.getASTBuilder();

        Expression result = null;
        final List<Expression> tempStringLiterals = new ArrayList<Expression>();
        final List<Expression> finalStrings = new ArrayList<Expression>();
        final AtomicBoolean isFirst = new AtomicBoolean(true);

        for (final Pair<ITypeBinding, Expression> appendedString : allAppendedStrings) {
            if (isValuedStringLiteralOrConstant(appendedString.getSecond())) {
                tempStringLiterals.add(b.copy(appendedString.getSecond()));
            } else {
                result = handleTempStringLiterals(b, lastExpr, isInstanceCreationToRewrite, result, tempStringLiterals,
                        finalStrings, isFirst);

                if (isFirst.get()) {
                    isFirst.set(false);

                    if (!isInstanceCreationToRewrite) {
                        result = b.copy(lastExpr);
                        finalStrings.add(getTypedExpression(b, appendedString));
                    } else if ((appendedString.getFirst() != null)
                            ? hasType(appendedString.getFirst(), "java.lang.String")
                                    : hasType(appendedString.getSecond(), "java.lang.String")) {
                        result = b.new0(b.copy(((ClassInstanceCreation) lastExpr).getType()),
                                getTypedExpression(b, appendedString));
                    } else {
                        result = b.new0(b.copy(((ClassInstanceCreation) lastExpr).getType()));
                        finalStrings.add(getTypedExpression(b, appendedString));
                    }
                } else {
                    finalStrings.add(getTypedExpression(b, appendedString));
                }
            }
        }

        result = handleTempStringLiterals(b, lastExpr, isInstanceCreationToRewrite, result, tempStringLiterals,
                finalStrings, isFirst);

        for (final Expression finalString : finalStrings) {
            if (result == null) {
                result = finalString;
            } else {
                result = b.invoke(result, "append", finalString);
            }
        }

        ctx.getRefactorings().replace(node, result);
    }

    private Expression handleTempStringLiterals(final ASTBuilder b, final Expression lastExpr,
            final boolean isInstanceCreationToRewrite, Expression result, final List<Expression> tempStringLiterals,
            final List<Expression> finalStrings, final AtomicBoolean isFirst) {
        if (!tempStringLiterals.isEmpty()) {
            final Expression newExpr = getString(b, tempStringLiterals);

            if (isFirst.get()) {
                isFirst.set(false);
                if (isInstanceCreationToRewrite) {
                    result = b.new0(b.copy(((ClassInstanceCreation) lastExpr).getType()), newExpr);
                } else {
                    result = b.copy(lastExpr);
                    finalStrings.add(newExpr);
                }
            } else {
                finalStrings.add(newExpr);
            }

            tempStringLiterals.clear();
        }
        return result;
    }

    private Expression getString(final ASTBuilder b, final List<Expression> tempStringLiterals) {
        final Expression newExpr;
        if (tempStringLiterals.size() == 1) {
            newExpr = tempStringLiterals.get(0);
        } else {
            newExpr = b.infixExpr(Operator.PLUS, tempStringLiterals);
        }
        return newExpr;
    }

    private void replaceWithAppendSubstring(final MethodInvocation node, final MethodInvocation embeddedMI) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        final Expression stringVar = b.copy(embeddedMI.getExpression());
        final List<Expression> args = arguments(embeddedMI);
        final Expression arg0 = b.copy(args.get(0));
        final Expression arg1 = b.copy(args.get(1));
        final Expression lastExpr = b.copy(node.getExpression());
        MethodInvocation newAppendSubstring = null;
        if (arg1 == null) {
            newAppendSubstring = b.invoke(lastExpr, "append", stringVar, arg0);
        } else {
            newAppendSubstring = b.invoke(lastExpr, "append", stringVar, arg0, arg1);
        }

        this.ctx.getRefactorings().replace(node,
                newAppendSubstring);
    }

    private boolean isStringBuilderOrBuffer(final Expression expr) {
        return hasType(expr, "java.lang.StringBuffer", "java.lang.StringBuilder");
    }

    private boolean isVariable(final Expression expr) {
        switch (removeParentheses(expr).getNodeType()) {
        case SIMPLE_NAME:
        case QUALIFIED_NAME:
        case FIELD_ACCESS:
            return true;

        default:
            return false;
        }
    }

    private boolean isToString(final MethodInvocation mi) {
        return isMethod(mi, "java.lang.Boolean", "toString", "boolean")
                || isMethod(mi, "java.lang.Byte", "toString", "byte")
                || isMethod(mi, "java.lang.Character", "toString", "char")
                || isMethod(mi, "java.lang.Short", "toString", "short")
                || isMethod(mi, "java.lang.Integer", "toString", "int")
                || isMethod(mi, "java.lang.Long", "toString", "long")
                || isMethod(mi, "java.lang.Float", "toString", "float")
                || isMethod(mi, "java.lang.Double", "toString", "double");
    }

    private boolean isStringValueOf(final MethodInvocation mi) {
        return isMethod(mi, "java.lang.String", "valueOf", "java.lang.Object")
                || isMethod(mi, "java.lang.String", "valueOf", "boolean")
                || isMethod(mi, "java.lang.Boolean", "valueOf", "boolean")
                || isMethod(mi, "java.lang.String", "valueOf", "char")
                || isMethod(mi, "java.lang.Character", "valueOf", "char")
                || isMethod(mi, "java.lang.String", "valueOf", "int")
                || isMethod(mi, "java.lang.Integer", "valueOf", "int")
                || isMethod(mi, "java.lang.String", "valueOf", "long")
                || isMethod(mi, "java.lang.Long", "valueOf", "long")
                || isMethod(mi, "java.lang.String", "valueOf", "float")
                || isMethod(mi, "java.lang.Float", "valueOf", "float")
                || isMethod(mi, "java.lang.String", "valueOf", "double")
                || isMethod(mi, "java.lang.Double", "valueOf", "double");
    }

    private Expression getTypedExpression(final ASTBuilder b, final Pair<ITypeBinding, Expression> typeAndValue) {
        Expression expression = null;
        if (typeAndValue.getFirst() != null) {
            expression = b.cast(b.type(typeAndValue.getFirst().getQualifiedName()),
                    b.copy(typeAndValue.getSecond()));
        } else if (typeAndValue.getFirst() == null)  {
            expression = b.copy(typeAndValue.getSecond());
        }
        return expression;
    }

    @Override
    public boolean visit(InfixExpression node) {
        if (isStringConcat(node)) {
            final LinkedList<Pair<ITypeBinding, Expression>> allOperands =
                    new LinkedList<Pair<ITypeBinding, Expression>>();
            readSubExpressions(node, allOperands, new AtomicBoolean(false));
            boolean replaceNeeded = filterOutEmptyStringsFromStringConcat(allOperands);
            if (replaceNeeded) {
                this.ctx.getRefactorings().replace(node, createStringConcats(allOperands));
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean filterOutEmptyStringsFromStringConcat(final List<Pair<ITypeBinding, Expression>> allOperands) {
        boolean replaceNeeded = false;
        boolean canRemoveEmptyStrings = false;
        for (int i = 0; i < allOperands.size(); i++) {
            Pair<ITypeBinding, Expression> expr = allOperands.get(i);
            boolean canNowRemoveEmptyStrings = canRemoveEmptyStrings || hasType(expr.getSecond(), "java.lang.String");
            if (isEmptyString(expr.getSecond())) {
                boolean removeExpr = false;
                if (canRemoveEmptyStrings) {
                    removeExpr = true;
                } else if (canNowRemoveEmptyStrings
                        && i + 1 < allOperands.size()
                        && hasType(allOperands.get(i + 1).getSecond(), "java.lang.String")) {
                    removeExpr = true;
                }

                if (removeExpr) {
                    allOperands.remove(i);
                    replaceNeeded = true;
                }
            }
            canRemoveEmptyStrings = canNowRemoveEmptyStrings;
        }
        return replaceNeeded;
    }

    private Expression createStringConcats(final List<Pair<ITypeBinding, Expression>> appendedStrings) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        switch (appendedStrings.size()) {
        case 0:
            return b.string("");

        case 1:
            final Pair<ITypeBinding, Expression> expr = appendedStrings.get(0);
            if (hasType(expr.getSecond(), "java.lang.String")) {
                return b.copy(expr.getSecond());
            }
            return b.invoke("String", "valueOf", getTypedExpression(b, expr));

        default: // >= 2
            boolean isFirstAndNotAString = isFirstAndNotAString(appendedStrings);

            List<Expression> concatenateStrings = new ArrayList<Expression>(appendedStrings.size());
            for (final Pair<ITypeBinding, Expression> typeAndValue : appendedStrings) {
                if (isFirstAndNotAString) {
                    concatenateStrings.add(b.invoke("String", "valueOf", getTypedExpression(b, typeAndValue)));
                    isFirstAndNotAString = false;
                } else {
                    concatenateStrings.add(b.parenthesizeIfNeeded(getTypedExpression(b, typeAndValue)));
                }
            }
            return b.infixExpr(Operator.PLUS, concatenateStrings);
        }
    }

    private boolean isFirstAndNotAString(final List<Pair<ITypeBinding, Expression>> appendedStrings) {
        final Pair<ITypeBinding, Expression> arg0 = appendedStrings.get(0);
        final Pair<ITypeBinding, Expression> arg1 = appendedStrings.get(1);

        return !hasType(arg0.getSecond(), "java.lang.String")
                && !hasType(arg1.getSecond(), "java.lang.String");
    }
}

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
package org.autorefactor.jdt.internal.ui.fix;

import static org.eclipse.jdt.core.dom.ASTNode.FIELD_ACCESS;
import static org.eclipse.jdt.core.dom.ASTNode.QUALIFIED_NAME;
import static org.eclipse.jdt.core.dom.ASTNode.SIMPLE_NAME;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.concurrent.atomic.AtomicBoolean;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Bindings;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.StringLiteral;

/** See {@link #getDescription()} method. */
public class StringBuilderCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_StringBuilderCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_StringBuilderCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_StringBuilderCleanUp_reason;
    }

    private boolean isEmptyString(final Expression expression) {
        StringLiteral stringLiteral= ASTNodes.as(expression, StringLiteral.class);

        return "".equals(expression.resolveConstantExpressionValue()) //$NON-NLS-1$
                // Due to a bug with ASTNode.resolveConstantExpressionValue()
                // in Eclipse 3.7.2 and 3.8.0, this second check is necessary
                || stringLiteral != null && "".equals(stringLiteral.getLiteralValue()); //$NON-NLS-1$
    }

    @Override
    public boolean visit(final MethodInvocation node) {
        if (node.getExpression() != null && "append".equals(node.getName().getIdentifier()) //$NON-NLS-1$
                && ASTNodes.arguments(node).size() == 1
                // Most expensive check comes last
                && isStringBuilderOrBuffer(node.getExpression())) {
            MethodInvocation embeddedMI= ASTNodes.as(ASTNodes.arguments(node).get(0), MethodInvocation.class);

            if (ASTNodes.usesGivenSignature(embeddedMI, String.class.getCanonicalName(), "substring", int.class.getSimpleName(), int.class.getSimpleName()) //$NON-NLS-1$
                    || ASTNodes.usesGivenSignature(embeddedMI, CharSequence.class.getCanonicalName(), "subSequence", int.class.getSimpleName(), int.class.getSimpleName())) { //$NON-NLS-1$
                replaceWithAppendSubstring(node, embeddedMI);
                return false;
            }

            return maybeRefactorAppending(node);
        }

        if (ASTNodes.usesGivenSignature(node, StringBuilder.class.getCanonicalName(), "toString") //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(node, StringBuffer.class.getCanonicalName(), "toString")) { //$NON-NLS-1$
            LinkedList<Pair<ITypeBinding, Expression>> allAppendedStrings= new LinkedList<>();
            Expression lastExpression= readAppendMethod(node.getExpression(), allAppendedStrings,
                    new AtomicBoolean(false), new AtomicBoolean(false));

            if (lastExpression instanceof ClassInstanceCreation) {
                // Replace with String concatenation
                this.ctx.getRefactorings().replace(node, createStringConcats(allAppendedStrings));
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean visit(final ClassInstanceCreation node) {
        if (ASTNodes.hasType(node, StringBuilder.class.getCanonicalName(), StringBuffer.class.getCanonicalName())
                && ASTNodes.arguments(node).size() == 1) {
            Expression arg0= ASTNodes.arguments(node).get(0);

            if (ASTNodes.hasType(arg0, String.class.getCanonicalName())
                    && (arg0 instanceof InfixExpression || arg0 instanceof MethodInvocation
                            && (isToString((MethodInvocation) arg0) || isStringValueOf((MethodInvocation) arg0)))) {
                return maybeRefactorAppending(node);
            }
        }

        return true;
    }

    private boolean maybeRefactorAppending(final Expression node) {
        LinkedList<Pair<ITypeBinding, Expression>> allAppendedStrings= new LinkedList<>();
        AtomicBoolean isRefactoringNeeded= new AtomicBoolean(false);
        AtomicBoolean isInstanceCreationToRewrite= new AtomicBoolean(false);
        Expression lastExpression= readAppendMethod(node, allAppendedStrings, isRefactoringNeeded,
                isInstanceCreationToRewrite);

        if (lastExpression != null) {
            removeEmptyStrings(allAppendedStrings, isRefactoringNeeded);
            removeCallsToToString(allAppendedStrings, isRefactoringNeeded);

            if (isRefactoringNeeded.get()) {
                if (!allAppendedStrings.isEmpty() || !isVariable(lastExpression) || !(node.getParent() instanceof Statement)) {
                    return maybeReplaceWithNewStringAppends(node, allAppendedStrings, lastExpression, isInstanceCreationToRewrite.get());
                }

                Refactorings r= ctx.getRefactorings();
                if (ASTNodes.canHaveSiblings((Statement) node.getParent())) {
                    r.remove(node.getParent());
                } else {
                    r.replace(node.getParent(), ctx.getASTBuilder().block());
                }

                return false;
            }
        }

        return true;
    }

    private Expression readAppendMethod(final Expression expression,
            final LinkedList<Pair<ITypeBinding, Expression>> allOperands, final AtomicBoolean isRefactoringNeeded,
            final AtomicBoolean isInstanceCreationToRewrite) {
        Expression exp= ASTNodes.getUnparenthesedExpression(expression);
        if (isStringBuilderOrBuffer(exp)) {
            if (exp instanceof MethodInvocation) {
                MethodInvocation mi= (MethodInvocation) exp;
                if ("append".equals(mi.getName().getIdentifier()) && ASTNodes.arguments(mi).size() == 1) { //$NON-NLS-1$
                    Expression arg0= ASTNodes.arguments(mi).get(0);
                    readSubExpressions(arg0, allOperands, isRefactoringNeeded);
                    return readAppendMethod(mi.getExpression(), allOperands, isRefactoringNeeded,
                            isInstanceCreationToRewrite);
                }
            } else if (exp instanceof ClassInstanceCreation) {
                ClassInstanceCreation cic= (ClassInstanceCreation) exp;

                if (ASTNodes.arguments(cic).size() == 1) {
                    Expression arg0= ASTNodes.arguments(cic).get(0);

                    if (isStringBuilderOrBuffer(cic)
                            && (ASTNodes.hasType(arg0, String.class.getCanonicalName()) || ASTNodes.instanceOf(arg0, CharSequence.class.getCanonicalName()))) {
                        isInstanceCreationToRewrite.set(true);
                        readSubExpressions(arg0, allOperands, isRefactoringNeeded);
                    }
                } else if (ASTNodes.arguments(cic).isEmpty() && !allOperands.isEmpty()
                        && (allOperands.getFirst().getFirst() != null
                                ? ASTNodes.hasType(allOperands.getFirst().getFirst(), String.class.getCanonicalName())
                                : ASTNodes.hasType(allOperands.getFirst().getSecond(), String.class.getCanonicalName()))) {
                    isInstanceCreationToRewrite.set(true);
                    isRefactoringNeeded.set(true);
                }

                return cic;
            } else {
                return expression;
            }
        }

        return null;
    }

    private void readSubExpressions(final Expression arg, final LinkedList<Pair<ITypeBinding, Expression>> results,
            final AtomicBoolean isRefactoringNeeded) {
        InfixExpression ie= ASTNodes.as(arg, InfixExpression.class);

        if (ie != null && isStringConcat(ie)) {
            if (ie.hasExtendedOperands()) {
                List<Expression> reversed= new ArrayList<>(ASTNodes.extendedOperands(ie));
                Collections.reverse(reversed);

                if (isValuedStringLiteralOrConstant(reversed.get(0)) && !results.isEmpty()
                        && isValuedStringLiteralOrConstant(results.get(0).getSecond())) {
                    isRefactoringNeeded.set(true);
                }

                for (Expression op : reversed) {
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

        if (isValuedStringLiteralOrConstant(arg) && !results.isEmpty()
                && isValuedStringLiteralOrConstant(results.get(0).getSecond())) {
            isRefactoringNeeded.set(true);
        }

        results.addFirst(Pair.<ITypeBinding, Expression>of(null, arg));
    }

    private boolean isStringConcat(final InfixExpression node) {
        if (!ASTNodes.hasOperator(node, InfixExpression.Operator.PLUS) || !ASTNodes.hasType(node, String.class.getCanonicalName())) {
            return false;
        }

        if (!isValuedStringLiteralOrConstant(node.getLeftOperand())
                || !isValuedStringLiteralOrConstant(node.getRightOperand())) {
            return true;
        }

        for (Expression expression : ASTNodes.extendedOperands(node)) {
            if (!isValuedStringLiteralOrConstant(expression)) {
                return true;
            }
        }

        return false;
    }

    private boolean isValuedStringLiteralOrConstant(final Expression expression) {
        if (expression instanceof StringLiteral) {
            return !isEmptyString(expression);
        }

        if (ASTNodes.hasType(expression, String.class.getCanonicalName())) {
            if (expression instanceof Name) {
                Name name= (Name) expression;
                return name.resolveConstantExpressionValue() != null;
            }

            if (expression instanceof FieldAccess) {
                FieldAccess fieldAccess= (FieldAccess) expression;
                return fieldAccess.resolveConstantExpressionValue() != null;
            }
        }

        return false;
    }

    private void removeEmptyStrings(final List<Pair<ITypeBinding, Expression>> allExprs,
            final AtomicBoolean isRefactoringNeeded) {
        for (Iterator<Pair<ITypeBinding, Expression>> iter= allExprs.iterator(); iter.hasNext();) {
            Pair<ITypeBinding, Expression> expression= iter.next();

            if (expression.getFirst() == null && isEmptyString(expression.getSecond())) {
                iter.remove();
                isRefactoringNeeded.set(true);
            }
        }
    }

    private void removeCallsToToString(final List<Pair<ITypeBinding, Expression>> allExprs,
            final AtomicBoolean isRefactoringNeeded) {
        for (ListIterator<Pair<ITypeBinding, Expression>> iter= allExprs.listIterator(); iter.hasNext();) {
            Pair<ITypeBinding, Expression> expression= iter.next();

            if (expression.getSecond().getNodeType() == ASTNode.METHOD_INVOCATION) {
                MethodInvocation mi= (MethodInvocation) expression.getSecond();

                if (ASTNodes.usesGivenSignature(mi, Object.class.getCanonicalName(), "toString")) { //$NON-NLS-1$
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
        IMethodBinding expectedType= mi.resolveMethodBinding();

        if (expectedType == null) {
            return null;
        }

        Expression argument= ASTNodes.arguments(mi).get(0);

        if (ASTNodes.hasType(argument, expectedType.getParameterTypes()[0].getQualifiedName(),
                Bindings.getBoxedTypeBinding(expectedType.getParameterTypes()[0], mi.getAST()).getQualifiedName())) {
            return Pair.<ITypeBinding, Expression>of(null, argument);
        }

        return Pair.<ITypeBinding, Expression>of(expectedType.getParameterTypes()[0], argument);
    }

    /**
     * Rewrite the successive calls to append()
     *
     * @param node                        The node to replace.
     * @param allAppendedStrings          All appended strings.
     * @param lastExpression              The expression on which the methods are
     *                                    called.
     * @param isInstanceCreationToRewrite
     * @return
     */
    private boolean maybeReplaceWithNewStringAppends(final Expression node,
            final LinkedList<Pair<ITypeBinding, Expression>> allAppendedStrings, final Expression lastExpression,
            final boolean isInstanceCreationToRewrite) {
        ASTNodeFactory b= this.ctx.getASTBuilder();

        Expression result= null;
        List<Expression> tempStringLiterals= new ArrayList<>();
        List<Expression> finalStrings= new ArrayList<>();
        AtomicBoolean isFirst= new AtomicBoolean(true);

        for (Pair<ITypeBinding, Expression> appendedString : allAppendedStrings) {
            if (appendedString == null) {
                return true;
            }

            if (isValuedStringLiteralOrConstant(appendedString.getSecond())) {
                tempStringLiterals.add(b.createCopyTarget(appendedString.getSecond()));
            } else {
                result= handleTempStringLiterals(b, lastExpression, isInstanceCreationToRewrite, result, tempStringLiterals,
                        finalStrings, isFirst);

                if (isFirst.get()) {
                    isFirst.set(false);

                    if (!isInstanceCreationToRewrite) {
                        result= b.createCopyTarget(lastExpression);
                        finalStrings.add(getTypedExpression(b, appendedString));
                    } else if (appendedString.getFirst() != null
                            ? ASTNodes.hasType(appendedString.getFirst(), String.class.getCanonicalName())
                            : ASTNodes.hasType(appendedString.getSecond(), String.class.getCanonicalName())) {
                        result= b.new0(b.createCopyTarget(((ClassInstanceCreation) lastExpression).getType()),
                                getTypedExpression(b, appendedString));
                    } else {
                        result= b.new0(b.createCopyTarget(((ClassInstanceCreation) lastExpression).getType()));
                        finalStrings.add(getTypedExpression(b, appendedString));
                    }
                } else {
                    finalStrings.add(getTypedExpression(b, appendedString));
                }
            }
        }

        result= handleTempStringLiterals(b, lastExpression, isInstanceCreationToRewrite, result, tempStringLiterals,
                finalStrings, isFirst);

        for (Expression finalString : finalStrings) {
            if (result == null) {
                result= finalString;
            } else {
                result= b.invoke(result, "append", finalString); //$NON-NLS-1$
            }
        }

        ctx.getRefactorings().replace(node, result);
        return false;
    }

    private Expression handleTempStringLiterals(final ASTNodeFactory b, final Expression lastExpression,
            final boolean isInstanceCreationToRewrite, Expression result, final List<Expression> tempStringLiterals,
            final List<Expression> finalStrings, final AtomicBoolean isFirst) {
        if (!tempStringLiterals.isEmpty()) {
            Expression newExpression= getString(b, tempStringLiterals);

            if (isFirst.get()) {
                isFirst.set(false);
                if (isInstanceCreationToRewrite) {
                    result= b.new0(b.createCopyTarget(((ClassInstanceCreation) lastExpression).getType()), newExpression);
                } else {
                    result= b.createCopyTarget(lastExpression);
                    finalStrings.add(newExpression);
                }
            } else {
                finalStrings.add(newExpression);
            }

            tempStringLiterals.clear();
        }

        return result;
    }

    private Expression getString(final ASTNodeFactory b, final List<Expression> tempStringLiterals) {
        Expression newExpression;
        if (tempStringLiterals.size() == 1) {
            newExpression= tempStringLiterals.get(0);
        } else {
            newExpression= b.infixExpression(InfixExpression.Operator.PLUS, tempStringLiterals);
        }

        return newExpression;
    }

    private void replaceWithAppendSubstring(final MethodInvocation node, final MethodInvocation embeddedMI) {
        ASTNodeFactory b= this.ctx.getASTBuilder();
        Expression stringVar= b.createCopyTarget(embeddedMI.getExpression());
        List<Expression> args= ASTNodes.arguments(embeddedMI);
        Expression arg0= b.createCopyTarget(args.get(0));
        Expression arg1= b.createCopyTarget(args.get(1));
        Expression lastExpression= b.createCopyTarget(node.getExpression());
        MethodInvocation newAppendSubstring= null;

        if (arg1 == null) {
            newAppendSubstring= b.invoke(lastExpression, "append", stringVar, arg0); //$NON-NLS-1$
        } else {
            newAppendSubstring= b.invoke(lastExpression, "append", stringVar, arg0, arg1); //$NON-NLS-1$
        }

        this.ctx.getRefactorings().replace(node, newAppendSubstring);
    }

    private boolean isStringBuilderOrBuffer(final Expression expression) {
        return ASTNodes.hasType(expression, StringBuffer.class.getCanonicalName(), StringBuilder.class.getCanonicalName());
    }

    private boolean isVariable(final Expression expression) {
        switch (ASTNodes.getUnparenthesedExpression(expression).getNodeType()) {
        case SIMPLE_NAME:
        case QUALIFIED_NAME:
        case FIELD_ACCESS:
            return true;

        default:
            return false;
        }
    }

    private boolean isToString(final MethodInvocation mi) {
        return ASTNodes.usesGivenSignature(mi, Boolean.class.getCanonicalName(), "toString", boolean.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, Byte.class.getCanonicalName(), "toString", byte.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, Character.class.getCanonicalName(), "toString", char.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, Short.class.getCanonicalName(), "toString", short.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, Integer.class.getCanonicalName(), "toString", int.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, Long.class.getCanonicalName(), "toString", long.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, Float.class.getCanonicalName(), "toString", float.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, Double.class.getCanonicalName(), "toString", double.class.getSimpleName()); //$NON-NLS-1$
    }

    private boolean isStringValueOf(final MethodInvocation mi) {
        return ASTNodes.usesGivenSignature(mi, String.class.getCanonicalName(), "valueOf", Object.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, String.class.getCanonicalName(), "valueOf", boolean.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, Boolean.class.getCanonicalName(), "valueOf", boolean.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, String.class.getCanonicalName(), "valueOf", char.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, Character.class.getCanonicalName(), "valueOf", char.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, String.class.getCanonicalName(), "valueOf", int.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, Integer.class.getCanonicalName(), "valueOf", int.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, String.class.getCanonicalName(), "valueOf", long.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, Long.class.getCanonicalName(), "valueOf", long.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, String.class.getCanonicalName(), "valueOf", float.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, Float.class.getCanonicalName(), "valueOf", float.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, String.class.getCanonicalName(), "valueOf", double.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, Double.class.getCanonicalName(), "valueOf", double.class.getSimpleName()); //$NON-NLS-1$
    }

    private Expression getTypedExpression(final ASTNodeFactory b, final Pair<ITypeBinding, Expression> typeAndValue) {
        Expression expression= null;
        if (typeAndValue.getFirst() != null) {
            expression= b.cast(b.type(typeAndValue.getFirst().getQualifiedName()), b.createCopyTarget(typeAndValue.getSecond()));
        } else if (typeAndValue.getFirst() == null) {
            expression= b.createCopyTarget(typeAndValue.getSecond());
        }

        return expression;
    }

    @Override
    public boolean visit(final InfixExpression node) {
        if (isStringConcat(node)) {
            LinkedList<Pair<ITypeBinding, Expression>> allOperands= new LinkedList<>();
            readSubExpressions(node, allOperands, new AtomicBoolean(false));
            boolean replaceNeeded= filterOutEmptyStringsFromStringConcat(allOperands);

            if (replaceNeeded) {
                this.ctx.getRefactorings().replace(node, createStringConcats(allOperands));
                return false;
            }
        }

        return true;
    }

    private boolean filterOutEmptyStringsFromStringConcat(final List<Pair<ITypeBinding, Expression>> allOperands) {
        boolean replaceNeeded= false;
        boolean canRemoveEmptyStrings= false;

        for (int i= 0; i < allOperands.size(); i++) {
            Pair<ITypeBinding, Expression> expression= allOperands.get(i);
            boolean canNowRemoveEmptyStrings= canRemoveEmptyStrings || ASTNodes.hasType(expression.getSecond(), String.class.getCanonicalName());

            if (isEmptyString(expression.getSecond())) {
                boolean removeExpression= canRemoveEmptyStrings || canNowRemoveEmptyStrings && i + 1 < allOperands.size()
                        && ASTNodes.hasType(allOperands.get(i + 1).getSecond(), String.class.getCanonicalName());
                if (removeExpression) {
                    allOperands.remove(i);
                    replaceNeeded= true;
                }
            }

            canRemoveEmptyStrings= canNowRemoveEmptyStrings;
        }

        return replaceNeeded;
    }

    private Expression createStringConcats(final List<Pair<ITypeBinding, Expression>> appendedStrings) {
        ASTNodeFactory b= this.ctx.getASTBuilder();
        switch (appendedStrings.size()) {
        case 0:
            return b.string(""); //$NON-NLS-1$

        case 1:
            Pair<ITypeBinding, Expression> expression= appendedStrings.get(0);
            if (ASTNodes.hasType(expression.getSecond(), String.class.getCanonicalName())) {
                return b.createCopyTarget(expression.getSecond());
            }

            return b.invoke(String.class.getSimpleName(), "valueOf", getTypedExpression(b, expression)); //$NON-NLS-1$

        default: // >= 2
            boolean isFirstAndNotAString= isFirstAndNotAString(appendedStrings);

            List<Expression> concatenateStrings= new ArrayList<>(appendedStrings.size());
            for (Pair<ITypeBinding, Expression> typeAndValue : appendedStrings) {
                if (isFirstAndNotAString) {
                    concatenateStrings.add(b.invoke(String.class.getSimpleName(), "valueOf", getTypedExpression(b, typeAndValue))); //$NON-NLS-1$
                    isFirstAndNotAString= false;
                } else {
                    concatenateStrings.add(b.parenthesizeIfNeeded(getTypedExpression(b, typeAndValue)));
                }
            }

            return b.infixExpression(InfixExpression.Operator.PLUS, concatenateStrings);
        }
    }

    private boolean isFirstAndNotAString(final List<Pair<ITypeBinding, Expression>> appendedStrings) {
        Pair<ITypeBinding, Expression> arg0= appendedStrings.get(0);
        Pair<ITypeBinding, Expression> arg1= appendedStrings.get(1);

        return !ASTNodes.hasType(arg0.getSecond(), String.class.getCanonicalName()) && !ASTNodes.hasType(arg1.getSecond(), String.class.getCanonicalName());
    }
}

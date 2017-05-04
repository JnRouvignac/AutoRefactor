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
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.StringLiteral;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.ASTNode.*;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.*;

/** See {@link #getDescription()} method. */
public class StringBuilderRefactoring extends AbstractRefactoringRule {
    @Override
    public String getDescription() {
        return ""
            + "Refactors to a proper use of StringBuilders:\n"
            + "- replace String concatenations using operator '+' as parameters"
            + " of StringBuffer/StringBuilder.append(),\n"
            + "- replace chained call to StringBuffer/StringBuilder constructor followed by calls to append()"
            + " and call toString() with straight String concatenation using operator '+'.";
    }

    @Override
    public String getName() {
        return "StringBuilder";
    }

    private int getJavaMinorVersion() {
        return ctx.getJavaProjectOptions().getJavaSERelease().getMinorVersion();
    }

    @Override
    public boolean visit(ClassInstanceCreation node) {
        if (getJavaMinorVersion() >= 5
                && hasType(node, "java.lang.StringBuffer")) {
            // TODO JNR replace with StringBuilder
            // check that the current method return type is not StringBuffer
            // do we need the CFG + live variable analysis first?
        }
        return VISIT_SUBTREE;
    }

    @Override
    public boolean visit(InfixExpression node) {
        if (isStringConcat(node)) {
            final LinkedList<Pair<ITypeBinding, Expression>> allOperands =
                    new LinkedList<Pair<ITypeBinding, Expression>>();
            addAllSubExpressions(node, allOperands, new AtomicBoolean(false));
            boolean replaceNeeded = filterOutEmptyStringsFromStringConcat(allOperands);
            if (replaceNeeded) {
                this.ctx.getRefactorings().replace(node, createStringConcats(allOperands));
                return DO_NOT_VISIT_SUBTREE;
            }
            // FIXME In theory commented code down below should work better than current code above
            // (preserving comments, etc.), but in practice it does not work at all.
            // for (Expression operand : allOperands) {
            // if ("".equals(operand.resolveConstantExpressionValue())) {
            // this.ctx.getRefactorings().remove(operand);
            // }
            // }
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

            final LinkedList<Pair<ITypeBinding, Expression>> allAppendedStrings =
                    new LinkedList<Pair<ITypeBinding, Expression>>();
            final AtomicBoolean hasStringConcat = new AtomicBoolean(false);
            final Expression lastExpr = collectAllAppendedStrings(node, allAppendedStrings, hasStringConcat);

            if (simplifyAppending(allAppendedStrings, hasStringConcat.get())) {
                if (allAppendedStrings.isEmpty()
                        && isVariable(node.getExpression())
                        && node.getParent() instanceof Statement) {
                    ctx.getRefactorings().remove(node.getParent());
                } else {
                    replaceWithNewStringAppends(node, allAppendedStrings, lastExpr);
                }
                return DO_NOT_VISIT_SUBTREE;
            }
        } else if (isMethod(node, "java.lang.StringBuilder", "toString")
                || isMethod(node, "java.lang.StringBuffer", "toString")) {
            final LinkedList<Pair<ITypeBinding, Expression>> allAppendedStrings =
                    new LinkedList<Pair<ITypeBinding, Expression>>();
            final Expression lastExpr = collectAllAppendedStrings(node.getExpression(), allAppendedStrings,
                    new AtomicBoolean(false));
            // TODO new StringBuffer().append(" bla").append("bla").toString();
            // outputs " blabla"
            if (lastExpr instanceof ClassInstanceCreation) {
                // Replace with String concatenation
                this.ctx.getRefactorings().replace(node,
                        createStringConcats(allAppendedStrings));
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    /**
     * Rewrite the successive calls to append()
     *
     * @param node The node to replace.
     * @param allAppendedStrings All appended strings.
     * @param lastExpr The expression on which the methods are called.
     */
    private void replaceWithNewStringAppends(final MethodInvocation node,
            final LinkedList<Pair<ITypeBinding, Expression>> allAppendedStrings, final Expression lastExpr) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        Expression result = b.copy(lastExpr);

        int i = 0;
        List<Expression> tempStringLiterals = new ArrayList<Expression>();
        while (i < allAppendedStrings.size()) {
            if (allAppendedStrings.get(i).getSecond() instanceof StringLiteral) {
                tempStringLiterals.add(b.copy(allAppendedStrings.get(i).getSecond()));
            } else {
                if (!tempStringLiterals.isEmpty()) {
                    result = addStringConcat(b, result, tempStringLiterals);

                    tempStringLiterals.clear();
                }

                if (result == null) {
                    result = b.copy(allAppendedStrings.get(i).getSecond());
                } else {
                    result = b.invoke(result, "append", getTypedExpression(b, allAppendedStrings.get(i)));
                }
            }

            i++;
        }

        if (!tempStringLiterals.isEmpty()) {
            result = addStringConcat(b, result, tempStringLiterals);
        }
        ctx.getRefactorings().replace(node, result);
    }

    private Expression addStringConcat(final ASTBuilder b, Expression result,
            final List<Expression> copyOfStringLiterals) {
        if (copyOfStringLiterals.size() == 1) {
            if (result == null) {
                result = copyOfStringLiterals.get(0);
            } else {
                result = b.invoke(result, "append", copyOfStringLiterals.get(0));
            }
        } else {
            if (result == null) {
                result = b.infixExpr(Operator.PLUS, copyOfStringLiterals);
            } else {
                result = b.invoke(result, "append", b.infixExpr(Operator.PLUS, copyOfStringLiterals));
            }
        }
        return result;
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

    /**
     * Simplify an appending chain.
     *
     * @param allAppendedStrings All appended strings
     * @param hasStringConcat True if has string concatenation
     * @return True if rewrite is needed
     */
    private boolean simplifyAppending(final LinkedList<Pair<ITypeBinding, Expression>> allAppendedStrings,
            final boolean hasStringConcat) {
        final boolean needRefactor1 = filterOutEmptyStrings(allAppendedStrings);
        final boolean needRefactor2 = removeCallsToToString(allAppendedStrings);
        return needRefactor1 || needRefactor2 || hasStringConcat;
    }

    private boolean filterOutEmptyStrings(final List<Pair<ITypeBinding, Expression>> allExprs) {
        boolean result = false;
        for (Iterator<Pair<ITypeBinding, Expression>> iter = allExprs.iterator(); iter.hasNext();) {
            Pair<ITypeBinding, Expression> expr = iter.next();
            if (expr.getFirst() == null && isEmptyString(expr.getSecond())) {
                iter.remove();
                result = true;
            }
        }
        return result;
    }

    private boolean removeCallsToToString(final List<Pair<ITypeBinding, Expression>> allExprs) {
        boolean result = false;
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
                    result = true;
                } else if (isToString(mi) || isStringValueOf(mi)) {
                    iter.set(getTypeAndValue(mi));
                    result = true;
                }
            }
        }
        return result;
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

    private Pair<ITypeBinding, Expression> getTypeAndValue(final MethodInvocation mi) {
        final ITypeBinding expectedType = mi.resolveMethodBinding().getParameterTypes()[0];
        final ITypeBinding actualType = arg0(mi).resolveTypeBinding();

        ITypeBinding otherType = null;
        if (!expectedType.equals(actualType)
                && !getBoxedTypeBinding(expectedType, mi.getAST()).equals(actualType)) {
            otherType = expectedType;
        }

        return Pair.<ITypeBinding, Expression>of(otherType, arg0(mi));
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

        default: // >== 2
            final Pair<ITypeBinding, Expression> arg0 = appendedStrings.get(0);
            final Pair<ITypeBinding, Expression> arg1 = appendedStrings.get(1);

            List<Expression> concatenateStrings = new ArrayList<Expression>(appendedStrings.size());
            boolean prependEmptyString = !hasType(arg0.getSecond(), "java.lang.String")
                    && !hasType(arg1.getSecond(), "java.lang.String");

            if (prependEmptyString) {
                concatenateStrings.add(b.string(""));
            }

            for (final Pair<ITypeBinding, Expression> typeAndValue : appendedStrings) {
                concatenateStrings.add(b.parenthesizeIfNeeded(getTypedExpression(b, typeAndValue)));
            }
            return b.infixExpr(Operator.PLUS, concatenateStrings);
        }
    }

    private Expression collectAllAppendedStrings(final Expression expr,
            final LinkedList<Pair<ITypeBinding, Expression>> allOperands, final AtomicBoolean hasStringConcat) {
        final Expression exp = removeParentheses(expr);
        if (isStringBuilderOrBuffer(exp)) {
            if (exp instanceof MethodInvocation) {
                final MethodInvocation mi = (MethodInvocation) exp;
                if ("append".equals(mi.getName().getIdentifier())
                        && arguments(mi).size() == 1) {
                    final Expression arg0 = arguments(mi).get(0);
                    addAllSubExpressions(arg0, allOperands, hasStringConcat);
                    return collectAllAppendedStrings(mi.getExpression(), allOperands, hasStringConcat);
                }
            } else if (exp instanceof ClassInstanceCreation) {
                final ClassInstanceCreation cic = (ClassInstanceCreation) exp;
                if (arguments(cic).size() == 1) {
                    final Expression arg0 = arguments(cic).get(0);
                    if (isStringBuilderOrBuffer(cic)
                        && (hasType(arg0, "java.lang.String")
                                || instanceOf(arg0, "java.lang.CharSequence"))) {
                        allOperands.addFirst(Pair.<ITypeBinding, Expression>of(null, arg0));
                    }
                }
                return cic;
            } else if (exp instanceof Name || exp instanceof FieldAccess) {
                return exp;
            }
        }
        return null;
    }

    private void addAllSubExpressions(final Expression arg, final LinkedList<Pair<ITypeBinding, Expression>> results,
            final AtomicBoolean hasStringConcat) {
        if (arg instanceof InfixExpression) {
            final InfixExpression ie = (InfixExpression) arg;
            if (isStringConcat(ie)) {
                if (ie.hasExtendedOperands()) {
                    final List<Expression> reversed = new ArrayList<Expression>(extendedOperands(ie));
                    Collections.reverse(reversed);
                    for (Expression op : reversed) {
                        addAllSubExpressions(op, results, hasStringConcat);
                    }
                }
                addAllSubExpressions(ie.getRightOperand(), results, hasStringConcat);
                addAllSubExpressions(ie.getLeftOperand(), results, hasStringConcat);
                hasStringConcat.set(true);
                return;
            }
        }
        results.addFirst(Pair.<ITypeBinding, Expression>of(null, arg));
    }

    private boolean isStringConcat(final InfixExpression node) {
        if (!hasOperator(node, PLUS) || !hasType(node, "java.lang.String")) {
            return false;
        }
        if (isEmptyStringLiteral(node.getLeftOperand())
                || isEmptyStringLiteral(node.getRightOperand())) {
            return true;
        }
        for (Expression expr : extendedOperands(node)) {
            if (isEmptyStringLiteral(expr)) {
                return true;
            }
        }
        return false;
    }

    private boolean isEmptyStringLiteral(Expression expr) {
        return !(expr instanceof StringLiteral) || isEmptyString(expr);
    }
}

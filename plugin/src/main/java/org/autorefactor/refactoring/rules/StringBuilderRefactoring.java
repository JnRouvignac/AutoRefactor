/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2014 Jean-Noël Rouvignac - initial API and implementation
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
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.concurrent.atomic.AtomicBoolean;

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.StringLiteral;

import static org.autorefactor.refactoring.ASTHelper.*;

/**
 * StringBuilder related refactorings:
 * <ul>
 * <li>StringBuffer to StringBuilder conversions</li>
 * <li>Remove String appends using operator '+' as parameters of
 * StringBuffer/StringBuilder.append()</li>
 * <li>Replace calls to StringBuffer/StringBuilder constructor + calls to
 * append() + calls toString() with straight String concatenation with operator
 * '+'</li>
 * </ul>
 */
public class StringBuilderRefactoring extends AbstractRefactoringRule {

    private int getJavaMinorVersion() {
        return ctx.getJavaProjectOptions().getJavaSERelease().getMinorVersion();
    }

    /** {@inheritDoc} */
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

    /** {@inheritDoc} */
    @Override
    public boolean visit(InfixExpression node) {
        // TODO JNR also remove valueOf() methods in these cases, etc.:
        // String s = "" + String.valueOf(1);
        // String s = "" + Integer.toString(1);
        // String s = "" + Long.toString(1);
        if (Operator.PLUS.equals(node.getOperator())
                && hasType(node, "java.lang.String")) {
            final LinkedList<Expression> allOperands = new LinkedList<Expression>();
            addAllSubExpressions(node, allOperands, null);
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

    private boolean filterOutEmptyStringsFromStringConcat(List<Expression> allOperands) {
        boolean replaceNeeded = false;
        boolean canRemoveEmptyStrings = false;
        for (int i = 0; i < allOperands.size(); i++) {
            Expression expr = allOperands.get(i);
            boolean canNowRemoveEmptyStrings = canRemoveEmptyStrings || hasType(expr, "java.lang.String");
            if (isEmptyString(expr)) {
                boolean removeExpr = false;
                if (canRemoveEmptyStrings) {
                    removeExpr = true;
                } else if (canNowRemoveEmptyStrings
                        && i + 1 < allOperands.size()
                        && hasType(allOperands.get(i + 1), "java.lang.String")) {
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

    private boolean isEmptyString(Expression expr) {
        return "".equals(expr.resolveConstantExpressionValue())
                // Due to a bug with ASTNode.resolveConstantExpressionValue()
                // in Eclipse 3.7.2 and 3.8.0, this second check is necessary
                || (expr instanceof StringLiteral
                        && "".equals(((StringLiteral) expr).getLiteralValue()));
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(MethodInvocation node) {
        if (node.getExpression() == null) {
            return VISIT_SUBTREE;
        }
        final ITypeBinding typeBinding = node.getExpression().resolveTypeBinding();
        if ("append".equals(node.getName().getIdentifier())
                && arguments(node).size() == 1
                // most expensive check comes last
                && instanceOf(typeBinding, "java.lang.Appendable")) {
            final LinkedList<Expression> allAppendedStrings = new LinkedList<Expression>();
            final AtomicBoolean hasStringConcat = new AtomicBoolean(false);
            final Expression lastExpr = collectAllAppendedStrings(node, allAppendedStrings, hasStringConcat);
            if ((lastExpr instanceof Name || lastExpr instanceof FieldAccess)
                    && isRewriteNeeded(allAppendedStrings, hasStringConcat)) {
                // rewrite the successive calls to append() on an Appendable
                this.ctx.getRefactorings().replace(node,
                        createStringAppends(lastExpr, allAppendedStrings));
                return DO_NOT_VISIT_SUBTREE;
            }

            final MethodInvocation embeddedMI = as(allAppendedStrings, MethodInvocation.class);
            if (isStringValueOf(embeddedMI)
                && (instanceOf(typeBinding, "java.lang.StringBuilder")
                    || instanceOf(typeBinding, "java.lang.StringBuffer"))) {
                final Expression arg0 = arguments(embeddedMI).get(0);
                this.ctx.getRefactorings().replace(node,
                        createStringAppends(lastExpr, Arrays.asList(arg0)));
                return DO_NOT_VISIT_SUBTREE;
            }
            if (isMethod(embeddedMI, "java.lang.String", "substring", "int", "int")
                    || isMethod(embeddedMI, "java.lang.CharSequence", "subSequence", "int", "int")) {
                final ASTBuilder b = this.ctx.getASTBuilder();
                final Expression stringVar = b.copy(embeddedMI.getExpression());
                final List<Expression> args = arguments(embeddedMI);
                final Expression arg0 = b.copy(args.get(0));
                final Expression arg1 = b.copy(args.get(1));
                this.ctx.getRefactorings().replace(node,
                        createAppendSubstring(b, b.copy(lastExpr), stringVar, arg0, arg1));
            }
        } else if (isMethod(node, "java.lang.StringBuilder", "toString")
                || isMethod(node, "java.lang.StringBuffer", "toString")) {
            final LinkedList<Expression> allAppendedStrings = new LinkedList<Expression>();
            final Expression lastExpr = collectAllAppendedStrings(node.getExpression(), allAppendedStrings, null);
            // TODO new StringBuffer().append(" bla").append("bla").toString();
            // outputs " blabla"
            if (lastExpr instanceof ClassInstanceCreation) {
                // replace with String concatenation
                this.ctx.getRefactorings().replace(node,
                        createStringConcats(allAppendedStrings));
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    private MethodInvocation createAppendSubstring(ASTBuilder b, Expression lastExpr,
            Expression stringVar, Expression substringArg0, Expression substringArg1) {
        if (substringArg1 == null) {
            return b.invoke(lastExpr, "append", stringVar, substringArg0);
        }
        return b.invoke(lastExpr, "append", stringVar, substringArg0, substringArg1);
    }

    private boolean isRewriteNeeded(final LinkedList<Expression> allAppendedStrings, AtomicBoolean hasStringConcat) {
        final boolean res1 = filterOutEmptyStrings(allAppendedStrings);
        final boolean res2 = removeCallsToToString(allAppendedStrings);
        return res1 || res2 || hasStringConcat.get();
    }

    private boolean filterOutEmptyStrings(List<Expression> allExprs) {
        boolean result = false;
        for (Iterator<Expression> iter = allExprs.iterator(); iter.hasNext();) {
            Expression expr = iter.next();
            if (isEmptyString(expr)) {
                iter.remove();
                result = true;
            }
        }
        return result;
    }

    private boolean removeCallsToToString(List<Expression> allExprs) {
        boolean result = false;
        for (ListIterator<Expression> iter = allExprs.listIterator(); iter.hasNext();) {
            final Expression expr = iter.next();
            if (expr.getNodeType() == ASTNode.METHOD_INVOCATION) {
                final MethodInvocation mi = (MethodInvocation) expr;
                if (isMethod(mi, "java.lang.Object", "toString")) {
                    if (mi.getExpression() != null) {
                        iter.set(mi.getExpression());
                    } else {
                        iter.set(this.ctx.getAST().newThisExpression());
                    }
                    result = true;
                } else if (isMethod(mi, "java.lang.Boolean", "toString", "boolean")
                        || isMethod(mi, "java.lang.Byte", "toString", "byte")
                        || isMethod(mi, "java.lang.Character", "toString", "char")
                        || isMethod(mi, "java.lang.Short", "toString", "short")
                        || isMethod(mi, "java.lang.Integer", "toString", "int")
                        || isMethod(mi, "java.lang.Long", "toString", "long")
                        || isMethod(mi, "java.lang.Float", "toString", "float")
                        || isMethod(mi, "java.lang.Double", "toString", "double")) {
                    iter.set(arguments(mi).get(0));
                    result = true;
                }
            }
        }
        return result;
    }

    private boolean isStringValueOf(MethodInvocation mi) {
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

    private ASTNode createStringAppends(Expression lastExpr, List<Expression> appendedStrings) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        Expression result = b.copy(lastExpr);
        for (Expression expr : appendedStrings) {
            if (result == null) {
                result = b.copy(expr);
            } else {
                result = b.invoke(result, "append", b.copy(expr));
            }
        }
        return result;
    }

    private Expression createStringConcats(List<Expression> appendedStrings) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        if (appendedStrings.size() == 0) {
            return b.string("");
        } else if (appendedStrings.size() == 1) {
            final Expression expr = appendedStrings.get(0);
            if (hasType(expr, "java.lang.String")) {
                return b.copy(expr);
            }
            return b.invoke("String", "valueOf", b.copy(expr));
        }

        final Iterator<Expression> it = appendedStrings.iterator();
        final Expression arg0 = it.next();
        final Expression arg1 = it.next();

        final Expression expr1;
        if (hasType(arg0, "java.lang.String")
                || hasType(arg1, "java.lang.String")) {
            expr1 = b.copy(arg0);
        } else {
            expr1 = b.infixExpr(b.string(""), Operator.PLUS, b.copy(arg0));
        }
        final Expression expr2 = b.copy(arg1);
        final InfixExpression ie = b.infixExpr(expr1, Operator.PLUS, expr2);
        while (it.hasNext()) {
            extendedOperands(ie).add(b.copy(it.next()));
        }
        return ie;
    }

    private Expression collectAllAppendedStrings(Expression expr,
            final LinkedList<Expression> allOperands, AtomicBoolean hasStringConcat) {
        if (instanceOf(expr, "java.lang.Appendable")) {
            if (expr instanceof MethodInvocation) {
                final MethodInvocation mi = (MethodInvocation) expr;
                if ("append".equals(mi.getName().getIdentifier())
                        && arguments(mi).size() == 1) {
                    final Expression arg0 = arguments(mi).get(0);
                    addAllSubExpressions(arg0, allOperands, hasStringConcat);
                    return collectAllAppendedStrings(mi.getExpression(), allOperands, hasStringConcat);
                }
            } else if (expr instanceof ClassInstanceCreation) {
                final ClassInstanceCreation cic = (ClassInstanceCreation) expr;
                if (arguments(cic).size() == 1) {
                    final Expression arg0 = arguments(cic).get(0);
                    if (hasType(cic, "java.lang.StringBuffer", "java.lang.StringBuilder")
                        && (hasType(arg0, "java.lang.String")
                                || instanceOf(arg0, "java.lang.CharSequence"))) {
                        allOperands.addFirst(arg0);
                    }
                }
                return cic;
            } else if (expr instanceof Name || expr instanceof FieldAccess) {
                return expr;
            }
        }
        return null;
    }

    private void addAllSubExpressions(final Expression arg, final LinkedList<Expression> results,
            final AtomicBoolean hasStringConcat) {
        if (arg instanceof InfixExpression) {
            final InfixExpression ie = (InfixExpression) arg;
            if (hasType(ie, "java.lang.String")
                    && InfixExpression.Operator.PLUS.equals(ie.getOperator())) {
                if (ie.hasExtendedOperands()) {
                    final List<Expression> reversed = new ArrayList<Expression>(extendedOperands(ie));
                    Collections.reverse(reversed);
                    for (Expression op : reversed) {
                        addAllSubExpressions(op, results, hasStringConcat);
                    }
                }
                addAllSubExpressions(ie.getRightOperand(), results, hasStringConcat);
                addAllSubExpressions(ie.getLeftOperand(), results, hasStringConcat);
                if (hasStringConcat != null) {
                    hasStringConcat.set(true);
                }
                return;
            }
        }
        results.addFirst(arg);
    }
}

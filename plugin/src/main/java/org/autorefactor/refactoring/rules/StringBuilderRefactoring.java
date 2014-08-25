/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013 Jean-Noël Rouvignac - initial API and implementation
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

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.util.NotImplementedException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.CompilationUnit;
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
public class StringBuilderRefactoring extends ASTVisitor implements
        IJavaRefactoring {

    private static final class BooleanHolder {

        private boolean value;

        public BooleanHolder(boolean defaultValue) {
            this.value = defaultValue;
        }
    }

    private RefactoringContext ctx;
    private int javaMinorVersion;

    /** Default constructor. */
    public StringBuilderRefactoring() {
        super();
    }

    /** {@inheritDoc} */
    @Override
    public void setRefactoringContext(RefactoringContext ctx) {
        this.ctx = ctx;
        this.javaMinorVersion = this.ctx.getJavaSERelease().getMinorVersion();
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ClassInstanceCreation node) {
        final ITypeBinding typeBinding = node.getType().resolveBinding();
        if (this.javaMinorVersion >= 5 && typeBinding != null) {
            if ("java.lang.StringBuffer".equals(typeBinding.getQualifiedName())) {
                // TODO JNR replace with StringBuilder
                // check that the current method return type is not StringBuffer
                // do we need the CFG + live variable analysis first?
            }
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
            final BooleanHolder foundInfixExpr = new BooleanHolder(false);
            final Expression lastExpr = collectAllAppendedStrings(node, allAppendedStrings, foundInfixExpr);
            if (lastExpr instanceof Name || lastExpr instanceof FieldAccess) {
                boolean rewriteNeeded = filterOutEmptyStrings(allAppendedStrings);
                if (rewriteNeeded || foundInfixExpr.value) {
                    // rewrite the successive calls to append() on an Appendable
                    this.ctx.getRefactorings().replace(node,
                            createStringAppends(lastExpr, allAppendedStrings));
                    return DO_NOT_VISIT_SUBTREE;
                }
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
            final boolean substringWithOneArg =
                isMethod(embeddedMI, "java.lang.String", "substring", "int");
            final boolean substringWithTwoArgs =
                isMethod(embeddedMI, "java.lang.String", "substring", "int", "int")
                || isMethod(embeddedMI, "java.lang.CharSequence", "subSequence", "int", "int");
            if (substringWithOneArg || substringWithTwoArgs) {
                final ASTBuilder b = this.ctx.getASTBuilder();
                final Expression stringVar = b.copy(embeddedMI.getExpression());
                final List<Expression> args = arguments(embeddedMI);
                final Expression arg0 = b.copy(args.get(0));
                final Expression arg1 = substringWithTwoArgs ? b.copy(args.get(1)) : null;
                this.ctx.getRefactorings().replace(node,
                        createAppendSubstring(b, lastExpr, stringVar, arg0, arg1));
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
        Expression result = lastExpr;
        for (Expression expr : appendedStrings) {
            if (result == null) {
                result = expr;
            } else {
                final ASTBuilder b = this.ctx.getASTBuilder();
                result = b.invoke(result, "append", b.copy(expr));
            }
        }
        return result;
    }

    private Expression createStringConcats(List<Expression> appendedStrings) {
        if (appendedStrings.size() == 0) {
            throw new NotImplementedException("when there are no appended strings");
        } else if (appendedStrings.size() == 1) {
            return appendedStrings.get(0);
        }

        final Iterator<Expression> it = appendedStrings.iterator();
        final ASTBuilder b = this.ctx.getASTBuilder();
        final Expression expr1 = b.copySubtree(it.next());
        final Expression expr2 = b.copySubtree(it.next());
        final InfixExpression ie = b.infixExpr(expr1, Operator.PLUS, expr2);
        while (it.hasNext()) {
            extendedOperands(ie).add(b.copySubtree(it.next()));
        }
        return ie;
    }

    private Expression collectAllAppendedStrings(Expression expr,
            final LinkedList<Expression> allOperands, BooleanHolder foundInfixExpr) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        if (instanceOf(expr, "java.lang.Appendable")) {
            if (expr instanceof MethodInvocation) {
                final MethodInvocation mi = (MethodInvocation) expr;
                if ("append".equals(mi.getName().getIdentifier())
                        && arguments(mi).size() == 1) {
                    final Expression arg0 = arguments(mi).get(0);
                    addAllSubExpressions(arg0, allOperands, foundInfixExpr);
                    return collectAllAppendedStrings(mi.getExpression(), allOperands, foundInfixExpr);
                }
            } else if (expr instanceof ClassInstanceCreation) {
                final ClassInstanceCreation cic = (ClassInstanceCreation) expr;
                if (arguments(cic).size() == 1) {
                    final Expression arg0 = arguments(cic).get(0);
                    if (hasType(arg0, "java.lang.String")
                            || instanceOf(arg0, "java.lang.CharSequence")) {
                        allOperands.addFirst(b.copySubtree(arg0));
                    }
                }
                return b.copySubtree(cic);
            } else if (expr instanceof Name || expr instanceof FieldAccess) {
                return b.copySubtree(expr);
            }
        }
        return null;
    }

    private void addAllSubExpressions(final Expression arg, final LinkedList<Expression> results,
            final BooleanHolder foundInfixExpr) {
        if (arg instanceof InfixExpression) {
            final InfixExpression ie = (InfixExpression) arg;
            if (InfixExpression.Operator.PLUS.equals(ie.getOperator())) {
                if (ie.hasExtendedOperands()) {
                    final List<Expression> reversed = new ArrayList<Expression>(extendedOperands(ie));
                    Collections.reverse(reversed);
                    for (Expression op : reversed) {
                        addAllSubExpressions(op, results, foundInfixExpr);
                    }
                }
                addAllSubExpressions(ie.getRightOperand(), results, foundInfixExpr);
                addAllSubExpressions(ie.getLeftOperand(), results, foundInfixExpr);
                if (foundInfixExpr != null) {
                    foundInfixExpr.value = true;
                }
                return;
            }
        }
        results.addFirst(arg);
    }

    /** {@inheritDoc} */
    @Override
    public Refactorings getRefactorings(CompilationUnit astRoot) {
        astRoot.accept(this);
        return this.ctx.getRefactorings();
    }
}

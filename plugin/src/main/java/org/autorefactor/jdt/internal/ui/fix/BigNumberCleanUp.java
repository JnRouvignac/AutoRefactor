/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2018 Jean-Noël Rouvignac - initial API and implementation
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

import java.math.BigDecimal;
import java.math.BigInteger;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.JavaConstants;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.StringLiteral;

/** See {@link #getDescription()} method. */
public class BigNumberCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_BigNumberCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_BigNumberCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_BigNumberCleanUp_reason;
    }

    private int getJavaMinorVersion() {
        return ctx.getJavaProjectOptions().getJavaSERelease().getMinorVersion();
    }

    @Override
    public boolean visit(ClassInstanceCreation node) {
        final ITypeBinding typeBinding= node.getType().resolveBinding();

        if (ASTNodes.hasType(typeBinding, BigDecimal.class.getCanonicalName(), BigInteger.class.getCanonicalName()) && ASTNodes.arguments(node).size() == 1) {
            final Expression arg0= ASTNodes.arguments(node).get(0);

            if (arg0 instanceof NumberLiteral && ASTNodes.hasType(typeBinding, BigDecimal.class.getCanonicalName())) {
                final String token= ((NumberLiteral) arg0).getToken().replaceFirst("[lLfFdD]$", ""); //$NON-NLS-1$ $NON-NLS-2$

                if (token.contains(".")) { //$NON-NLS-1$
                    // Only instantiation from double, not from integer
                    ctx.getRefactorings().replace(arg0, getStringLiteral(token));
                    return false;
                }
                if (getJavaMinorVersion() >= 5) {
                    if (JavaConstants.ZERO_LONG_LITERAL_RE.matcher(token).matches()) {
                        return replaceWithQualifiedName(node, typeBinding, "ZERO"); //$NON-NLS-1$
                    }
                    if (JavaConstants.ONE_LONG_LITERAL_RE.matcher(token).matches()) {
                        return replaceWithQualifiedName(node, typeBinding, "ONE"); //$NON-NLS-1$
                    }
                    if (JavaConstants.TEN_LONG_LITERAL_RE.matcher(token).matches()) {
                        return replaceWithQualifiedName(node, typeBinding, "TEN"); //$NON-NLS-1$
                    }
                    ctx.getRefactorings().replace(node, getValueOf(typeBinding.getName(), token));
                    return false;
                }
            } else if (arg0 instanceof StringLiteral) {
                if (getJavaMinorVersion() < 5) {
                    return true;
                }

                final String literalValue= ((StringLiteral) arg0).getLiteralValue().replaceFirst("[lLfFdD]$", ""); //$NON-NLS-1$ $NON-NLS-2$

                if (literalValue.matches("0+")) { //$NON-NLS-1$
                    return replaceWithQualifiedName(node, typeBinding, "ZERO"); //$NON-NLS-1$
                }
                if (literalValue.matches("0+1")) { //$NON-NLS-1$
                    return replaceWithQualifiedName(node, typeBinding, "ONE"); //$NON-NLS-1$
                }
                if (literalValue.matches("0+10")) { //$NON-NLS-1$
                    return replaceWithQualifiedName(node, typeBinding, "TEN"); //$NON-NLS-1$
                }
                if (literalValue.matches("\\d+")) { //$NON-NLS-1$
                    this.ctx.getRefactorings().replace(node, getValueOf(typeBinding.getName(), literalValue));
                    return false;
                }
            }
        }

        return true;
    }

    private boolean replaceWithQualifiedName(ASTNode node, ITypeBinding typeBinding, String field) {
        this.ctx.getRefactorings().replace(node, this.ctx.getASTBuilder().name(typeBinding.getName(), field));
        return false;
    }

    private ASTNode getValueOf(String name, String numberLiteral) {
        final ASTNodeFactory b= this.ctx.getASTBuilder();
        return b.invoke(name, "valueOf", b.number(numberLiteral)); //$NON-NLS-1$
    }

    private StringLiteral getStringLiteral(String numberLiteral) {
        return this.ctx.getASTBuilder().string(numberLiteral);
    }

    @Override
    public boolean visit(PrefixExpression node) {
        final MethodInvocation mi= ASTNodes.as(node.getOperand(), MethodInvocation.class);
        return !ASTNodes.hasOperator(node, PrefixExpression.Operator.NOT) || mi == null || maybeReplaceEquals(false, node, mi);
    }

    @Override
    public boolean visit(MethodInvocation node) {
        if (node.getExpression() == null) {
            return true;
        }
        if (getJavaMinorVersion() >= 5 && (ASTNodes.usesGivenSignature(node, BigInteger.class.getCanonicalName(), "valueOf", long.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(node, BigDecimal.class.getCanonicalName(), "valueOf", long.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(node, BigDecimal.class.getCanonicalName(), "valueOf", double.class.getSimpleName()))) { //$NON-NLS-1$
            final ITypeBinding typeBinding= node.getExpression().resolveTypeBinding();
            final Expression arg0= ASTNodes.arg0(node);
            if (arg0 instanceof NumberLiteral) {
                final String token= ((NumberLiteral) arg0).getToken().replaceFirst("[lLfFdD]$", ""); //$NON-NLS-1$ $NON-NLS-2$
                if (token.contains(".") && ASTNodes.hasType(typeBinding, BigDecimal.class.getCanonicalName())) { //$NON-NLS-1$
                    this.ctx.getRefactorings().replace(node,
                            getClassInstanceCreatorNode((Name) node.getExpression(), token));
                } else if (JavaConstants.ZERO_LONG_LITERAL_RE.matcher(token).matches()) {
                    replaceWithQualifiedName(node, typeBinding, "ZERO"); //$NON-NLS-1$
                } else if (JavaConstants.ONE_LONG_LITERAL_RE.matcher(token).matches()) {
                    replaceWithQualifiedName(node, typeBinding, "ONE"); //$NON-NLS-1$
                } else if (JavaConstants.TEN_LONG_LITERAL_RE.matcher(token).matches()) {
                    replaceWithQualifiedName(node, typeBinding, "TEN"); //$NON-NLS-1$
                } else {
                    return true;
                }
                return false;
            }
        } else if (!(node.getParent() instanceof PrefixExpression)
                || !ASTNodes.hasOperator((PrefixExpression) node.getParent(), PrefixExpression.Operator.NOT)) {
            return maybeReplaceEquals(true, node, node);
        }
        return true;
    }

    private boolean maybeReplaceEquals(final boolean isPositive, final Expression node, final MethodInvocation mi) {
        if (ASTNodes.usesGivenSignature(mi, BigDecimal.class.getCanonicalName(), "equals", Object.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, BigInteger.class.getCanonicalName(), "equals", Object.class.getCanonicalName())) { //$NON-NLS-1$
            final Expression arg0= ASTNodes.arg0(mi);
            if (ASTNodes.hasType(arg0, BigDecimal.class.getCanonicalName(), BigInteger.class.getCanonicalName())) {
                if (isInStringAppend(mi.getParent())) {
                    final ASTNodeFactory b= this.ctx.getASTBuilder();
                    this.ctx.getRefactorings().replace(node, b.parenthesize(getCompareToNode(isPositive, mi)));
                } else {
                    this.ctx.getRefactorings().replace(node, getCompareToNode(isPositive, mi));
                }
                return false;
            }
        }
        return true;
    }

    private boolean isInStringAppend(final ASTNode node) {
        if (node instanceof InfixExpression) {
            final InfixExpression expression= (InfixExpression) node;

            if (ASTNodes.hasOperator(expression, InfixExpression.Operator.PLUS) || ASTNodes.hasType(expression.getLeftOperand(), String.class.getCanonicalName())
                    || ASTNodes.hasType(expression.getRightOperand(), String.class.getCanonicalName())) {
                return true;
            }
        }

        return false;
    }

    private ASTNode getClassInstanceCreatorNode(final Name className, final String numberLiteral) {
        final ASTNodeFactory b= this.ctx.getASTBuilder();
        return b.new0(className.getFullyQualifiedName(), b.string(numberLiteral));
    }

    private InfixExpression getCompareToNode(final boolean isPositive, final MethodInvocation node) {
        final ASTNodeFactory b= this.ctx.getASTBuilder();
        final MethodInvocation mi= b.invoke(b.copy(node.getExpression()), "compareTo", b.copy(ASTNodes.arg0(node))); //$NON-NLS-1$

        return b.infixExpression(mi, isPositive ? InfixExpression.Operator.EQUALS : InfixExpression.Operator.NOT_EQUALS, b.int0(0));
    }
}

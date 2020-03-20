package org.autorefactor.jdt.internal.corext.dom;

import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;

/**
 * TypedInfixExpression.
 *
 * @param <F> First operand
 * @param <S> Second operand
 */
public class TypedInfixExpression<F extends Expression, S extends Expression> {
    private F firstOperand;
    private InfixExpression.Operator operator;
    private S secondOperand;

    /**
     * TypedInfixExpression.
     *
     * @param firstOperand first operand
     * @param operator operator
     * @param secondOperand second operand
     */
    public TypedInfixExpression(final F firstOperand, InfixExpression.Operator operator, final S secondOperand) {
        this.firstOperand= firstOperand;
        this.operator= operator;
        this.secondOperand= secondOperand;
    }

    /**
     * GetFirstOperand.
     *
     * @return GetFirstOperand.
     */
    public F getFirstOperand() {
        return firstOperand;
    }

    /**
     * GetOperator.
     *
     * @return GetOperator.
     */
    public InfixExpression.Operator getOperator() {
        return operator;
    }

    /**
     * GetSecondOperand.
     *
     * @return GetSecondOperand.
     */
    public S getSecondOperand() {
        return secondOperand;
    }
}

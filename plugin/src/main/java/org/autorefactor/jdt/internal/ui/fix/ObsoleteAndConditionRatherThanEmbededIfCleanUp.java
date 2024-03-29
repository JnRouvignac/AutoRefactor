/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2015 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Make sure we do not visit again modified nodes
 * Copyright (C) 2019 Fabrice Tiercelin - Add parenthesis when it's needed
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

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class ObsoleteAndConditionRatherThanEmbededIfCleanUp extends AbstractCleanUpRule {
    @Override
    public String getName() {
        return MultiFixMessages.ObsoleteAndConditionRatherThanEmbededIfCleanUp_name;
    }

    @Override
    public String getDescription() {
        return MultiFixMessages.ObsoleteAndConditionRatherThanEmbededIfCleanUp_description;
    }

    @Override
    public String getReason() {
        return MultiFixMessages.ObsoleteAndConditionRatherThanEmbededIfCleanUp_reason;
    }

    @Override
    public boolean visit(final IfStatement node) {
        if (node.getElseStatement() == null) {
            IfStatement innerIf= ASTNodes.as(node.getThenStatement(), IfStatement.class);

            if (innerIf != null
                    && innerIf.getElseStatement() == null
                    && ASTNodes.getNbOperands(node.getExpression()) + ASTNodes.getNbOperands(innerIf.getExpression()) < ASTNodes.EXCESSIVE_OPERAND_NUMBER) {
                replaceIfNoElseStatement(node, innerIf);
                return false;
            }
        }

        return true;
    }

    private void replaceIfNoElseStatement(final IfStatement node, final IfStatement innerIf) {
        ASTRewrite rewrite= cuRewrite.getASTRewrite();
        ASTNodeFactory ast= cuRewrite.getASTBuilder();
        TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteAndConditionRatherThanEmbededIfCleanUp_description);

		if (node.getThenStatement() instanceof Block || innerIf.getThenStatement() instanceof Block) {
			InfixExpression newInfixExpression= ast.newInfixExpression();
			newInfixExpression.setLeftOperand(ASTNodeFactory.parenthesizeIfNeeded(ast, ASTNodes.createMoveTarget(rewrite, node.getExpression())));
			newInfixExpression.setOperator(InfixExpression.Operator.CONDITIONAL_AND);
			newInfixExpression.setRightOperand(ASTNodeFactory.parenthesizeIfNeeded(ast, ASTNodes.createMoveTarget(rewrite, innerIf.getExpression())));

			ASTNodes.replaceButKeepComment(rewrite, innerIf.getExpression(), newInfixExpression, group);
	        ASTNodes.replaceButKeepComment(rewrite, node, ASTNodes.createMoveTarget(rewrite, innerIf), group);
		} else {
			// Do not do the cleanup
			// Prepare the code for the next pass
			Block newBlock= ast.newBlock();
			newBlock.statements().add(ASTNodes.createMoveTarget(rewrite, innerIf));

			ASTNodes.replaceButKeepComment(rewrite, innerIf, newBlock, group);
		}
    }
}

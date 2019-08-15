/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Split the code
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

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.eclipse.jdt.core.dom.WhileStatement;

/**
 * TODO Use variable values analysis for determining where code is dead.
 *
 * @see #getDescription()
 */
public class DoWhileRatherThanWhileCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_DoWhileRatherThanWhileCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_DoWhileRatherThanWhileCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_DoWhileRatherThanWhileCleanUp_reason;
    }

    @Override
    public boolean visit(WhileStatement node) {
        final Object constantCondition= node.getExpression().resolveConstantExpressionValue();
        if (Boolean.TRUE.equals(constantCondition)) {
            ASTNodeFactory b= this.ctx.getASTBuilder();
            this.ctx.getRefactorings().replace(node, b.doWhile(b.copy(node.getExpression()), b.copy(node.getBody())));
            return false;
        }
        return true;
    }
}

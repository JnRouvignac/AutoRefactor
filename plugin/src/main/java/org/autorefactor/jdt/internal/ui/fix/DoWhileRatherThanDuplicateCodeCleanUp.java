/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Initial API and implementation
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

import java.util.ArrayList;
import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.ASTSemanticMatcher;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.WhileStatement;

/** See {@link #getDescription()} method. */
public class DoWhileRatherThanDuplicateCodeCleanUp extends AbstractCleanUpRule {
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
        final List<Statement> whileStatements= ASTNodes.asList(node.getBody());

        if (whileStatements.isEmpty()) {
            return true;
        }

        final List<Statement> previousStatements= new ArrayList<>(whileStatements.size());
        final ASTSemanticMatcher matcher= new ASTSemanticMatcher();

        Statement previousStatement= ASTNodes.getPreviousSibling(node);
        int i= whileStatements.size() - 1;
        while (i >= 0) {
            if (previousStatement == null || !ASTNodes.match(matcher, previousStatement, whileStatements.get(i))) {
                return true;
            }
            i--;
            previousStatements.add(previousStatement);
            previousStatement= ASTNodes.getPreviousSibling(previousStatement);
        }

        replaceWithDoWhile(node, previousStatements);
        return false;
    }

    private void replaceWithDoWhile(final WhileStatement node, final List<Statement> previousStatements) {
        final Refactorings r= this.ctx.getRefactorings();
        r.remove(previousStatements);

        final ASTNodeFactory b= this.ctx.getASTBuilder();
        r.replace(node, b.doWhile(b.copy(node.getExpression()), b.copy(node.getBody())));
    }
}

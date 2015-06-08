/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2015 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.MemberValuePair;
import org.eclipse.jdt.core.dom.NormalAnnotation;

import static org.autorefactor.refactoring.ASTHelper.*;

/** See {@link #getDescription()} method. */
@SuppressWarnings("javadoc")
public class AnnotationRefactoring extends AbstractRefactoringRule {

    @Override
    public String getDescription() {
        return ""
            + "Simplifies annotation uses:\n"
            + "- empty parentheses will be removed from annotations,\n"
            + "- single members named \"value\" will be removed from annotations and only the value will be left.";
    }

    @Override
    public String getName() {
        return "Annotation";
    }

    @Override
    public boolean visit(NormalAnnotation node) {
        final Refactorings r = this.ctx.getRefactorings();
        final ASTBuilder b = this.ctx.getASTBuilder();
        final List<MemberValuePair> values = values(node);
        if (values.isEmpty()) {
            r.replace(node, b.markerAnnotation(b.move(node.getTypeName())));
            return DO_NOT_VISIT_SUBTREE;
        } else if (values.size() == 1) {
            MemberValuePair pair = values.get(0);
            if ("value".equals(pair.getName().getIdentifier())) {
                r.replace(node,
                        b.singleValueAnnotation(b.move(node.getTypeName()), b.move(pair.getValue())));
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

}

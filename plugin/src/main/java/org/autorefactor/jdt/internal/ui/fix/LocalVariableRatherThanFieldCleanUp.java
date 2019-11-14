/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016-2017 Fabrice Tiercelin - initial API and implementation
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
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Dimension;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.IExtendedModifier;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/** See {@link #getDescription()} method. */
public class LocalVariableRatherThanFieldCleanUp extends AbstractCleanUpRule {
    private final class FieldUseVisitor extends ASTVisitor {
        private final SimpleName field;
        private final List<SimpleName> occurrences= new ArrayList<>();

        private FieldUseVisitor(final SimpleName field) {
            this.field= field;
        }

        @Override
        public boolean visit(final SimpleName aVariable) {
            if (field != aVariable
                    && field.getIdentifier().equals(aVariable.getIdentifier())
                    && !(aVariable.getParent() instanceof MethodDeclaration)
                    && (!(aVariable.getParent() instanceof MethodInvocation) || aVariable.getLocationInParent() != MethodInvocation.NAME_PROPERTY)) {
                occurrences.add(aVariable);
            }

            return true;
        }

        private List<SimpleName> getOccurrences() {
            return occurrences;
        }
    }

    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_LocalVariableRatherThanFieldCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_LocalVariableRatherThanFieldCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_LocalVariableRatherThanFieldCleanUp_reason;
    }

    @Override
    public boolean visit(TypeDeclaration node) {
        for (FieldDeclaration field : node.getFields()) {
            if (!maybeReplaceFieldByLocalVariable(node, field)) {
                return false;
            }
        }

        return true;
    }

    private boolean maybeReplaceFieldByLocalVariable(TypeDeclaration node, FieldDeclaration field) {
        if (Modifier.isPrivate(field.getModifiers()) && !Modifier.isFinal(field.getModifiers()) && !hasAnnotation(field) && field.getType().isPrimitiveType()) {
            for (Object object : field.fragments()) {
                VariableDeclarationFragment fragment= (VariableDeclarationFragment) object;

                if (!maybeReplaceFragmentByLocalVariable(node, field, fragment)) {
                    return false;
                }
            }
        }

        return true;
    }

    private boolean maybeReplaceFragmentByLocalVariable(TypeDeclaration node, FieldDeclaration field,
            VariableDeclarationFragment fragment) {
        if (fragment.getInitializer() != null && !ASTNodes.isPassive(fragment.getInitializer())) {
            return true;
        }

        FieldUseVisitor fieldUseVisitor= new FieldUseVisitor(fragment.getName());
        node.accept(fieldUseVisitor);
        List<SimpleName> occurrences= fieldUseVisitor.getOccurrences();

        MethodDeclaration oneMethodDeclaration= null;

        for (SimpleName occurrence : occurrences) {
            MethodDeclaration currentMethodDeclaration= ASTNodes.getAncestorOrNull(occurrence, MethodDeclaration.class);

            if (isVariableDeclaration(occurrence)
                    || isExternalField(occurrence)
                    || currentMethodDeclaration == null
                    || oneMethodDeclaration != null && currentMethodDeclaration != oneMethodDeclaration) {
                return true;
            }

            oneMethodDeclaration= currentMethodDeclaration;
        }

        if (oneMethodDeclaration == null) {
            return true;
        }

        boolean isReassigned= isAlwaysErased(occurrences);

        if (isReassigned) {
            SimpleName reassignment= findReassignment(occurrences);

            if (reassignment != null && reassignment.getParent() instanceof Assignment) {
                replaceFieldByLocalVariable(field, fragment, reassignment);
                return false;
            }
        }

        return true;
    }

    private void replaceFieldByLocalVariable(FieldDeclaration field, VariableDeclarationFragment fragment, SimpleName reassignment) {
        final ASTNodeFactory b= this.ctx.getASTBuilder();
        final Refactorings r= this.ctx.getRefactorings();

        boolean isFieldKept= field.fragments().size() != 1;

        Assignment reassignmentAssignment= (Assignment) reassignment.getParent();
        final VariableDeclarationFragment newFragment= b.declareFragment(b.move(reassignment), b.move(reassignmentAssignment.getRightHandSide()));
        @SuppressWarnings("unchecked")
        List<Dimension> extraDimensions= fragment.extraDimensions();
        @SuppressWarnings("unchecked")
        List<Dimension> newExtraDimensions= newFragment.extraDimensions();
        newExtraDimensions.addAll(b.move(extraDimensions));
        VariableDeclarationStatement newDeclareStatement= b.declareStatement(isFieldKept ? b.move(field.getType()) : b.copy(field.getType()), newFragment);
        @SuppressWarnings("unchecked")
        List<IExtendedModifier> modifiers= field.modifiers();
        @SuppressWarnings("unchecked")
        List<IExtendedModifier> newModifiers= newDeclareStatement.modifiers();

        for (IExtendedModifier iExtendedModifier : modifiers) {
            Modifier modifier= (Modifier) iExtendedModifier;

            if (!modifier.isPrivate() && !modifier.isStatic()) {
                newModifiers.add(isFieldKept ? b.move(modifier) : b.copy(modifier));
            }
        }

        r.replace(ASTNodes.getAncestor(reassignmentAssignment, Statement.class),
                newDeclareStatement);

        if (isFieldKept) {
            r.remove(fragment);
            r.replace(field.getType(), b.copy(field.getType()));
        } else {
            r.remove(field);
        }
    }

    private SimpleName findReassignment(List<SimpleName> occurrences) {
        for (SimpleName reassignment : occurrences) {
            if (isReassigned(reassignment) && isReassignmentForAll(reassignment, occurrences)) {
                return reassignment;
            }
        }

        return null;
    }

    private boolean isReassignmentForAll(SimpleName reassignment, List<SimpleName> occurrences) {
        for (SimpleName occurrence : occurrences) {
            if (reassignment != occurrence) {
                Statement statement= ASTNodes.getAncestorOrNull(occurrence, Statement.class);
                boolean isReassigned= false;

                while (statement != null) {
                    ExpressionStatement expressionStatement= ASTNodes.as(statement, ExpressionStatement.class);

                    if (expressionStatement != null) {
                        Assignment assignment= ASTNodes.as(expressionStatement.getExpression(), Assignment.class);

                        if (assignment != null
                                && ASTNodes.hasOperator(assignment, Assignment.Operator.ASSIGN)
                                && assignment.getLeftHandSide() == reassignment) {
                            isReassigned= true;
                            break;
                        }
                    }

                    statement= ASTNodes.getPreviousStatement(statement);
                }

                if (!isReassigned) {
                    return false;
                }
            }
        }

        return true;
    }

    private boolean isAlwaysErased(List<SimpleName> occurrences) {
        for (SimpleName occurrence : occurrences) {
            if (!isReassigned(occurrence)) {
                Statement statement= ASTNodes.getAncestorOrNull(occurrence, Statement.class);
                boolean isReassigned= false;

                while (statement != null) {
                    statement= ASTNodes.getPreviousStatement(statement);
                    ExpressionStatement expressionStatement= ASTNodes.as(statement, ExpressionStatement.class);

                    if (expressionStatement != null) {
                        Assignment assignment= ASTNodes.as(expressionStatement.getExpression(), Assignment.class);

                        if (assignment != null
                                && ASTNodes.hasOperator(assignment, Assignment.Operator.ASSIGN)
                                && ASTNodes.areSameVariables(assignment.getLeftHandSide(), occurrence)) {
                            isReassigned= true;
                            break;
                        }
                    }
                }

                if (!isReassigned) {
                    return false;
                }
            }
        }

        return true;
    }

    private boolean isReassigned(SimpleName occurrence) {
        return occurrence.getParent() instanceof Assignment
                && occurrence.getLocationInParent() == Assignment.LEFT_HAND_SIDE_PROPERTY
                && ASTNodes.hasOperator((Assignment) occurrence.getParent(), Assignment.Operator.ASSIGN);
    }

    private static boolean isExternalField(SimpleName occurrence) {
        FieldAccess fieldAccess= ASTNodes.as(occurrence, FieldAccess.class);

        if (fieldAccess != null && fieldAccess.getExpression() instanceof ThisExpression) {
            return true;
        }

        QualifiedName qualifiedName= ASTNodes.as(occurrence, QualifiedName.class);

        return qualifiedName != null;
    }

    private static boolean isVariableDeclaration(SimpleName occurrence) {
        switch (occurrence.getParent().getNodeType()) {
        case ASTNode.SINGLE_VARIABLE_DECLARATION:
        case ASTNode.VARIABLE_DECLARATION_STATEMENT:
            return occurrence.getLocationInParent() == SingleVariableDeclaration.NAME_PROPERTY;

        case ASTNode.VARIABLE_DECLARATION_EXPRESSION:
            return occurrence.getLocationInParent() == VariableDeclarationExpression.FRAGMENTS_PROPERTY;

        case ASTNode.VARIABLE_DECLARATION_FRAGMENT:
            return occurrence.getLocationInParent() == VariableDeclarationFragment.NAME_PROPERTY;

        default:
            return false;
        }
    }

    private boolean hasAnnotation(FieldDeclaration field) {
        @SuppressWarnings("unchecked")
        final List<IExtendedModifier> modifiers= field.modifiers();

        for (IExtendedModifier em : modifiers) {
            if (em.isAnnotation()) {
                return true;
            }
        }

        return false;
    }
}
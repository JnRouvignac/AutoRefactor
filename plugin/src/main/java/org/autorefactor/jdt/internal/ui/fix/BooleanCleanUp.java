/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2015 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016-2017 Fabrice Tiercelin - Make sure we do not visit again modified nodes
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

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.ASTSemanticMatcher;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.dom.VarDefinitionsUsesVisitor;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BooleanLiteral;
import org.eclipse.jdt.core.dom.ChildListPropertyDescriptor;
import org.eclipse.jdt.core.dom.ChildPropertyDescriptor;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.LambdaExpression;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.StructuralPropertyDescriptor;
import org.eclipse.jdt.core.dom.SuperFieldAccess;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class BooleanCleanUp extends AbstractCleanUpRule {
    @Override
    public String getName() {
        return MultiFixMessages.BooleanCleanUp_name;
    }

    @Override
    public String getDescription() {
        return MultiFixMessages.BooleanCleanUp_description;
    }

    @Override
    public String getReason() {
        return MultiFixMessages.BooleanCleanUp_reason;
    }

    private static class BooleanASTMatcher extends ASTSemanticMatcher {
        /** else node to then node. */
        final Map<ASTNode, ASTNode> matches= new HashMap<>();
        final Map<ASTNode, ASTNode> previousMatches;

        public BooleanASTMatcher(final Map<ASTNode, ASTNode> previousMatches) {
            if (previousMatches != null) {
                this.previousMatches= previousMatches;
            } else {
                this.previousMatches= Collections.emptyMap();
            }
        }

        @Override
        public boolean match(final BooleanLiteral node, final Object other) {
            if (other instanceof Expression) {
                Expression expression= (Expression) other;

                if (areNegatedBooleanValues(node, expression)) {
                    matches.put(expression, node);
                    return true;
                }
            }

            return super.match(node, other);
        }

        @Override
        public boolean match(final QualifiedName node, final Object other) {
            if (other instanceof Expression) {
                Expression expression= (Expression) other;

                if (this.previousMatches.containsKey(other) || areNegatedBooleanValues(node, expression)) {
                    matches.put(expression, node);
                    return true;
                }
            }

            return super.match(node, other);
        }
    }

    private final class AssignmentIfAndReturnVisitor extends BlockSubVisitor {
        @Override
        public boolean visit(final IfStatement visited) {
            if (result) {
                Expression ifCondition= visited.getExpression();
                BooleanASTMatcher matcher= new BooleanASTMatcher(null);

                if (ASTNodes.isPassive(ifCondition)
                		&& ASTNodes.match(matcher, visited.getThenStatement(), visited.getElseStatement())
                		&& (matcher.matches.size() <= 1 || ifCondition instanceof Name || ifCondition instanceof FieldAccess || ifCondition instanceof SuperFieldAccess)) {
                    ASTNodeFactory ast= cuRewrite.getASTBuilder();
                	// Then and else statements are matching, bar the boolean values
                	// which are opposite
                	Statement copyStatement= ast.copySubtree(visited.getThenStatement());
                	// Identify the node that needs to be replaced after the copy
                	BooleanASTMatcher matcher2= new BooleanASTMatcher(matcher.matches);

                	if (ASTNodes.match(matcher2, copyStatement, visited.getElseStatement())) {
                		ASTRewrite rewrite = cuRewrite.getASTRewrite();
                		TextEditGroup group= new TextEditGroup(MultiFixMessages.BooleanCleanUp_description);

                		copyStatement.accept(
                				new BooleanReplaceVisitor(ifCondition, matcher2.matches.values(), visited));

                		if (!ASTNodes.canHaveSiblings(visited)) {
                			// Make sure to keep curly braces if the node is an else statement
                			ASTNodes.replaceButKeepComment(rewrite, visited, copyStatement, group);
                        	result= false;
                			return false;
                		}

                		if (!ASTNodes.hasVariableConflict(visited, visited.getThenStatement())) {
                			List<Statement> statementsToMove= ASTNodes.asList(copyStatement);

                			if (statementsToMove.isEmpty()) {
                				rewrite.remove(visited, group);
                			} else {
                    			for (int i= statementsToMove.size() - 1; i > 0; i--) {
                    				rewrite.insertAfter(statementsToMove.get(i), visited, group);
                    			}

                    			ASTNodes.replaceButKeepComment(rewrite, visited, statementsToMove.get(0), group);
                			}

                        	result= false;
                			return false;
                		}
                	}
                }

                ReturnStatement thenReturnStatement= ASTNodes.as(visited.getThenStatement(), ReturnStatement.class);

                if (thenReturnStatement != null) {
                    ReturnStatement elseReturnStatement= ASTNodes.as(
                            visited.getElseStatement() != null ? visited.getElseStatement() : ASTNodes.getNextSibling(visited),
                                    ReturnStatement.class);

                    if (elseReturnStatement != null && !withThenReturnStatement(visited, thenReturnStatement, elseReturnStatement)) {
                    	result= false;
                    }

                    return result;
                }

                if (!noThenReturnStatement(visited)) {
                	result= false;
                }
            }

            return result;
        }
    }

    @Override
    public boolean visit(final Block visited) {
        AssignmentIfAndReturnVisitor assignmentIfAndReturnVisitor= new AssignmentIfAndReturnVisitor();
        assignmentIfAndReturnVisitor.visitNode(visited);
        return assignmentIfAndReturnVisitor.result;
    }

    /**
     * Compares mixed boolean literal and Boolean object values against each other.
     */
    private static boolean areNegatedBooleanValues(final Expression booleanExpression1, final Expression booleanExpression2) {
        Boolean booleanLiteral1= ASTNodes.getBooleanLiteral(booleanExpression1);
        Boolean booleanLiteral2= ASTNodes.getBooleanLiteral(booleanExpression2);
        return booleanLiteral1 != null
        		&& booleanLiteral2 != null
        		&& booleanLiteral1 ^ booleanLiteral2;
    }

    private class BooleanReplaceVisitor extends ASTVisitor {
        private final Expression ifCondition;
        private final Collection<ASTNode> nodesToReplace;
        private final ASTNode context;

        public BooleanReplaceVisitor(final Expression ifCondition, final Collection<ASTNode> nodesToReplace, final ASTNode context) {
            this.ifCondition= ifCondition;
            this.nodesToReplace= nodesToReplace;
            this.context= context;
        }

        @Override
        public boolean visit(final BooleanLiteral visited) {
			return visitBoolean(visited, boolean.class.getSimpleName());
        }

        @Override
        public boolean visit(final QualifiedName visited) {
			return visitBoolean(visited, Boolean.class.getCanonicalName());
        }

		private boolean visitBoolean(final Expression visited, final String className) {
			if (nodesToReplace.contains(visited)) {
			    Boolean booleanValue= ASTNodes.getBooleanLiteral(visited);

			    if (booleanValue != null) {
					Expression expression= getExpression(signExpression(ifCondition, booleanValue), className, context);
			        replaceInParent(visited, expression);
			    }
			}

			return false;
		}

		private void replaceInParent(final ASTNode nodeToReplace, final ASTNode replacementNode) {
            StructuralPropertyDescriptor locationInParent= nodeToReplace.getLocationInParent();

            if (locationInParent instanceof ChildPropertyDescriptor) {
                ChildPropertyDescriptor childPropertyDescriptor= (ChildPropertyDescriptor) locationInParent;
                nodeToReplace.getParent().setStructuralProperty(childPropertyDescriptor, replacementNode);
            } else if (locationInParent instanceof ChildListPropertyDescriptor) {
                ChildListPropertyDescriptor childListPropertyDescriptor= (ChildListPropertyDescriptor) locationInParent;
                List<ASTNode> property= (List<ASTNode>) nodeToReplace.getParent().getStructuralProperty(childListPropertyDescriptor);
                property.set(property.indexOf(nodeToReplace), replacementNode);
            }
        }
    }

    @Override
    public boolean visit(final ConditionalExpression visited) {
        ITypeBinding typeBinding= visited.resolveTypeBinding();

        if (typeBinding != null) {
            Expression newExpression= newExpressionOrNull(typeBinding, visited.getExpression(),
                    visited.getThenExpression(), visited.getElseExpression());

            if (newExpression != null) {
                TextEditGroup group= new TextEditGroup(MultiFixMessages.BooleanCleanUp_description);
                ASTRewrite rewrite= cuRewrite.getASTRewrite();

                ASTNodes.replaceButKeepComment(rewrite, visited, newExpression, group);
                return false;
            }
        }

        return true;
    }

    private boolean withThenReturnStatement(final IfStatement visited, final ReturnStatement thenReturnStatement, final ReturnStatement elseReturnStatement) {
    	ASTNode callable= ASTNodes.getFirstAncestorOrNull(visited, MethodDeclaration.class, TypeDeclaration.class, LambdaExpression.class);

        if (callable instanceof MethodDeclaration) {
        	MethodDeclaration methodDeclaration= (MethodDeclaration) callable;
            ReturnStatement newReturnStatement= getReturnStatement(visited, thenReturnStatement.getExpression(), elseReturnStatement.getExpression(), methodDeclaration);

            if (newReturnStatement != null) {
                replaceExpression(visited, newReturnStatement, elseReturnStatement);
                return false;
            }
        }

        return true;
    }

    private ReturnStatement getReturnStatement(final IfStatement visited, final Expression thenExpression,
            final Expression elseExpression, final MethodDeclaration methodDeclaration) {
        ASTNodeFactory ast= cuRewrite.getASTBuilder();

        if (areNegatedBooleanValues(thenExpression, elseExpression)) {
            Expression returnExpression= getReturnExpression(methodDeclaration, signExpression(visited.getExpression(), !ASTNodes.getBooleanLiteral(elseExpression)));

            if (returnExpression != null) {
                return ast.newReturnStatement(returnExpression);
            }
        }

        Type returnType= methodDeclaration.getReturnType2();

        if (returnType != null && ASTNodes.hasType(returnType.resolveBinding(), boolean.class.getCanonicalName())) {
            Boolean thenBoolean= ASTNodes.getBooleanLiteral(thenExpression);
            Boolean elseBoolean= ASTNodes.getBooleanLiteral(elseExpression);

            if (thenBoolean == null && elseBoolean != null) {
                Expression leftOperand= ASTNodeFactory.parenthesizeIfNeeded(ast, signExpression(visited.getExpression(), !elseBoolean));

    			InfixExpression newInfixExpression= ast.newInfixExpression();
    			newInfixExpression.setLeftOperand(leftOperand);
    			newInfixExpression.setOperator(getConditionalOperator(elseBoolean));
    			newInfixExpression.setRightOperand(ASTNodeFactory.parenthesizeIfNeeded(ast, ast.createCopyTarget(thenExpression)));
    			return ast.newReturnStatement(newInfixExpression);
            }

            if (thenBoolean != null && elseBoolean == null) {
                Expression leftOperand= ASTNodeFactory.parenthesizeIfNeeded(ast, signExpression(visited.getExpression(), thenBoolean));

    			InfixExpression newInfixExpression= ast.newInfixExpression();
    			newInfixExpression.setLeftOperand(leftOperand);
    			newInfixExpression.setOperator(getConditionalOperator(thenBoolean));
    			newInfixExpression.setRightOperand(ASTNodeFactory.parenthesizeIfNeeded(ast, ast.createCopyTarget(elseExpression)));
    			return ast.newReturnStatement(newInfixExpression);
            }
        }

        return null;
    }

    private boolean noThenReturnStatement(final IfStatement visited) {
        Assignment thenAssignment= ASTNodes.asExpression(visited.getThenStatement(), Assignment.class);

        if (ASTNodes.hasOperator(thenAssignment, Assignment.Operator.ASSIGN)
        		&& ASTNodes.asList(visited.getElseStatement()).isEmpty()
                && (thenAssignment.getLeftHandSide() instanceof Name || thenAssignment.getLeftHandSide() instanceof FieldAccess || thenAssignment.getLeftHandSide() instanceof SuperFieldAccess)) {
            Statement previousSibling= ASTNodes.getPreviousSibling(visited);

            if (previousSibling instanceof VariableDeclarationStatement) {
                VariableDeclarationStatement variableDeclarationStatement= (VariableDeclarationStatement) previousSibling;
                VariableDeclarationFragment fragment= getVariableDeclarationFragment(variableDeclarationStatement, thenAssignment.getLeftHandSide());

                if (fragment != null) {
                    VarDefinitionsUsesVisitor variableUseVisitor;
					try {
						variableUseVisitor= new VarDefinitionsUsesVisitor(fragment.resolveBinding(), visited.getExpression(), true);
					} catch (Exception e) {
				        return true;
					}

                    if (variableUseVisitor.getReads().isEmpty()) {
                        ITypeBinding typeBinding= variableDeclarationStatement.getType().resolveBinding();
                        return maybeReplace(visited, thenAssignment, typeBinding, fragment.getInitializer());
                    }
                }
            } else if (previousSibling instanceof ExpressionStatement) {
                Assignment elseAssignment= ASTNodes.asExpression(previousSibling, Assignment.class);

                if (ASTNodes.hasOperator(elseAssignment, Assignment.Operator.ASSIGN)
                		&& ASTNodes.isSameVariable(thenAssignment.getLeftHandSide(), elseAssignment.getLeftHandSide())) {
                    ITypeBinding typeBinding= elseAssignment.resolveTypeBinding();
                    return maybeReplace(visited, thenAssignment, typeBinding, elseAssignment.getRightHandSide());
                }
            }
        }

        return true;
    }

    private boolean maybeReplace(final IfStatement visited, final Assignment assignment, final ITypeBinding typeBinding, final Expression rightHandSide) {
        if (typeBinding != null) {
            Expression newExpression= newExpressionOrNull(typeBinding, visited.getExpression(), assignment.getRightHandSide(),
                    rightHandSide);

            if (newExpression != null) {
                replaceExpression(rightHandSide, newExpression, visited);
                return false;
            }
        }

        return true;
    }

	private void replaceExpression(final ASTNode originalNode, final ASTNode newNode,
			final ASTNode nodeToRemove) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.BooleanCleanUp_description);

		ASTNodes.replaceButKeepComment(rewrite, originalNode, newNode, group);
		rewrite.remove(nodeToRemove, group);
	}

    private InfixExpression.Operator getConditionalOperator(final boolean isOrOperator) {
        return isOrOperator ? InfixExpression.Operator.CONDITIONAL_OR : InfixExpression.Operator.CONDITIONAL_AND;
    }

    private Expression signExpression(final Expression booleanExpression, final boolean isPositive) {
        ASTNodeFactory ast= cuRewrite.getASTBuilder();

        if (isPositive) {
            return ast.createCopyTarget(booleanExpression);
        }

        return ast.negate(booleanExpression, false);
    }

    private VariableDeclarationFragment getVariableDeclarationFragment(final VariableDeclarationStatement variableDeclarationStatement,
            final Expression expression) {
        if (variableDeclarationStatement == null) {
            return null;
        }

        for (VariableDeclarationFragment fragment : (List<VariableDeclarationFragment>) variableDeclarationStatement.fragments()) {
            if (ASTNodes.isSameVariable(expression, fragment)) {
                return fragment;
            }
        }

        return null;
    }

    private Expression getReturnExpression(final MethodDeclaration methodDeclaration, final Expression ifCondition) {
        IMethodBinding methodBinding= methodDeclaration.resolveBinding();

        if (methodBinding == null) {
            return null;
        }

        String qualifiedName= methodBinding.getReturnType().getQualifiedName();
        return getExpression(ifCondition, qualifiedName, methodDeclaration);
    }

    private Expression newExpressionOrNull(final ITypeBinding typeBinding, final Expression condition, final Expression thenExpression,
            final Expression elseExpression) {
        Boolean thenLiteral= ASTNodes.getBooleanLiteral(thenExpression);
        Boolean elseLiteral= ASTNodes.getBooleanLiteral(elseExpression);

        if (areNegatedBooleanValues(thenExpression, elseExpression)) {
            return getExpression(signExpression(condition, thenLiteral), typeBinding.getQualifiedName(), condition);
        }

        if ((ASTNodes.isPrimitive(thenExpression) || ASTNodes.isPrimitive(elseExpression))
                && ASTNodes.hasType(typeBinding, boolean.class.getCanonicalName(), Boolean.class.getCanonicalName())) {
            ASTNodeFactory ast= cuRewrite.getASTBuilder();

            // If both expressions are primitive, there cannot be any NPE
            // If only one expression is primitive, a NPE is already possible so we do not
            // care
            if (thenLiteral != null && elseLiteral == null) {
                InfixExpression newInfixExpression= ast.newInfixExpression();
				newInfixExpression.setLeftOperand(signExpression(condition, thenLiteral));
				newInfixExpression.setOperator(getConditionalOperator(thenLiteral));
				newInfixExpression.setRightOperand(ast.createCopyTarget(elseExpression));
				return newInfixExpression;
            }

            if (thenLiteral == null && elseLiteral != null) {
                InfixExpression newInfixExpression= ast.newInfixExpression();
				newInfixExpression.setLeftOperand(signExpression(condition, !elseLiteral));
				newInfixExpression.setOperator(getConditionalOperator(elseLiteral));
				newInfixExpression.setRightOperand(ast.createCopyTarget(thenExpression));
				return newInfixExpression;
            }
        }

        return null;
    }

    private Expression getExpression(final Expression condition, final String expressionTypeName, final ASTNode context) {
    	Name newBooleanName= getBooleanName(context);

        if (boolean.class.getSimpleName().equals(expressionTypeName)) {
            return condition;
        }

        if (Boolean.class.getCanonicalName().equals(expressionTypeName)) {
            ASTNodeFactory ast= cuRewrite.getASTBuilder();

            if (getJavaMinorVersion() >= 4) {
                MethodInvocation valueOfMethod= ast.newMethodInvocation();
				valueOfMethod.setExpression(newBooleanName);
				valueOfMethod.setName(ast.newSimpleName("valueOf")); //$NON-NLS-1$
				valueOfMethod.arguments().add(condition);
				return valueOfMethod;
            }

            return ast.newClassInstanceCreation(expressionTypeName, condition);
        }

        return null;
    }

    private Name getBooleanName(final ASTNode node) {
        ASTNodeFactory ast= cuRewrite.getASTBuilder();
        CompilationUnit compilationUnit= ASTNodes.getTypedAncestor(node, CompilationUnit.class);

		if (compilationUnit != null && !isSimpleNameAlreadyUsed(Boolean.class.getSimpleName(), compilationUnit)) {
            return ast.newSimpleName(Boolean.class.getSimpleName());
        }

        return ASTNodeFactory.newName(ast, Boolean.class.getCanonicalName());
    }

    private boolean isSimpleNameAlreadyUsed(final String simpleName, final CompilationUnit compilationUnit) {
        for (ImportDeclaration importDeclaration : (List<ImportDeclaration>) compilationUnit.imports()) {
            QualifiedName f= (QualifiedName) importDeclaration.getName();

            if (simpleName.equals(f.getName().getIdentifier())) {
                return true;
            }
        }

        return false;
    }
}

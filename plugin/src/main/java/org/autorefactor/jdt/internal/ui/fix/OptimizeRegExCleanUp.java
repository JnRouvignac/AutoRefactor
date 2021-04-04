/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - initial API and implementation
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

import java.util.List;
import java.util.regex.MatchResult;
import java.util.regex.Pattern;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.VarDefinitionsUsesVisitor;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class OptimizeRegExCleanUp extends AbstractCleanUpRule {
    private static final String SPLIT_METHOD= "split"; //$NON-NLS-1$
    private static final String REPLACE_FIRST_METHOD= "replaceFirst"; //$NON-NLS-1$
    private static final String REPLACE_ALL_METHOD= "replaceAll"; //$NON-NLS-1$
    private static final String MATCHES_METHOD= "matches"; //$NON-NLS-1$
    private static final String COMPILE_METHOD= "compile"; //$NON-NLS-1$

    private static final Pattern COMMENT_PATTERN= Pattern.compile("(?<!\\\\)\\(\\?#"); //$NON-NLS-1$
    private static final Pattern DIGIT_PATTERN= Pattern.compile("(?<!\\\\)\\[0\\-9\\]"); //$NON-NLS-1$
    private static final Pattern QUESTION_DOT_PATTERN= Pattern.compile("(?<!\\\\)\\{\\s*?0\\s*?,\\s*?1\\s*?\\}"); //$NON-NLS-1$
    private static final Pattern START_PATTERN= Pattern.compile("(?<!\\\\)\\{\\s*?0\\s*?,\\s*?\\}"); //$NON-NLS-1$
    private static final Pattern PLUS_PATTERN= Pattern.compile("(?<!\\\\)\\{\\s*?1\\s*?,\\s*?\\}"); //$NON-NLS-1$
    private static final Pattern DUPLICATE_WITH_REPETITOR_PATTERN= Pattern.compile("([^\\*\\?\\{][^\\(\\)\\[\\]\\{\\}]*(?:(?:\\.|\\[[^\\(\\)\\[\\]\\{\\}]*\\]|\\{[^\\(\\)\\[\\]\\{\\}]*\\})[^\\(\\)\\[\\]\\{\\}]*)+)\\1(\\+\\*\\?\\{\\d+,\\d*\\})"); //$NON-NLS-1$
    private static final Pattern SIMPLE_DUPLICATE_WITHOUT_REPETITOR_PATTERN= Pattern.compile("([^\\\\]|\\(\\?\\:[^\\(\\)\\[\\]\\{\\}]*\\)|\\[[^\\(\\)\\[\\]\\{\\}]*\\]|\\{[^\\(\\)\\[\\]\\{\\}]*\\})\\1(?!\\+\\*\\?\\{)"); //$NON-NLS-1$
    private static final Pattern DUPLICATE_WITHOUT_REPETITOR_PATTERN= Pattern.compile("([^\\*\\?\\{][^\\(\\)\\[\\]\\{\\}]*(?:(?:\\.|\\[[^\\(\\)\\[\\]\\{\\}]*\\]|\\{[^\\(\\)\\[\\]\\{\\}]*\\})[^\\(\\)\\[\\]\\{\\}]*)+)\\1(?!\\+\\*\\?\\{)"); //$NON-NLS-1$

    @Override
    public String getName() {
        return MultiFixMessages.OptimizeRegExCleanUp_name;
    }

    @Override
    public String getDescription() {
        return MultiFixMessages.OptimizeRegExCleanUp_description;
    }

    @Override
    public String getReason() {
        return MultiFixMessages.OptimizeRegExCleanUp_reason;
    }

    @Override
    public boolean visit(final StringLiteral visited) {
        if (isRegEx(visited)) {
            return maybeRewriteRegEx(visited);
        }

        return true;
    }

    private boolean isRegEx(final ASTNode visited) {
        ASTNode parent= visited.getParent();

        switch (parent.getNodeType()) {
        case ASTNode.PARENTHESIZED_EXPRESSION:
            return isRegEx(parent);

        case ASTNode.METHOD_INVOCATION:
            MethodInvocation methodInvocation= (MethodInvocation) parent;

            if (visited.getLocationInParent() == MethodInvocation.ARGUMENTS_PROPERTY) {
                if (!methodInvocation.arguments().isEmpty()
                        && visited.equals(methodInvocation.arguments().get(0))) {
                    if (ASTNodes.usesGivenSignature(methodInvocation, String.class.getCanonicalName(), MATCHES_METHOD, String.class.getCanonicalName())
                            || ASTNodes.usesGivenSignature(methodInvocation, String.class.getCanonicalName(), REPLACE_ALL_METHOD, String.class.getCanonicalName(), String.class.getCanonicalName())
                            || ASTNodes.usesGivenSignature(methodInvocation, String.class.getCanonicalName(), REPLACE_FIRST_METHOD, String.class.getCanonicalName(), String.class.getCanonicalName())
                            || ASTNodes.usesGivenSignature(methodInvocation, String.class.getCanonicalName(), SPLIT_METHOD, String.class.getCanonicalName())
                            || ASTNodes.usesGivenSignature(methodInvocation, String.class.getCanonicalName(), SPLIT_METHOD, String.class.getCanonicalName(), int.class.getCanonicalName())
                            || ASTNodes.usesGivenSignature(methodInvocation, Pattern.class.getCanonicalName(), MATCHES_METHOD, String.class.getCanonicalName(), CharSequence.class.getCanonicalName())) {
                        return true;
                    }

                    if (ASTNodes.usesGivenSignature(methodInvocation, Pattern.class.getCanonicalName(), COMPILE_METHOD, String.class.getCanonicalName())
                            || ASTNodes.usesGivenSignature(methodInvocation, Pattern.class.getCanonicalName(), COMPILE_METHOD, String.class.getCanonicalName(), int.class.getCanonicalName())) {
                        // TODO Do escape analysis for Pattern.pattern()
                    	methodInvocation.getParent();
                        return false;
                    }
                }
            }

            return false;

        case ASTNode.SINGLE_VARIABLE_DECLARATION:
            SingleVariableDeclaration singleVariableDeclaration= (SingleVariableDeclaration) parent;

            if (visited.getLocationInParent() != SingleVariableDeclaration.INITIALIZER_PROPERTY) {
                return false;
            }

            return isRegEx(singleVariableDeclaration.getType().resolveBinding(), singleVariableDeclaration.resolveBinding(), singleVariableDeclaration.getExtraDimensions(), singleVariableDeclaration.getInitializer());

        case ASTNode.ASSIGNMENT:
            Assignment assignment= (Assignment) parent;

            if (visited.getLocationInParent() != Assignment.RIGHT_HAND_SIDE_PROPERTY
                    || !ASTNodes.hasOperator(assignment, Assignment.Operator.ASSIGN)
                    || !(assignment.getLeftHandSide() instanceof SimpleName)
                    || ((SimpleName) assignment.getLeftHandSide()).resolveTypeBinding().getKind() != IBinding.VARIABLE) {
                return false;
            }

            return isRegEx(assignment.getLeftHandSide().resolveTypeBinding(), (IVariableBinding) (SimpleName) assignment.getLeftHandSide(), 0, assignment.getRightHandSide());

        case ASTNode.VARIABLE_DECLARATION_FRAGMENT:
            VariableDeclarationFragment fragment= (VariableDeclarationFragment) parent;

            if (visited.getLocationInParent() != VariableDeclarationFragment.INITIALIZER_PROPERTY
                    || !(fragment.getParent() instanceof VariableDeclarationStatement)
                    || fragment.getLocationInParent() != VariableDeclarationStatement.FRAGMENTS_PROPERTY) {
                return false;
            }

            return isRegEx(((VariableDeclarationStatement) fragment.getParent()).getType().resolveBinding(), fragment.resolveBinding(), fragment.getExtraDimensions(), fragment.getInitializer());
        }

        return false;
    }

    private boolean isRegEx(ITypeBinding typeBinding, final IVariableBinding variableBinding, final int extraDimensions,
            final Expression initializer) {
        if (ASTNodes.hasType(typeBinding, String.class.getCanonicalName())
                && extraDimensions == 0
                && initializer != null) {
            VarDefinitionsUsesVisitor varOccurrencesVisitor= new VarDefinitionsUsesVisitor(variableBinding,
			initializer.getRoot(), true);

            List<SimpleName> reads= varOccurrencesVisitor.getReads();
            List<SimpleName> writes= varOccurrencesVisitor.getWrites();

            for (SimpleName write : writes) {
                if (write.getParent() instanceof Assignment
                        && write.getLocationInParent() == Assignment.LEFT_HAND_SIDE_PROPERTY
                        && !ASTNodes.hasOperator((Assignment) write.getParent(), Assignment.Operator.ASSIGN)) {
                    return false;
                }
            }

            if (!reads.isEmpty()) {
                for (SimpleName read : reads) {
                    if (!isRegEx(read)) {
                        return false;
                    }
                }

                return true;
            }
        }

        return false;
    }

    private boolean maybeRewriteRegEx(final StringLiteral visited) {
        String pattern= visited.getLiteralValue();

        if (COMMENT_PATTERN.matcher(pattern).find()) {
            return true;
        }

        if (QUESTION_DOT_PATTERN.matcher(pattern).find()) {
            pattern= QUESTION_DOT_PATTERN.matcher(pattern).replaceAll("?"); //$NON-NLS-1$
        }

        if (START_PATTERN.matcher(pattern).find()) {
            pattern= START_PATTERN.matcher(pattern).replaceAll("*"); //$NON-NLS-1$
        }

        if (PLUS_PATTERN.matcher(pattern).find()) {
            pattern= PLUS_PATTERN.matcher(pattern).replaceAll("+"); //$NON-NLS-1$
        }

        if (DIGIT_PATTERN.matcher(pattern).find()) {
            pattern= DIGIT_PATTERN.matcher(pattern).replaceAll("\\\\d"); //$NON-NLS-1$
        }

        if (SIMPLE_DUPLICATE_WITHOUT_REPETITOR_PATTERN.matcher(pattern).find()) {
            pattern= SIMPLE_DUPLICATE_WITHOUT_REPETITOR_PATTERN.matcher(pattern).replaceAll("$1{2}"); //$NON-NLS-1$
        }

        if (DUPLICATE_WITHOUT_REPETITOR_PATTERN.matcher(pattern).find()) {
            pattern= DUPLICATE_WITHOUT_REPETITOR_PATTERN.matcher(pattern).replaceAll("(?:$1){2}"); //$NON-NLS-1$
        }

        try {
			while (DUPLICATE_WITHOUT_REPETITOR_PATTERN.matcher(pattern).find()) {
			    MatchResult matchResult= DUPLICATE_WITHOUT_REPETITOR_PATTERN.matcher(pattern).toMatchResult();
			    // TODO Correctly handle repetition enclosing
			    String pattern2;

			    if ("*".equals(matchResult.group(2))) { //$NON-NLS-1$
			        pattern2= DUPLICATE_WITHOUT_REPETITOR_PATTERN.matcher(pattern).replaceAll("(?:$1)+"); //$NON-NLS-1$
			    } else if ("+".equals(matchResult.group(2))) { //$NON-NLS-1$
			        pattern2= DUPLICATE_WITHOUT_REPETITOR_PATTERN.matcher(pattern).replaceAll("(?:$1){2,}"); //$NON-NLS-1$
			    } else if ("?".equals(matchResult.group(2))) { //$NON-NLS-1$
			        pattern2= DUPLICATE_WITHOUT_REPETITOR_PATTERN.matcher(pattern).replaceAll("(?:$1){1,2}"); //$NON-NLS-1$
			    }
			    break;
			}
		} catch (Exception e) {
			e.getMessage();
		}

        if (!Utils.equalNotNull(visited.getLiteralValue(), pattern)) {
            rewriteRegEx(visited, pattern);
            return false;
        }

        return true;
    }

    private void rewriteRegEx(final StringLiteral visited, final String pattern) {
        ASTRewrite rewrite= cuRewrite.getASTRewrite();
        ASTNodeFactory ast= cuRewrite.getASTBuilder();
        TextEditGroup group= new TextEditGroup(MultiFixMessages.OptimizeRegExCleanUp_description);


        rewrite.replace(visited, ast.newStringLiteral(pattern), group);
    }
}

/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - #199 Replace unnecessary Boolean constant on boolean assignment
 *                                        #200 Compile error when Float myFloat = new Float(doubleObject);
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

import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.arg0;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.arguments;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.getTargetType;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.hasType;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.usesGivenSignature;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.isPrimitive;
import static org.autorefactor.util.Utils.equal;

import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.ASTBuilder;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.SimpleName;

/** See {@link #getDescription()} method. */
public class PrimitiveWrapperCreationCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_PrimitiveWrapperCreationCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_PrimitiveWrapperCreationCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_PrimitiveWrapperCreationCleanUp_reason;
    }

    private int getJavaMinorVersion() {
        return ctx.getJavaProjectOptions().getJavaSERelease().getMinorVersion();
    }

    @Override
    public boolean visit(MethodInvocation node) {
        if (node.getExpression() == null) {
            return true;
        }

        ITypeBinding destinationTypeBinding= getTargetType(node);

        if (destinationTypeBinding != null && destinationTypeBinding.isPrimitive()
                && "valueOf".equals(node.getName().getIdentifier())) { //$NON-NLS-1$
            if (usesGivenSignature(node, Boolean.class.getCanonicalName(), "valueOf", boolean.class.getSimpleName()) //$NON-NLS-1$
                    || usesGivenSignature(node, Byte.class.getCanonicalName(), "valueOf", byte.class.getSimpleName()) //$NON-NLS-1$
                    || usesGivenSignature(node, Character.class.getCanonicalName(), "valueOf", char.class.getSimpleName()) //$NON-NLS-1$
                    || usesGivenSignature(node, Short.class.getCanonicalName(), "valueOf", short.class.getSimpleName()) //$NON-NLS-1$
                    || usesGivenSignature(node, Integer.class.getCanonicalName(), "valueOf", int.class.getSimpleName()) //$NON-NLS-1$
                    || usesGivenSignature(node, Long.class.getCanonicalName(), "valueOf", long.class.getSimpleName()) //$NON-NLS-1$
                    || usesGivenSignature(node, Float.class.getCanonicalName(), "valueOf", float.class.getSimpleName()) //$NON-NLS-1$
                    || usesGivenSignature(node, Double.class.getCanonicalName(), "valueOf", double.class.getSimpleName())) { //$NON-NLS-1$
                return replaceWithTheSingleArgument(node);
            }
            if (is(node, Byte.class.getCanonicalName())) {
                return replaceMethodName(node, "parseByte"); //$NON-NLS-1$
            }
            if (is(node, Short.class.getCanonicalName())) {
                return replaceMethodName(node, "parseShort"); //$NON-NLS-1$
            }
            if (is(node, Integer.class.getCanonicalName())) {
                return replaceMethodName(node, "parseInt"); //$NON-NLS-1$
            }
            if (is(node, Long.class.getCanonicalName())) {
                return replaceMethodName(node, "parseLong"); //$NON-NLS-1$
            }
            if (usesGivenSignature(node, Boolean.class.getCanonicalName(), "valueOf", String.class.getCanonicalName())) { //$NON-NLS-1$
                return replaceMethodName(node, "parseBoolean"); //$NON-NLS-1$
            }
            if (is(node, Float.class.getCanonicalName())) {
                return replaceMethodName(node, "parseFloat"); //$NON-NLS-1$
            }
            if (is(node, Double.class.getCanonicalName())) {
                return replaceMethodName(node, "parseDouble"); //$NON-NLS-1$
            }
        }

        final ITypeBinding typeBinding= node.getExpression().resolveTypeBinding();
        if (typeBinding != null && node.getExpression() instanceof ClassInstanceCreation) {
            final List<Expression> cicArgs= arguments((ClassInstanceCreation) node.getExpression());
            if (cicArgs.size() == 1) {
                final Expression arg0= cicArgs.get(0);
                if (arguments(node).isEmpty() && hasType(arg0, String.class.getCanonicalName())) {
                    final String methodName= getMethodName(typeBinding.getQualifiedName(),
                            node.getName().getIdentifier());
                    if (methodName != null) {
                        ctx.getRefactorings().replace(node,
                                newMethodInvocation(typeBinding.getName(), methodName, arg0));
                        return false;
                    }
                }
            }
        }
        return true;
    }

    private boolean is(MethodInvocation node, String declaringTypeQualifiedName) {
        return usesGivenSignature(node, declaringTypeQualifiedName, "valueOf", String.class.getCanonicalName()) //$NON-NLS-1$
                || (usesGivenSignature(node, declaringTypeQualifiedName, "valueOf", String.class.getCanonicalName(), int.class.getSimpleName()) //$NON-NLS-1$
                        && equal(10, arguments(node).get(1).resolveConstantExpressionValue()));
    }

    private boolean replaceMethodName(MethodInvocation node, String methodName) {
        final SimpleName name= this.ctx.getASTBuilder().simpleName(methodName);
        this.ctx.getRefactorings().set(node, MethodInvocation.NAME_PROPERTY, name);
        return false;
    }

    private boolean replaceWithTheSingleArgument(MethodInvocation node) {
        final ASTBuilder b= this.ctx.getASTBuilder();
        this.ctx.getRefactorings().replace(node, b.copy(arg0(node)));
        return false;
    }

    private String getMethodName(final String typeName, final String invokedMethodName) {
        if (Boolean.class.getCanonicalName().equals(typeName) && "booleanValue".equals(invokedMethodName)) { //$NON-NLS-1$
            return "valueOf"; //$NON-NLS-1$
        } else if (Byte.class.getCanonicalName().equals(typeName) && "byteValue".equals(invokedMethodName)) { //$NON-NLS-1$
            return "parseByte"; //$NON-NLS-1$
        } else if (Double.class.getCanonicalName().equals(typeName) && "doubleValue".equals(invokedMethodName)) { //$NON-NLS-1$
            return "parseDouble"; //$NON-NLS-1$
        } else if (Float.class.getCanonicalName().equals(typeName) && "floatValue".equals(invokedMethodName)) { //$NON-NLS-1$
            return "parseFloat"; //$NON-NLS-1$
        } else if (Long.class.getCanonicalName().equals(typeName) && "longValue".equals(invokedMethodName)) { //$NON-NLS-1$
            return "parseLong"; //$NON-NLS-1$
        } else if (Short.class.getCanonicalName().equals(typeName) && "shortValue".equals(invokedMethodName)) { //$NON-NLS-1$
            return "parseShort"; //$NON-NLS-1$
        } else if (Integer.class.getCanonicalName().equals(typeName) && "intValue".equals(invokedMethodName)) { //$NON-NLS-1$
            return "parseInt"; //$NON-NLS-1$
        }
        return null;
    }

    @Override
    public boolean visit(ClassInstanceCreation node) {
        final ITypeBinding typeBinding= node.getType().resolveBinding();
        final List<Expression> args= arguments(node);
        if (getJavaMinorVersion() >= 5 && args.size() == 1) {
            if (hasType(typeBinding, Boolean.class.getCanonicalName(), Byte.class.getCanonicalName(), Character.class.getCanonicalName(), Double.class.getCanonicalName(),
                    Long.class.getCanonicalName(), Short.class.getCanonicalName(), Integer.class.getCanonicalName())) {
                replaceWithValueOf(node, typeBinding);
                return false;
            } else if (hasType(typeBinding, Float.class.getCanonicalName())) {
                return replaceFloatInstanceWithValueOf(node, typeBinding, args);
            }
        }
        return true;
    }

    private boolean replaceFloatInstanceWithValueOf(ClassInstanceCreation node, final ITypeBinding typeBinding,
            final List<Expression> args) {
        final Expression arg0= args.get(0);
        if (isPrimitive(arg0, double.class.getSimpleName())) {
            final ASTBuilder b= ctx.getASTBuilder();
            ctx.getRefactorings().replace(node,
                    b.invoke(typeBinding.getName(), "valueOf", b.cast(b.type(float.class.getSimpleName()), b.copy(arg0)))); //$NON-NLS-1$
        } else if (hasType(arg0, Double.class.getCanonicalName())) {
            final ASTBuilder b= ctx.getASTBuilder();
            ctx.getRefactorings().replace(node, b.invoke(b.copy(arg0), "floatValue")); //$NON-NLS-1$
        } else {
            replaceWithValueOf(node, typeBinding);
        }
        return false;
    }

    private void replaceWithValueOf(ClassInstanceCreation node, final ITypeBinding typeBinding) {
        this.ctx.getRefactorings().replace(node,
                newMethodInvocation(typeBinding.getName(), "valueOf", arguments(node).get(0))); //$NON-NLS-1$
    }

    private MethodInvocation newMethodInvocation(String typeName, String methodName, Expression arg) {
        final ASTBuilder b= this.ctx.getASTBuilder();
        return b.invoke(typeName, methodName, b.copy(arg));
    }
}

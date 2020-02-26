/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2016 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ArrayInitializer;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IAnnotationBinding;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MemberValuePair;
import org.eclipse.jdt.core.dom.NormalAnnotation;

/** See {@link #getDescription()} method. */
public class AnnotationCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_AnnotationCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_AnnotationCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_AnnotationCleanUp_reason;
    }

    @Override
    public boolean visit(final NormalAnnotation node) {
        Refactorings r= this.ctx.getRefactorings();
        ASTNodeFactory b= this.ctx.getASTBuilder();
        List<MemberValuePair> values= ASTNodes.values(node);
        if (values.isEmpty()) {
            r.replace(node, b.markerAnnotation(b.createMoveTarget(node.getTypeName())));
            return false;
        }
        if (values.size() == 1) {
            MemberValuePair pair= values.get(0);
            if ("value".equals(pair.getName().getIdentifier())) { //$NON-NLS-1$
                r.replace(node, b.singleValueAnnotation(b.createMoveTarget(node.getTypeName()), b.createMoveTarget(pair.getValue())));
                return false;
            }
        }

        boolean result= true;
        Map<String, IMethodBinding> elements= toElementsMap(node.resolveAnnotationBinding());
        for (MemberValuePair pair : values) {
            IMethodBinding elementBinding= elements.get(pair.getName().getIdentifier());
            if (equal(elementBinding.getReturnType(), pair.getValue(), elementBinding.getDefaultValue())) {
                r.remove(pair);
                result= false;
            } else if (pair.getValue().getNodeType() == ASTNode.ARRAY_INITIALIZER) {
                ArrayInitializer arrayInit= (ArrayInitializer) pair.getValue();
                List<Expression> exprs= ASTNodes.expressions(arrayInit);
                if (exprs.size() == 1) {
                    r.replace(arrayInit, b.createMoveTarget(exprs.get(0)));
                    result= false;
                }
            }
        }

        return result;
    }

    private Map<String, IMethodBinding> toElementsMap(final IAnnotationBinding annotBinding) {
        if (annotBinding == null) {
            return Collections.emptyMap();
        }
        ITypeBinding annotationType= annotBinding.getAnnotationType();
        IMethodBinding[] elements= annotationType.getDeclaredMethods();
        Map<String, IMethodBinding> results= new HashMap<>();
        for (IMethodBinding element : elements) {
            results.put(element.getName(), element);
        }

        return results;
    }

    private boolean equal(final ITypeBinding typeBinding, final Expression expression, final Object javaObj2) {
        Object javaObj1= expression.resolveConstantExpressionValue();
        switch (expression.getNodeType()) {
        case ASTNode.ARRAY_INITIALIZER:
            return arraysEqual(typeBinding, (ArrayInitializer) expression, javaObj2);

        case ASTNode.BOOLEAN_LITERAL:
        case ASTNode.CHARACTER_LITERAL:
        case ASTNode.STRING_LITERAL:
            return Utils.equalNotNull(javaObj1, javaObj2);

        case ASTNode.NUMBER_LITERAL:
            if (typeBinding.isPrimitive()) {
                String type= typeBinding.getQualifiedName();

                if (type.equals(byte.class.getSimpleName())) {
                    return Utils.equalNotNull(toByte(javaObj1), toByte(javaObj2));
                }

                if (type.equals(short.class.getSimpleName())) {
                    return Utils.equalNotNull(toShort(javaObj1), toShort(javaObj2));
                }

                if (type.equals(int.class.getSimpleName())) {
                    return Utils.equalNotNull(toInteger(javaObj1), toInteger(javaObj2));
                }

                if (type.equals(long.class.getSimpleName())) {
                    return Utils.equalNotNull(toLong(javaObj1), toLong(javaObj2));
                }

                if (type.equals(float.class.getSimpleName())) {
                    return Utils.equalNotNull(toFloat(javaObj1), toFloat(javaObj2));
                }

                if (type.equals(double.class.getSimpleName())) {
                    return Utils.equalNotNull(toDouble(javaObj1), toDouble(javaObj2));
                }

                throw new NotImplementedException(expression, "for primitive type \"" + type + "\"."); //$NON-NLS-1$ //$NON-NLS-2$
            }

            return false;

        default:
            return false;
        }
    }

    private boolean arraysEqual(final ITypeBinding typeBinding, final ArrayInitializer arrayInit, final Object javaObj) {
        if (javaObj instanceof Object[]) {
            Object[] javaObjArray= (Object[]) javaObj;
            List<Expression> exprs= ASTNodes.expressions(arrayInit);

            if (exprs.size() == javaObjArray.length) {
                for (int i= 0; i < javaObjArray.length; i++) {
                    if (!equal(typeBinding.getElementType(), exprs.get(i), javaObjArray[i])) {
                        return false;
                    }
                }

                return true;
            }
        }

        return false;
    }

    private Byte toByte(final Object javaObj) {
        // No byte literal exist
        if (javaObj instanceof Integer) {
            int i= (Integer) javaObj;
            if (Byte.MIN_VALUE <= i && i <= Byte.MAX_VALUE) {
                return (byte) i;
            }
        }

        return null;
    }

    private Short toShort(final Object javaObj) {
        // No short literal exist
        if (javaObj instanceof Integer) {
            int i= (Integer) javaObj;
            if (Short.MIN_VALUE <= i && i <= Short.MAX_VALUE) {
                return (short) i;
            }
        }

        return null;
    }

    private Integer toInteger(final Object javaObj) {
        if (javaObj instanceof Integer) {
            return (Integer) javaObj;
        }

        return null;
    }

    private Long toLong(final Object javaObj) {
        if (javaObj instanceof Integer) {
            return ((Integer) javaObj).longValue();
        }
        if (javaObj instanceof Long) {
            return (Long) javaObj;
        }

        return null;
    }

    private Float toFloat(final Object javaObj) {
        if (javaObj instanceof Integer) {
            return ((Integer) javaObj).floatValue();
        }
        if (javaObj instanceof Long) {
            return ((Long) javaObj).floatValue();
        }
        if (javaObj instanceof Float) {
            return (Float) javaObj;
        }

        return null;
    }

    private Double toDouble(final Object javaObj) {
        if (javaObj instanceof Integer) {
            return ((Integer) javaObj).doubleValue();
        }
        if (javaObj instanceof Long) {
            return ((Long) javaObj).doubleValue();
        }
        if (javaObj instanceof Float) {
            return ((Float) javaObj).doubleValue();
        }
        if (javaObj instanceof Double) {
            return (Double) javaObj;
        }

        return null;
    }
}

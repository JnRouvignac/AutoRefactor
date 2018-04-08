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
package org.autorefactor.refactoring.rules;

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.expressions;
import static org.autorefactor.refactoring.ASTHelper.getPrimitiveEnum;
import static org.autorefactor.refactoring.ASTHelper.values;
import static org.eclipse.jdt.core.dom.ASTNode.ARRAY_INITIALIZER;
import static org.eclipse.jdt.core.dom.ASTNode.BOOLEAN_LITERAL;
import static org.eclipse.jdt.core.dom.ASTNode.CHARACTER_LITERAL;
import static org.eclipse.jdt.core.dom.ASTNode.NUMBER_LITERAL;
import static org.eclipse.jdt.core.dom.ASTNode.STRING_LITERAL;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.ASTHelper.PrimitiveEnum;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.ArrayInitializer;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IAnnotationBinding;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MemberValuePair;
import org.eclipse.jdt.core.dom.NormalAnnotation;

/** See {@link #getDescription()} method. */
public class AnnotationRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Annotation";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Simplifies annotation uses:\n"
            + "- empty parentheses will be removed from annotations,\n"
            + "- single members named \"value\" will be removed from annotations and only the value will be left.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves the readibility.";
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

        boolean result = VISIT_SUBTREE;
        Map<String, IMethodBinding> elements = toElementsMap(node.resolveAnnotationBinding());
        for (MemberValuePair pair : values) {
            IMethodBinding elementBinding = elements.get(pair.getName().getIdentifier());
            if (equal(elementBinding.getReturnType(), pair.getValue(), elementBinding.getDefaultValue())) {
                r.remove(pair);
                result = DO_NOT_VISIT_SUBTREE;
            } else if (pair.getValue().getNodeType() == ARRAY_INITIALIZER) {
                ArrayInitializer arrayInit = (ArrayInitializer) pair.getValue();
                List<Expression> exprs = expressions(arrayInit);
                if (exprs.size() == 1) {
                    r.replace(arrayInit, b.move(exprs.get(0)));
                    result = DO_NOT_VISIT_SUBTREE;
                }
            }
        }
        return result;
    }

    private Map<String, IMethodBinding> toElementsMap(IAnnotationBinding annotBinding) {
        if (annotBinding == null) {
            return Collections.emptyMap();
        }
        ITypeBinding annotationType = annotBinding.getAnnotationType();
        IMethodBinding[] elements = annotationType.getDeclaredMethods();
        Map<String, IMethodBinding> results = new HashMap<String, IMethodBinding>();
        for (IMethodBinding element : elements) {
            results.put(element.getName(), element);
        }
        return results;
    }

    private boolean equal(ITypeBinding typeBinding, Expression expr, Object javaObj2) {
        Object javaObj1 = expr.resolveConstantExpressionValue();
        switch (expr.getNodeType()) {
        case ARRAY_INITIALIZER:
            return arraysEqual(typeBinding, (ArrayInitializer) expr, javaObj2);
        case BOOLEAN_LITERAL:
            return Utils.equalNotNull(javaObj1, javaObj2);
        case CHARACTER_LITERAL:
            return Utils.equalNotNull(javaObj1, javaObj2);
        case STRING_LITERAL:
            return Utils.equalNotNull(javaObj1, javaObj2);
        case NUMBER_LITERAL:
            PrimitiveEnum primEnum = getPrimitiveEnum(typeBinding);
            switch (primEnum) {
            case BYTE:
                return Utils.equalNotNull(toByte(javaObj1), toByte(javaObj2));
            case SHORT:
                return Utils.equalNotNull(toShort(javaObj1), toShort(javaObj2));
            case INT:
                return Utils.equalNotNull(toInteger(javaObj1), toInteger(javaObj2));
            case LONG:
                return Utils.equalNotNull(toLong(javaObj1), toLong(javaObj2));
            case FLOAT:
                return Utils.equalNotNull(toFloat(javaObj1), toFloat(javaObj2));
            case DOUBLE:
                return Utils.equalNotNull(toDouble(javaObj1), toDouble(javaObj2));
            default:
                throw new NotImplementedException(expr, "for primitive type \"" + primEnum + "\".");
            }
        default:
            return false;
        }
    }

    private boolean arraysEqual(ITypeBinding typeBinding, ArrayInitializer arrayInit, Object javaObj) {
        if (javaObj instanceof Object[]) {
            Object[] javaObjArray = (Object[]) javaObj;
            List<Expression> exprs = expressions(arrayInit);
            if (exprs.size() == javaObjArray.length) {
                boolean result = true;
                for (int i = 0; i < javaObjArray.length; i++) {
                    result &= equal(typeBinding.getElementType(), exprs.get(i), javaObjArray[i]);
                }
                return result;
            }
        }
        return false;
    }

    private Byte toByte(Object javaObj) {
        // no byte literal exist
        if (javaObj instanceof Integer) {
            int i = (Integer) javaObj;
            if (Byte.MIN_VALUE <= i && i <= Byte.MAX_VALUE) {
                return (byte) i;
            }
        }
        return null;
    }

    private Short toShort(Object javaObj) {
        // no short literal exist
        if (javaObj instanceof Integer) {
            int i = (Integer) javaObj;
            if (Short.MIN_VALUE <= i && i <= Short.MAX_VALUE) {
                return (short) i;
            }
        }
        return null;
    }

    private Integer toInteger(Object javaObj) {
        if (javaObj instanceof Integer) {
            return (Integer) javaObj;
        }
        return null;
    }

    private Long toLong(Object javaObj) {
        if (javaObj instanceof Integer) {
            return ((Integer) javaObj).longValue();
        } else  if (javaObj instanceof Long) {
            return (Long) javaObj;
        }
        return null;
    }

    private Float toFloat(Object javaObj) {
        if (javaObj instanceof Integer) {
            return ((Integer) javaObj).floatValue();
        } else  if (javaObj instanceof Long) {
            return ((Long) javaObj).floatValue();
        } else  if (javaObj instanceof Float) {
            return (Float) javaObj;
        }
        return null;
    }

    private Double toDouble(Object javaObj) {
        if (javaObj instanceof Integer) {
            return ((Integer) javaObj).doubleValue();
        } else  if (javaObj instanceof Long) {
            return ((Long) javaObj).doubleValue();
        } else  if (javaObj instanceof Float) {
            return ((Float) javaObj).doubleValue();
        } else  if (javaObj instanceof Double) {
            return (Double) javaObj;
        }
        return null;
    }
}

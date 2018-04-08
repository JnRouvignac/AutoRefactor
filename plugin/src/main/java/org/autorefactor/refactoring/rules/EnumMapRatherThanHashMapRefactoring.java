/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Andrei Paikin - Initial API and implementation
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
import static org.autorefactor.refactoring.ASTHelper.arguments;
import static org.autorefactor.refactoring.ASTHelper.hasType;
import static org.autorefactor.refactoring.ASTHelper.instanceOf;

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.TypeLiteral;

/**
 * Replaces HashMap for enum type creation to EnumMap.
 */
public final class EnumMapRatherThanHashMapRefactoring extends
        AbstractEnumCollectionReplacementRefactoring {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "EnumMap rather than HashMap for enum keys";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Refactor implementation class HashMap -> EnumMap when key is a enum type";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves the space performance.";
    }

    @Override
    String getImplType() {
        return "java.util.HashMap";
    }

    @Override
    String getInterfaceType() {
        return "java.util.Map";
    }

    /**
     * Replace given class instance creation with suitable EnumMap constructor. <br>
     * <br>
     * Replacement is not correct if HashMap constructor accepts map <br>
     * other than EnumMap, because it throws <code>IllegalArgumentException</code> if map is empty,
     * <br>
     * and HashMap(Map) does not. Therefore, for correctness reasons, it should not be refactored.
     * <br>
     *
     * @see {@link java.util.EnumMap#EnumMap(java.util.Map)}
     * @see {@link java.util.HashMap#HashMap(java.util.Map)}
     */
    @Override
    boolean replace(ClassInstanceCreation cic, Type... types) {

        if (types == null || types.length < 2) {
            return VISIT_SUBTREE;
        }
        Type keyType = types[0];
        Type valueType = types[1];
        ASTBuilder b = ctx.getASTBuilder();
        List<Expression> arguments = arguments(cic);
        if (!arguments.isEmpty()
                && isTargetType(arguments.get(0).resolveTypeBinding())
                && !hasType(arguments.get(0).resolveTypeBinding(),
                        "java.util.EnumMap")) {
            return VISIT_SUBTREE;
        }
        Expression newParam = resolveParameter(keyType, arguments);
        Type newType = b.genericType("java.util.EnumMap", b.copy(keyType),
                b.copy(valueType));
        // if there were no type args in original creation (diamond operator),
        // remove them from replacement
        if (typeArgs(cic.getType()).isEmpty()) {
            typeArgs(newType).clear();
        }

        ctx.getRefactorings().replace(cic, b.new0(newType, newParam));
        return DO_NOT_VISIT_SUBTREE;
    }

    /**
     * Map parameter for HashMap constructor to EnumMap constructor. HashMap(Map) ->
     * EnumMap(EnumMap) <br/>
     * other HashMap constructors -> EnumMap(Class) <br>
     *
     * @return correct parameter for EnumMap constructor
     */
    private Expression resolveParameter(Type keyType,
            List<Expression> originalArgs) {
        if (!originalArgs.isEmpty()
                && instanceOf(originalArgs.get(0), "java.util.EnumMap")) {
            return ctx.getASTBuilder().copy(originalArgs.get(0));
        }
        TypeLiteral keyTypeLiteral = keyType.getAST().newTypeLiteral();
        keyTypeLiteral.setType(ctx.getASTBuilder().copy(keyType));
        return keyTypeLiteral;
    }
}

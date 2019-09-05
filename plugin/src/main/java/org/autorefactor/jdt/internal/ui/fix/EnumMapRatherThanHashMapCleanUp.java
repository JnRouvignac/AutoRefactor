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
package org.autorefactor.jdt.internal.ui.fix;

import java.util.Arrays;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.TypeLiteral;

/**
 * Replaces HashMap for enum type creation to EnumMap.
 */
public final class EnumMapRatherThanHashMapCleanUp extends AbstractEnumCollectionReplacementCleanUp {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_EnumMapRatherThanHashMapCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_EnumMapRatherThanHashMapCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_EnumMapRatherThanHashMapCleanUp_reason;
    }

    @Override
    public String getImplType() {
        return HashMap.class.getCanonicalName();
    }

    @Override
    public String getInterfaceType() {
        return Map.class.getCanonicalName();
    }

    @Override
    public Set<String> getClassesToImport() {
        return new HashSet<>(Arrays.asList(EnumMap.class.getCanonicalName()));
    }

    /**
     * Replace given class instance creation with suitable EnumMap constructor. <br>
     * <br>
     * Replacement is not correct if HashMap constructor accepts map <br>
     * other than EnumMap, because it throws <code>IllegalArgumentException</code>
     * if map is empty, <br>
     * and HashMap(Map) does not. Therefore, for correctness reasons, it should not
     * be refactored. <br>
     *
     * @see {@link java.util.EnumMap#EnumMap(java.util.Map)}
     * @see {@link java.util.HashMap#HashMap(java.util.Map)}
     */
    @Override
    boolean maybeReplace(ClassInstanceCreation cic, Set<String> alreadyImportedClasses, Set<String> importsToAdd,
            Type... types) {
        if (types == null || types.length < 2) {
            return true;
        }

        Type keyType= types[0];
        Type valueType= types[1];
        List<Expression> arguments= ASTNodes.arguments(cic);

        if (!arguments.isEmpty() && isTargetType(arguments.get(0).resolveTypeBinding())
                && !ASTNodes.hasType(arguments.get(0).resolveTypeBinding(), EnumMap.class.getCanonicalName())) {
            return true;
        }

        replace(cic, alreadyImportedClasses, importsToAdd, keyType, valueType, arguments);
        importsToAdd.add(EnumMap.class.getCanonicalName());
        return false;
    }

    private void replace(ClassInstanceCreation cic, Set<String> alreadyImportedClasses, Set<String> importsToAdd,
            Type keyType, Type valueType, List<Expression> arguments) {
        ASTNodeFactory b= ctx.getASTBuilder();
        Expression newParam= resolveParameter(keyType, arguments);
        Type newType= b.genericType(
                alreadyImportedClasses.contains(EnumMap.class.getCanonicalName()) ? "EnumMap" : EnumMap.class.getCanonicalName(), b.copy(keyType), //$NON-NLS-1$
                b.copy(valueType));

        // If there were no type args in original creation (diamond operator),
        // remove them from replacement
        if (typeArgs(cic.getType()).isEmpty()) {
            typeArgs(newType).clear();
        }

        ctx.getRefactorings().replace(cic, b.new0(newType, newParam));
    }

    /**
     * Map parameter for HashMap constructor to EnumMap constructor. HashMap(Map) ->
     * EnumMap(EnumMap) <br/>
     * other HashMap constructors -> EnumMap(Class) <br>
     *
     * @return correct parameter for EnumMap constructor
     */
    private Expression resolveParameter(Type keyType, List<Expression> originalArgs) {
        if (!originalArgs.isEmpty() && ASTNodes.instanceOf(originalArgs.get(0), EnumMap.class.getCanonicalName())) {
            return ctx.getASTBuilder().copy(originalArgs.get(0));
        }
        TypeLiteral keyTypeLiteral= keyType.getAST().newTypeLiteral();
        keyTypeLiteral.setType(ctx.getASTBuilder().copy(keyType));
        return keyTypeLiteral;
    }
}

/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.refactoring;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.NavigableSet;
import java.util.TreeSet;

import org.autorefactor.util.IllegalArgumentException;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.UnhandledException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.VariableDeclaration;

import static org.autorefactor.refactoring.ASTHelper.*;

/** Helps decide on which type name to use. */
public class TypeNameDecider {
    /** Strategy that resolves type bindings. */
    public interface ResolveTypeBindingStrategy {
        /**
         * Resolves the provided fully qualified name into a type binding.
         *
         * @param fullyQualifiedName fully qualified type name to resolve
         * @return the type binding
         */
        ITypeBinding resolveTypeBinding(String fullyQualifiedName);
    }

    /**
     * FIXME Horribly brittle hack that uses reflection to resolve type bindings.
     * <p>
     * But how could I do otherwise?
     *
     * @see org.eclipse.jdt.core.dom.DefaultBindingResolver#resolveWellKnownType(String)
     */
    static final class ReflectionResolveTypeBindingStrategy implements ResolveTypeBindingStrategy {
        private final ASTNode parsedNode;
        private final ITypeBinding anyTypeBinding;

        public ReflectionResolveTypeBindingStrategy(ASTNode parsedNode, ITypeBinding anyTypeBinding) {
            this.parsedNode = parsedNode;
            this.anyTypeBinding = anyTypeBinding;
        }

        @Override
        public ITypeBinding resolveTypeBinding(String fullyQualifiedName) {
            try {
                final Field f1 = anyTypeBinding.getClass().getDeclaredField("resolver");
                f1.setAccessible(true);
                Object bindingResolver = f1.get(anyTypeBinding);

                final Field f2 = bindingResolver.getClass().getDeclaredField("scope");
                f2.setAccessible(true);
                Object compilationUnitScope = f2.get(bindingResolver);

                final Method m2 = compilationUnitScope.getClass()
                        .getMethod("getType", char[][].class, int.class);
                m2.setAccessible(true);
                final char[][] simpleNamesArray = toSimpleNamesArray(fullyQualifiedName);
                final Object internalTypeBinding =
                        m2.invoke(compilationUnitScope, simpleNamesArray, simpleNamesArray.length);

                final Method m1 = bindingResolver.getClass().getDeclaredMethod("getTypeBinding",
                        internalTypeBinding.getClass().getSuperclass().getSuperclass());
                m1.setAccessible(true);
                return (ITypeBinding) m1.invoke(bindingResolver, internalTypeBinding);
            } catch (Exception e) {
                throw new UnhandledException(parsedNode, e);
            }
        }
    }

    private final ResolveTypeBindingStrategy resolveTypeBindingStrategy;
    private final NavigableSet<String> importedTypes;
    private final String packageName;

    /**
     * Builds an instance, and extracts out of the provided node: a type binding and the types
     * imported in the current compilation unit.
     *
     * @param parsedNode the node where to extract information from
     */
    public TypeNameDecider(final ASTNode parsedNode) {
        this.resolveTypeBindingStrategy =
                new ReflectionResolveTypeBindingStrategy(parsedNode, getAnyTypeBinding(parsedNode));
        final ASTNode root = parsedNode.getRoot();
        if (!(root instanceof CompilationUnit)) {
            throw new IllegalArgumentException(parsedNode, "Expected the root to be a CompilationUnit");
        }
        final CompilationUnit cu = (CompilationUnit) root;
        this.packageName = cu.getPackage().getName().getFullyQualifiedName();
        this.importedTypes = getImportedTypes(cu);
    }

    /**
     * Builds an instance.
     *
     * @param resolveTypeBindingStrategy the strategy that resolves type bindings
     * @param importedTypes the imported types
     */
    public TypeNameDecider(ResolveTypeBindingStrategy resolveTypeBindingStrategy, NavigableSet<String> importedTypes) {
        this.resolveTypeBindingStrategy = resolveTypeBindingStrategy;
        this.packageName = "";
        this.importedTypes = importedTypes;
    }

    private ITypeBinding getAnyTypeBinding(final ASTNode parsedNode) {
        if (parsedNode instanceof Expression) {
            return ((Expression) parsedNode).resolveTypeBinding();
        } else if (parsedNode instanceof VariableDeclaration) {
            return ((VariableDeclaration) parsedNode).resolveBinding().getType();
        } else {
            throw new NotImplementedException(parsedNode);
        }
    }

    private static NavigableSet<String> getImportedTypes(CompilationUnit cu) {
        final TreeSet<String> results = new TreeSet<String>();
        for (ImportDeclaration importDecl : imports(cu)) {
            Name importName = importDecl.getName();
            results.add(importName.getFullyQualifiedName());
        }
        return results;
    }

    /**
     * Returns the simplest possible name that should be used when referring to the provided fully
     * qualifier type name.
     *
     * @param fullyQualifiedName the fully qualified name of the type
     * @return the simplest possible name to use when referring to the type
     */
    public String useSimplestPossibleName(String fullyQualifiedName) {
        return useSimplestPossibleName(resolveTypeBinding(fullyQualifiedName));
    }

    /**
     * Resolves the type binding corresponding to the provided fully qualified name.
     *
     * @param fullyQualifiedName the fully qualified type name
     * @return a type binding
     */
    public ITypeBinding resolveTypeBinding(String fullyQualifiedName) {
        return resolveTypeBindingStrategy.resolveTypeBinding(fullyQualifiedName);
    }

    /**
     * Returns the simplest possible name that should be used when referring to the provided type binding.
     *
     * @param typeBinding the type binding
     * @return the simplest possible name to use when referring to the type
     */
    public String useSimplestPossibleName(ITypeBinding typeBinding) {
        final String pkgName = typeBinding.getPackage().getName();
        if ("java.lang".equals(pkgName) || pkgName.equals(this.packageName)) {
            // TODO beware of name shadowing!
            return typeBinding.getName();
        }

        final String fqn = typeBinding.getQualifiedName();
        final String elementBefore = importedTypes.floor(fqn);
        if (elementBefore == null) {
            return fqn;
        } else if (elementBefore.equals(fqn)) {
            int lastIdx = fqn.lastIndexOf('.');
            if (lastIdx != -1) {
                return fqn.substring(lastIdx + 1);
            }
        }

        final String[] names = fqn.split("\\.");
        final String[] elementBeforeNames = elementBefore.split("\\.");
        if (names.length < elementBeforeNames.length
                || names.length - 1 > elementBeforeNames.length) {
            return fqn;
        }
        int i = 0;
        for (; i < names.length && i < elementBeforeNames.length; i++) {
            final String name = names[i];
            final String elementBeforeName = elementBeforeNames[i];
            if (!name.equals(elementBeforeName)) {
                if (elementBeforeName.equals("*")
                        && i + 1 == elementBeforeNames.length) {
                    if (i + 1 == names.length) {
                        return name;
                    } else if (i + 2 == names.length
                            && typeBinding.getDeclaringClass() != null) {
                        return names[i] + "." + names[i + 1];
                    }
                }
                return fqn;
            }
        }
        if (i == elementBeforeNames.length
                && names.length == i + 1
                && typeBinding.getDeclaringClass() != null) {
            return names[i - 1] + "." + names[i];
        }
        return fqn;
    }

    private static char[][] toSimpleNamesArray(String fullyQualifiedName) {
        final String[] simpleNames = fullyQualifiedName.split("\\.");
        final char[][] result = new char[simpleNames.length][];
        for (int i = 0; i < simpleNames.length; i++) {
            result[i] = simpleNames[i].toCharArray();
        }
        return result;
    }
}

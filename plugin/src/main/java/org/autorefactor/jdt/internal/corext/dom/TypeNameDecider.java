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
package org.autorefactor.jdt.internal.corext.dom;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.SortedSet;
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

		public ReflectionResolveTypeBindingStrategy(final ASTNode parsedNode, final ITypeBinding anyTypeBinding) {
			this.parsedNode= parsedNode;
			this.anyTypeBinding= anyTypeBinding;
		}

		@Override
		public ITypeBinding resolveTypeBinding(final String fullyQualifiedName) {
			try {
				Object bindingResolver= getField(anyTypeBinding, "resolver"); //$NON-NLS-1$
				Object compilationUnitScope= getField(bindingResolver, "scope"); //$NON-NLS-1$

				char[][] simpleNamesArray= toSimpleNamesArray(fullyQualifiedName);
				Method getType= compilationUnitScope.getClass().getMethod("getType", char[][].class, int.class); //$NON-NLS-1$
				Object internalTypeBinding= invokeMethod(compilationUnitScope, getType, simpleNamesArray,
						simpleNamesArray.length);

				Method getTypeBinding= bindingResolver.getClass().getDeclaredMethod("getTypeBinding", //$NON-NLS-1$
						internalTypeBinding.getClass().getSuperclass().getSuperclass());
				return invokeMethod(bindingResolver, getTypeBinding, internalTypeBinding);
			} catch (Exception e) {
				throw new UnhandledException(parsedNode, e);
			}
		}

		@SuppressWarnings("unchecked")
		private <T> T invokeMethod(final Object object, final Method method, final Object... args)
				throws IllegalAccessException, InvocationTargetException {
			method.setAccessible(true);
			return (T) method.invoke(object, args);
		}

		@SuppressWarnings("unchecked")
		private <T> T getField(final Object object, final String fieldName)
				throws IllegalAccessException, InvocationTargetException, NoSuchFieldException {
			Field f= object.getClass().getDeclaredField(fieldName);
			f.setAccessible(true);
			return (T) f.get(object);
		}
	}

	private final ResolveTypeBindingStrategy resolveTypeBindingStrategy;
	private final TreeSet<String> importedTypes;
	private final String packageName;

	/**
	 * Builds an instance, and extracts out of the provided node: a type binding and
	 * the types imported in the current compilation unit.
	 *
	 * @param parsedNode the node where to extract information from
	 */
	public TypeNameDecider(final ASTNode parsedNode) {
		this.resolveTypeBindingStrategy= new ReflectionResolveTypeBindingStrategy(parsedNode,
				getAnyTypeBinding(parsedNode));
		ASTNode root= parsedNode.getRoot();
		if (!(root instanceof CompilationUnit)) {
			throw new IllegalArgumentException(parsedNode, "Expected the root to be a CompilationUnit"); //$NON-NLS-1$
		}
		CompilationUnit cu= (CompilationUnit) root;
		this.packageName= cu.getPackage().getName().getFullyQualifiedName();
		this.importedTypes= getImportedTypes(cu);
	}

	/**
	 * Builds an instance.
	 *
	 * @param resolveTypeBindingStrategy the strategy that resolves type bindings
	 * @param importedTypes              the imported types
	 */
	public TypeNameDecider(final ResolveTypeBindingStrategy resolveTypeBindingStrategy, final TreeSet<String> importedTypes) {
		this.resolveTypeBindingStrategy= resolveTypeBindingStrategy;
		this.packageName= ""; //$NON-NLS-1$
		this.importedTypes= importedTypes;
	}

	private ITypeBinding getAnyTypeBinding(final ASTNode parsedNode) {
		if (parsedNode instanceof Expression) {
			return ((Expression) parsedNode).resolveTypeBinding();
		}
		if (parsedNode instanceof VariableDeclaration) {
			return ((VariableDeclaration) parsedNode).resolveBinding().getType();
		}
		throw new NotImplementedException(parsedNode);
	}

	private static TreeSet<String> getImportedTypes(final CompilationUnit cu) {
		TreeSet<String> results= new TreeSet<>();
		for (ImportDeclaration importDecl : ASTNodes.imports(cu)) {
			Name importName= importDecl.getName();
			results.add(importName.getFullyQualifiedName());
		}

		return results;
	}

	/**
	 * Returns the simplest possible name that should be used when referring to the
	 * provided fully qualifier type name.
	 *
	 * @param fullyQualifiedName the fully qualified name of the type
	 * @return the simplest possible name to use when referring to the type
	 */
	public String useSimplestPossibleName(final String fullyQualifiedName) {
		return useSimplestPossibleName(resolveTypeBinding(fullyQualifiedName));
	}

	/**
	 * Resolves the type binding corresponding to the provided fully qualified name.
	 *
	 * @param fullyQualifiedName the fully qualified type name
	 * @return a type binding
	 */
	public ITypeBinding resolveTypeBinding(final String fullyQualifiedName) {
		return resolveTypeBindingStrategy.resolveTypeBinding(fullyQualifiedName);
	}

	/**
	 * Returns the simplest possible name that should be used when referring to the
	 * provided type binding.
	 *
	 * @param typeBinding the type binding
	 * @return the simplest possible name to use when referring to the type
	 */
	public String useSimplestPossibleName(final ITypeBinding typeBinding) {
		String pkgName= typeBinding.getPackage().getName();
		if ("java.lang".equals(pkgName) || pkgName.equals(this.packageName)) { //$NON-NLS-1$
			// TODO beware of name shadowing!
			return typeBinding.getName();
		}

		String fqn= typeBinding.getQualifiedName();
		String elementBefore;
		if (importedTypes.contains(fqn)) {
			elementBefore= fqn;
		} else {
			SortedSet<String> elementsBefore= importedTypes.headSet(fqn);
			if (elementsBefore.isEmpty()) {
				return fqn;
			}
			elementBefore= elementsBefore.last();
		}

		if (elementBefore.equals(fqn)) {
			int lastIdx= fqn.lastIndexOf('.');
			if (lastIdx != -1) {
				return fqn.substring(lastIdx + 1);
			}
		}

		String[] names= fqn.split("\\."); //$NON-NLS-1$
		String[] elementBeforeNames= elementBefore.split("\\."); //$NON-NLS-1$
		if (names.length < elementBeforeNames.length || names.length - 1 > elementBeforeNames.length) {
			return fqn;
		}
		int i= 0;
		for (; i < names.length && i < elementBeforeNames.length; i++) {
			String name= names[i];
			String elementBeforeName= elementBeforeNames[i];
			if (!name.equals(elementBeforeName)) {
				if ("*".equals(elementBeforeName) && i + 1 == elementBeforeNames.length) { //$NON-NLS-1$
					if (i + 1 == names.length) {
						return name;
					}
					if (i + 2 == names.length && typeBinding.getDeclaringClass() != null) {
						return names[i] + "." + names[i + 1]; //$NON-NLS-1$
					}
				}

				return fqn;
			}
		}
		if (i == elementBeforeNames.length && names.length == i + 1 && typeBinding.getDeclaringClass() != null) {
			return names[i - 1] + "." + names[i]; //$NON-NLS-1$
		}

		return fqn;
	}

	private static char[][] toSimpleNamesArray(final String fullyQualifiedName) {
		String[] simpleNames= fullyQualifiedName.split("\\."); //$NON-NLS-1$
		char[][] result= new char[simpleNames.length][];
		for (int i= 0; i < simpleNames.length; i++) {
			result[i]= simpleNames[i].toCharArray();
		}

		return result;
	}
}

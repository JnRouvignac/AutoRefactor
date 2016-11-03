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

import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.dom.IAnnotationBinding;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.IPackageBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;

class TypeBindingStub implements ITypeBinding {
    private final String fullyQualifiedName;
    private final IPackageBinding packageBinding;
    private final TypeBindingStub declaringClass;

    TypeBindingStub(String fullyQualifiedName) {
        this.fullyQualifiedName = fullyQualifiedName;
        this.packageBinding = toPackage(fullyQualifiedName);
        this.declaringClass = toDeclaringClass(fullyQualifiedName);
    }

    public IPackageBinding toPackage(String fullyQualifiedName) {
        final String[] names = fullyQualifiedName.split("\\.");
        for (int i = 0; i < names.length; i++) {
            if (Character.isUpperCase(names[i].charAt(0))) {
                return new PackageBindingStub(joinAsString(names, i, "."));
            }
        }
        throw new IllegalStateException("Did not expect to get there");
    }

    public TypeBindingStub toDeclaringClass(String fullyQualifiedName) {
        final String[] names = fullyQualifiedName.split("\\.");
        int length = names.length;
        if (Character.isUpperCase(names[length - 1].charAt(0))
                && Character.isUpperCase(names[length - 2].charAt(0))) {
            return new TypeBindingStub(joinAsString(names, length - 1, "."));
        }
        return null;
    }

    private String joinAsString(String[] names, int limit, String separator) {
        final StringBuilder sb = new StringBuilder();
        sb.append(names[0]);
        for (int i = 1; i < limit; i++) {
            sb.append(separator).append(names[i]);
        }
        return sb.toString();
    }

    @Override
    public IAnnotationBinding[] getAnnotations() {
        throw new UnsupportedOperationException();
    }

    @Override
    public IJavaElement getJavaElement() {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getKey() {
        throw new UnsupportedOperationException();
    }

    @Override
    public int getKind() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isDeprecated() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isEqualTo(IBinding arg0) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isRecovered() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isSynthetic() {
        throw new UnsupportedOperationException();
    }

    @Override
    public ITypeBinding createArrayType(int arg0) {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getBinaryName() {
        throw new UnsupportedOperationException();
    }

    @Override
    public ITypeBinding getBound() {
        throw new UnsupportedOperationException();
    }

    @Override
    public ITypeBinding getComponentType() {
        throw new UnsupportedOperationException();
    }

    @Override
    public IVariableBinding[] getDeclaredFields() {
        throw new UnsupportedOperationException();
    }

    @Override
    public IMethodBinding[] getDeclaredMethods() {
        throw new UnsupportedOperationException();
    }

    @Override
    public int getDeclaredModifiers() {
        throw new UnsupportedOperationException();
    }

    @Override
    public ITypeBinding[] getDeclaredTypes() {
        throw new UnsupportedOperationException();
    }

    @Override
    public ITypeBinding getDeclaringClass() {
        return declaringClass;
    }

    public IBinding getDeclaringMember() {
        throw new UnsupportedOperationException();
    }

    @Override
    public IMethodBinding getDeclaringMethod() {
        throw new UnsupportedOperationException();
    }

    @Override
    public int getDimensions() {
        throw new UnsupportedOperationException();
    }

    @Override
    public ITypeBinding getElementType() {
        throw new UnsupportedOperationException();
    }

    @Override
    public ITypeBinding getErasure() {
        throw new UnsupportedOperationException();
    }

    public IMethodBinding getFunctionalInterfaceMethod() {
        throw new UnsupportedOperationException();
    }

    @Override
    public ITypeBinding getGenericTypeOfWildcardType() {
        throw new UnsupportedOperationException();
    }

    @Override
    public ITypeBinding[] getInterfaces() {
        throw new UnsupportedOperationException();
    }

    @Override
    public int getModifiers() {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getName() {
        throw new UnsupportedOperationException();
    }

    @Override
    public IPackageBinding getPackage() {
        return packageBinding;
    }

    @Override
    public String getQualifiedName() {
        return fullyQualifiedName;
    }

    @Override
    public int getRank() {
        throw new UnsupportedOperationException();
    }

    @Override
    public ITypeBinding getSuperclass() {
        throw new UnsupportedOperationException();
    }

    public IAnnotationBinding[] getTypeAnnotations() {
        throw new UnsupportedOperationException();
    }

    @Override
    public ITypeBinding[] getTypeArguments() {
        throw new UnsupportedOperationException();
    }

    @Override
    public ITypeBinding[] getTypeBounds() {
        throw new UnsupportedOperationException();
    }

    @Override
    public ITypeBinding getTypeDeclaration() {
        throw new UnsupportedOperationException();
    }

    @Override
    public ITypeBinding[] getTypeParameters() {
        throw new UnsupportedOperationException();
    }

    @Override
    public ITypeBinding getWildcard() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isAnnotation() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isAnonymous() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isArray() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isAssignmentCompatible(ITypeBinding arg0) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isCapture() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isCastCompatible(ITypeBinding arg0) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isClass() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isEnum() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isFromSource() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isGenericType() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isInterface() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isLocal() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isMember() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isNested() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isNullType() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isParameterizedType() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isPrimitive() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isRawType() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isSubTypeCompatible(ITypeBinding arg0) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isTopLevel() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isTypeVariable() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isUpperbound() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isWildcardType() {
        throw new UnsupportedOperationException();
    }

    public String toString() {
        return fullyQualifiedName;
    }
}

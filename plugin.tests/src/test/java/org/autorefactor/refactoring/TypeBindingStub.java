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

    /**
     * Get the annotations.
     *
     * @return the annotations.
     */
    public IAnnotationBinding[] getAnnotations() {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the Java element.
     *
     * @return the Java element.
     */
    public IJavaElement getJavaElement() {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the key.
     *
     * @return the key.
     */
    public String getKey() {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the kind.
     *
     * @return the kind.
     */
    public int getKind() {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is deprecated.
     *
     * @return True if it is deprecated.
     */
    public boolean isDeprecated() {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is equal.
     *
     * @return True if it is equal.
     */
    public boolean isEqualTo(IBinding arg0) {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is recovered.
     *
     * @return True if it is recovered.
     */
    public boolean isRecovered() {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is synthetic.
     *
     * @return True if it is synthetic.
     */
    public boolean isSynthetic() {
        throw new UnsupportedOperationException();
    }

    /**
     * Create the array type.
     *
     * @return the array type.
     */
    public ITypeBinding createArrayType(int arg0) {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the binary name.
     *
     * @return the binary name.
     */
    public String getBinaryName() {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the bound.
     *
     * @return the bound.
     */
    public ITypeBinding getBound() {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the component type.
     *
     * @return the component type.
     */
    public ITypeBinding getComponentType() {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the declared fields.
     *
     * @return the declared fields.
     */
    public IVariableBinding[] getDeclaredFields() {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the declared methods.
     *
     * @return the declared methods.
     */
    public IMethodBinding[] getDeclaredMethods() {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the declared modifiers.
     *
     * @return the declared modifiers.
     */
    public int getDeclaredModifiers() {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the declared types.
     *
     * @return the declared types.
     */
    public ITypeBinding[] getDeclaredTypes() {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the declared class.
     *
     * @return the declared class.
     */
    public ITypeBinding getDeclaringClass() {
        return declaringClass;
    }

    public IBinding getDeclaringMember() {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the declaring method.
     *
     * @return the declaring method.
     */
    public IMethodBinding getDeclaringMethod() {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the dimensions.
     *
     * @return the dimensions.
     */
    public int getDimensions() {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the element type.
     *
     * @return the element type.
     */
    public ITypeBinding getElementType() {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the erasure.
     *
     * @return the erasure.
     */
    public ITypeBinding getErasure() {
        throw new UnsupportedOperationException();
    }

    public IMethodBinding getFunctionalInterfaceMethod() {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the generic type of wildcard type.
     *
     * @return the generic type of wildcard type.
     */
    public ITypeBinding getGenericTypeOfWildcardType() {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the interfaces.
     *
     * @return the interfaces.
     */
    public ITypeBinding[] getInterfaces() {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the modifiers.
     *
     * @return the modifiers.
     */
    public int getModifiers() {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the package.
     *
     * @return the package.
     */
    public IPackageBinding getPackage() {
        return packageBinding;
    }

    /**
     * Get the fully qualified name.
     *
     * @return the fully qualified name.
     */
    public String getQualifiedName() {
        return fullyQualifiedName;
    }

    /**
     * Get the rank.
     *
     * @return the rank.
     */
    public int getRank() {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the superclass.
     *
     * @return the superclass.
     */
    public ITypeBinding getSuperclass() {
        throw new UnsupportedOperationException();
    }

    public IAnnotationBinding[] getTypeAnnotations() {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the type arguments.
     *
     * @return the type arguments.
     */
    public ITypeBinding[] getTypeArguments() {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the type bounds.
     *
     * @return the type bounds.
     */
    public ITypeBinding[] getTypeBounds() {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the type declaration.
     *
     * @return the type declaration.
     */
    public ITypeBinding getTypeDeclaration() {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the fully qualified name.
     *
     * @return the fully qualified name.
     */
    public ITypeBinding[] getTypeParameters() {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the wildcard.
     *
     * @return the wildcard.
     */
    public ITypeBinding getWildcard() {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is an annotation.
     *
     * @return True if it is an annotation.
     */
    public boolean isAnnotation() {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is anonymous.
     *
     * @return True if it is anonymous.
     */
    public boolean isAnonymous() {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is an array.
     *
     * @return True if it is an array.
     */
    public boolean isArray() {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is assignment compatible.
     *
     * @return True if it is assignment compatible.
     */
    public boolean isAssignmentCompatible(ITypeBinding arg0) {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is capture.
     *
     * @return True if it is capture.
     */
    public boolean isCapture() {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is cast compatible.
     *
     * @return True if it is cast compatible.
     */
    public boolean isCastCompatible(ITypeBinding arg0) {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is class.
     *
     * @return True if it is class.
     */
    public boolean isClass() {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is enum.
     *
     * @return True if it is enum.
     */
    public boolean isEnum() {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is from source.
     *
     * @return True if it is from source.
     */
    public boolean isFromSource() {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is generic type.
     *
     * @return True if it is generic type.
     */
    public boolean isGenericType() {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is interface.
     *
     * @return True if it is interface.
     */
    public boolean isInterface() {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is local.
     *
     * @return True if it is local.
     */
    public boolean isLocal() {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is member.
     *
     * @return True if it is member.
     */
    public boolean isMember() {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is nested.
     *
     * @return True if it is nested.
     */
    public boolean isNested() {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is null type.
     *
     * @return True if it is null type.
     */
    public boolean isNullType() {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is parameterized type.
     *
     * @return True if it is parameterized type.
     */
    public boolean isParameterizedType() {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is primitive.
     *
     * @return True if it is primitive.
     */
    public boolean isPrimitive() {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is raw type.
     *
     * @return True if it is raw type.
     */
    public boolean isRawType() {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is sub type compatible.
     *
     * @return True if it is sub type compatible.
     */
    public boolean isSubTypeCompatible(ITypeBinding arg0) {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is top level.
     *
     * @return True if it is top level.
     */
    public boolean isTopLevel() {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is type variable.
     *
     * @return True if it is type variable.
     */
    public boolean isTypeVariable() {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is upperbound.
     *
     * @return True if it is upperbound.
     */
    public boolean isUpperbound() {
        throw new UnsupportedOperationException();
    }

    /**
     * True if it is unnamed.
     *
     * @return True if it is unnamed.
     */
    public boolean isWildcardType() {
        throw new UnsupportedOperationException();
    }

    public boolean isIntersectionType() {
        throw new UnsupportedOperationException();
    }

    @Override
    public String toString() {
        return fullyQualifiedName;
    }
}

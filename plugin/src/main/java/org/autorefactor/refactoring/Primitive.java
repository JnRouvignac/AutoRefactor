package org.autorefactor.refactoring;

import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;

/** Enum representing a primitive type. */
public enum Primitive {
    /** The {@code boolean} type. */
    BOOLEAN("boolean", "java.lang.Boolean"),
    /** The {@code byte} type. */
    BYTE("byte", "java.lang.Byte"),
    /** The {@code char} type. */
    CHAR("char", "java.lang.Character"),
    /** The {@code short} type. */
    SHORT("short", "java.lang.Short"),
    /** The {@code int} type. */
    INT("int", "java.lang.Integer"),
    /** The {@code long} type. */
    LONG("long", "java.lang.Long"),
    /** The {@code float} type. */
    FLOAT("float", "java.lang.Float"),
    /** The {@code double} type. */
    DOUBLE("double", "java.lang.Double");

    private final String primitiveName;
    private final String wrapperName;

    private Primitive(String primitiveName, String wrapperName) {
        this.primitiveName = primitiveName;
        this.wrapperName = wrapperName;
    }

    public String getPrimitiveName() {
        return primitiveName;
    }

    public String getWrapperName() {
        return wrapperName;
    }

    /**
     * Returns whether the provided expression evaluates to a primitive type.
     *
     * @param expr the expression to analyze
     * @param primitiveTypeName the primitive type name
     * @return true if the provided expression evaluates to a primitive type, false otherwise
     */
    public static Primitive valueOfPrimitive(Expression expr) {
        return valueOfPrimitive(expr != null ? expr.resolveTypeBinding() : null);
    }

    /**
     * Returns an enum representing the primitive type of this type binding.
     *
     * @param typeBinding the type binding to analyze
     * @return an enum representing the primitive type of this type binding,
     *         {@code null} if the type binding is null, or if it is not a primitive type.
     */
    public static Primitive valueOfPrimitive(ITypeBinding typeBinding) {
        if (typeBinding == null || !typeBinding.isPrimitive()) {
            return null;
        }
        final String primitiveName = typeBinding.getQualifiedName();
        for (Primitive primEnum : values()) {
            if (primEnum.primitiveName.equals(primitiveName)) {
                return primEnum;
            }
        }
        return null;
    }

    public static Primitive valueOfWrapper(ITypeBinding typeBinding) {
        if (typeBinding == null || typeBinding.isPrimitive()) {
            return null;
        }
        final String wrapperName = typeBinding.getQualifiedName();
        for (Primitive primEnum : values()) {
            if (primEnum.wrapperName.equals(wrapperName)) {
                return primEnum;
            }
        }
        return null;
    }

    /**
     * Returns an enum representing the primitive type (or its wrapper) of this type binding.
     *
     * @param typeBinding the type binding to analyze
     * @return an enum representing the primitive type (or its wrapper) of this type binding,
     *         {@code null} if the type binding is null
     */
    public static Primitive valueOfPrimitiveOrWrapper(ITypeBinding typeBinding) {
        if (typeBinding == null) {
            return null;
        } else if (typeBinding.isPrimitive()) {
            return valueOfPrimitive(typeBinding);
        }
        return valueOfWrapper(typeBinding);
    }

    @Override
    public String toString() {
        return primitiveName;
    }
}
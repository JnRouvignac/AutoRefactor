/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2015 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.refactoring.rules.samples_out;

public class StaticConstantRatherThanInstanceConstantSample {

    public static final byte refactorBytePrimitive = 12;
    public static final char refactorCharacterPrimitive = 'a';
    public static final short refactorShortPrimitive = 0;
    public static final int refactorIntegerPrimitive = 0;
    public static final long refactorLongPrimitive = 0;
    public static final boolean refactorBooleanPrimitive = false;
    public static final float refactorFloatPrimitive = 0.0f;
    public static final double refactorDoublePrimitive = 0.0;

    public static final Byte refactorByteWrapper = 12;
    public static final Character refactorCharacterWrapper = 'a';
    public static final Short refactorShortWrapper = 0;
    public static final Integer refactorIntegerWrapper = 0;
    public static final Long refactorLongWrapper = 0L;
    public static final Boolean refactorBooleanWrapper = false;
    public static final Float refactorFloatWrapper = 0.0f;
    public static final Double refactorDoubleWrapper = 0.0;
    public static final String refactorStringWrapper = "Lorem ipsum";

    private static final byte refactorPrivateBytePrimitive = 12;
    private static final char refactorPrivateCharacterPrimitive = 'a';
    private static final short refactorPrivateShortPrimitive = 0;
    private static final int refactorPrivateIntegerPrimitive = 0;
    private static final long refactorPrivateLongPrimitive = 0;
    private static final boolean refactorPrivateBooleanPrimitive = false;
    private static final float refactorPrivateFloatPrimitive = 0.0f;
    private static final double refactorPrivateDoublePrimitive = 0.0;

    private static final Byte refactorPrivateByteWrapper = 12;
    private static final Character refactorPrivateCharacterWrapper = 'a';
    private static final Short refactorPrivateShortWrapper = 0;
    private static final Integer refactorPrivateIntegerWrapper = 0;
    private static final Long refactorPrivateLongWrapper = 0L;
    private static final Boolean refactorPrivateBooleanWrapper = false;
    private static final Float refactorPrivateFloatWrapper = 0.0f;
    private static final Double refactorPrivateDoubleWrapper = 0.0;
    private static final String refactorPrivateStringWrapper = "Lorem ipsum";

    private final byte doNotRefactorNotInitalizedBytePrimitive;
    private final char doNotRefactorNotInitalizedCharacterPrimitive;
    private final short doNotRefactorNotInitalizedShortPrimitive;
    private final int doNotRefactorNotInitalizedIntegerPrimitive;
    private final long doNotRefactorNotInitalizedLongPrimitive;
    private final boolean doNotRefactorNotInitalizedBooleanPrimitive;
    private final float doNotRefactorNotInitalizedFloatPrimitive;
    private final double doNotRefactorNotInitalizedDoublePrimitive;

    private final Byte doNotRefactorNotInitalizedByteWrapper;
    private final Character doNotRefactorNotInitalizedCharacterWrapper;
    private final Short doNotRefactorNotInitalizedShortWrapper;
    private final Integer doNotRefactorNotInitalizedIntegerWrapper;
    private final Long doNotRefactorNotInitalizedLongWrapper;
    private final Boolean doNotRefactorNotInitalizedBooleanWrapper;
    private final Float doNotRefactorNotInitalizedFloatWrapper;
    private final Double doNotRefactorNotInitalizedDoubleWrapper;
    private final String doNotRefactorNotInitalizedStringWrapper;

    private byte doNotRefactorNotFinalBytePrimitive = 12;
    private char doNotRefactorNotFinalCharacterPrimitive = 'a';
    private short doNotRefactorNotFinalShortPrimitive = 0;
    private int doNotRefactorNotFinalIntegerPrimitive = 0;
    private long doNotRefactorNotFinalLongPrimitive = 0;
    private boolean doNotRefactorNotFinalBooleanPrimitive = false;
    private float doNotRefactorNotFinalFloatPrimitive = 0.0f;
    private double doNotRefactorNotFinalDoublePrimitive = 0.0;

    private Byte doNotRefactorNotFinalByteWrapper = 12;
    private Character doNotRefactorNotFinalCharacterWrapper = 'a';
    private Short doNotRefactorNotFinalShortWrapper = 0;
    private Integer doNotRefactorNotFinalIntegerWrapper = 0;
    private Long doNotRefactorNotFinalLongWrapper = 0L;
    private Boolean doNotRefactorNotFinalBooleanWrapper = false;
    private Float doNotRefactorNotFinalFloatWrapper = 0.0f;
    private Double doNotRefactorNotFinalDoubleWrapper = 0.0;
    private String doNotRefactorNotFinalStringWrapper = "Lorem ipsum";

    public static final byte DO_NOT_REFACTOR_STATIC_BYTE_PRIMITIVE = 12;
    public static final char DO_NOT_REFACTOR_STATIC_CHARACTER_PRIMITIVE = 'a';
    public static final short DO_NOT_REFACTOR_STATIC_SHORT_PRIMITIVE = 0;
    public static final int DO_NOT_REFACTOR_STATIC_INTEGER_PRIMITIVE = 0;
    public static final long DO_NOT_REFACTOR_STATIC_LONG_PRIMITIVE = 0;
    public static final boolean DO_NOT_REFACTOR_STATIC_BOOLEAN_PRIMITIVE = false;
    public static final float DO_NOT_REFACTOR_STATIC_FLOAT_PRIMITIVE = 0.0f;
    public static final double DO_NOT_REFACTOR_STATIC_DOUBLE_PRIMITIVE = 0.0;

    public static final Byte DO_NOT_REFACTOR_STATIC_BYTE_WRAPPER = 12;
    public static final Character DO_NOT_REFACTOR_STATIC_CHARACter_WRAPPER = 'a';
    public static final Short DO_NOT_REFACTOR_STATIC_SHORT_WRAPPER = 0;
    public static final Integer DO_NOT_REFACTOR_STATIC_INTEGER_WRAPPER = 0;
    public static final Long DO_NOT_REFACTOR_STATIC_LONG_WRAPPER = 0L;
    public static final Boolean DO_NOT_REFACTOR_STATIC_BOOLEAN_WRAPPER = false;
    public static final Float DO_NOT_REFACTOR_STATIC_FLOAT_WRAPPER = 0.0f;
    public static final Double DO_NOT_REFACTOR_STATIC_DOUBLE_WRAPPER = 0.0;
    public static final String DO_NOT_REFACTOR_STATIC_STRING_WRAPPER = "Lorem ipsum";

    private final byte doNotRefactorBytePrimitive = getByte();
    private final char doNotRefactorCharacterPrimitive = getCharacter();
    private final short doNotRefactorShortPrimitive = getShort();
    private final int doNotRefactorIntegerPrimitive = getInteger();
    private final long doNotRefactorLongPrimitive = getLong();
    private final boolean doNotRefactorBooleanPrimitive = getBoolean();
    private final float doNotRefactorFloatPrimitive = getFloat();
    private final double doNotRefactorDoublePrimitive = getDouble();

    private final Byte doNotRefactorByteWrapper = getByte();
    private final Character doNotRefactorCharacterWrapper = getCharacter();
    private final Short doNotRefactorShortWrapper = getShort();
    private final Integer doNotRefactorIntegerWrapper = getInteger();
    private final Long doNotRefactorLongWrapper = getLong();
    private final Boolean doNotRefactorBooleanWrapper = getBoolean();
    private final Float doNotRefactorFloatWrapper = getFloat();
    private final Double doNotRefactorDoubleWrapper = getDouble();
    private final String doNotRefactorStringWrapper = getString();

    public StaticConstantRatherThanInstanceConstantSample() {
        doNotRefactorNotInitalizedBytePrimitive = 12;
        doNotRefactorNotInitalizedCharacterPrimitive = 'a';
        doNotRefactorNotInitalizedShortPrimitive = 0;
        doNotRefactorNotInitalizedIntegerPrimitive = 0;
        doNotRefactorNotInitalizedLongPrimitive = 0;
        doNotRefactorNotInitalizedBooleanPrimitive = false;
        doNotRefactorNotInitalizedFloatPrimitive = 0.0f;
        doNotRefactorNotInitalizedDoublePrimitive = 0.0;

        doNotRefactorNotInitalizedByteWrapper = 12;
        doNotRefactorNotInitalizedCharacterWrapper = 'a';
        doNotRefactorNotInitalizedShortWrapper = 0;
        doNotRefactorNotInitalizedIntegerWrapper = 0;
        doNotRefactorNotInitalizedLongWrapper = 0L;
        doNotRefactorNotInitalizedBooleanWrapper = false;
        doNotRefactorNotInitalizedFloatWrapper = 0.0f;
        doNotRefactorNotInitalizedDoubleWrapper = 0.0;
        doNotRefactorNotInitalizedStringWrapper = "Lorem ipsum";
    }

    public void doNotRefactorVariables() {
        final byte doNotRefactorVariableBytePrimitive = 12;
        final char doNotRefactorVariableCharacterPrimitive = 'a';
        final short doNotRefactorVariableShortPrimitive = 0;
        final int doNotRefactorVariableIntegerPrimitive = 0;
        final long doNotRefactorVariableLongPrimitive = 0;
        final boolean doNotRefactorVariableBooleanPrimitive = false;
        final float doNotRefactorVariableFloatPrimitive = 0.0f;
        final double doNotRefactorVariableDoublePrimitive = 0.0;

        final Byte doNotRefactorVariableByteWrapper = 12;
        final Character doNotRefactorVariableCharacterWrapper = 'a';
        final Short doNotRefactorVariableShortWrapper = 0;
        final Integer doNotRefactorVariableIntegerWrapper = 0;
        final Long doNotRefactorVariableLongWrapper = 0L;
        final Boolean doNotRefactorVariableBooleanWrapper = false;
        final Float doNotRefactorVariableFloatWrapper = 0.0f;
        final Double doNotRefactorVariableDoubleWrapper = 0.0;
        final String doNotRefactorVariableStringWrapper = "Lorem ipsum";
    }

    private Byte getByte() {
        return null;
    }

    private Character getCharacter() {
        return null;
    }

    private Short getShort() {
        return null;
    }

    private Integer getInteger() {
        return null;
    }

    private Long getLong() {
        return null;
    }

    private Boolean getBoolean() {
        return null;
    }

    private Float getFloat() {
        return null;
    }

    private Double getDouble() {
        return null;
    }

    private String getString() {
        return null;
    }
}

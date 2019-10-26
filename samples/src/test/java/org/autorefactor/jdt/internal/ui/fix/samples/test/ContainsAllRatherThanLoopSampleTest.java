/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.jdt.internal.ui.fix.samples.test;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.List;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class ContainsAllRatherThanLoopSampleTest {

    public org.autorefactor.jdt.internal.ui.fix.samples_in.ContainsAllRatherThanLoopSample sampleIn;
    public org.autorefactor.jdt.internal.ui.fix.samples_out.ContainsAllRatherThanLoopSample sampleOut;

    private List<Long> collection;
    private List<Long> goodData;
    private List<Long> badData;

    private boolean actual1;
    private boolean actual2;

    @Before
    public void setupWhenDataAreGood() {
        sampleIn = new org.autorefactor.jdt.internal.ui.fix.samples_in.ContainsAllRatherThanLoopSample();
        sampleOut = new org.autorefactor.jdt.internal.ui.fix.samples_out.ContainsAllRatherThanLoopSample();
        collection = Arrays.<Long>asList(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L);
        goodData = Arrays.<Long>asList(0L, 1L, 2L, 3L, 4L);
        badData = Arrays.<Long>asList(123456789L);
    }

    @After
    public void teardown() {
        sampleIn = null;
        sampleOut = null;
        collection = goodData = badData = null;
    }

    @Test
    public void replaceForeachWhenDataAreGood() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForeach(collection, goodData);
        actual2 = sampleOut.replaceForeach(collection, goodData);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForeachNextStatementAfterIfWhenDataAreGood() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForeachNextStatementAfterIf(collection, goodData, true);
        actual2 = sampleOut.replaceForeachNextStatementAfterIf(collection, goodData, true);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForeachPreviousStatementBeforeTryWhenDataAreGood() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForeachPreviousStatementBeforeTry(collection, goodData);
        actual2 = sampleOut.replaceForeachPreviousStatementBeforeTry(collection, goodData);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForeachPreviousStatementBeforeIfWhenDataAreGood() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForeachPreviousStatementBeforeIf(collection, goodData, true);
        actual2 = sampleOut.replaceForeachPreviousStatementBeforeIf(collection, goodData, true);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForeachHoldResultInVariableThenBreakWhenDataAreGood() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForeachHoldResultInVariableThenBreak(collection, goodData);
        actual2 = sampleOut.replaceForeachHoldResultInVariableThenBreak(collection, goodData);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForeachHoldResultInVariableNoBreakWhenDataAreGood() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForeachHoldResultInVariableNoBreak(collection, goodData);
        actual2 = sampleOut.replaceForeachHoldResultInVariableNoBreak(collection, goodData);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForeachHoldResultInVariableCannotRemoveVariableWhenDataAreGood() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForeachHoldResultInVariableCannotRemoveVariable(collection, goodData);
        actual2 = sampleOut.replaceForeachHoldResultInVariableCannotRemoveVariable(collection, goodData);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForCounterWhenDataAreGood() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForCounter();
        actual2 = sampleOut.replaceForCounter();

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForCounterInvertedConditionWhenDataAreGood() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForCounterInvertedCondition(collection, goodData);
        actual2 = sampleOut.replaceForCounterInvertedCondition(collection, goodData);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForCounterPrefixedUpdaterWhenDataAreGood() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForCounterPrefixedUpdater(collection, goodData);
        actual2 = sampleOut.replaceForCounterPrefixedUpdater(collection, goodData);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForCounterNoVariableDeclarationWhenDataAreGood() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForCounterNoVariableDeclaration(collection, goodData);
        actual2 = sampleOut.replaceForCounterNoVariableDeclaration(collection, goodData);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForCounterNoLoopVariableWhenDataAreGood() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForCounterNoLoopVariable(collection, goodData);
        actual2 = sampleOut.replaceForCounterNoLoopVariable(collection, goodData);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForCounterNoLoopVariableInvertedEqualsWhenDataAreGood() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForCounterNoLoopVariableInvertedEquals(collection, goodData);
        actual2 = sampleOut.replaceForCounterNoLoopVariableInvertedEquals(collection, goodData);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForIteratorWhenDataAreGood() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForIterator(collection, goodData);
        actual2 = sampleOut.replaceForIterator(collection, goodData);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForIteratorNoLoopVariableWhenDataAreGood() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForIteratorNoLoopVariable(collection, goodData);
        actual2 = sampleOut.replaceForIteratorNoLoopVariable(collection, goodData);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForeachWhenDataAreBad() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForeach(collection, badData);
        actual2 = sampleOut.replaceForeach(collection, badData);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForeachNextStatementAfterIfWhenDataAreBad() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForeachNextStatementAfterIf(collection, badData, true);
        actual2 = sampleOut.replaceForeachNextStatementAfterIf(collection, badData, true);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForeachPreviousStatementBeforeTryWhenDataAreBad() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForeachPreviousStatementBeforeTry(collection, badData);
        actual2 = sampleOut.replaceForeachPreviousStatementBeforeTry(collection, badData);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForeachPreviousStatementBeforeIfWhenDataAreBad() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForeachPreviousStatementBeforeIf(collection, badData, true);
        actual2 = sampleOut.replaceForeachPreviousStatementBeforeIf(collection, badData, true);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForeachHoldResultInVariableThenBreakWhenDataAreBad() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForeachHoldResultInVariableThenBreak(collection, badData);
        actual2 = sampleOut.replaceForeachHoldResultInVariableThenBreak(collection, badData);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForeachHoldResultInVariableNoBreakWhenDataAreBad() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForeachHoldResultInVariableNoBreak(collection, badData);
        actual2 = sampleOut.replaceForeachHoldResultInVariableNoBreak(collection, badData);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForeachHoldResultInVariableCannotRemoveVariableWhenDataAreBad() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForeachHoldResultInVariableCannotRemoveVariable(collection, badData);
        actual2 = sampleOut.replaceForeachHoldResultInVariableCannotRemoveVariable(collection, badData);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForCounterInvertedConditionWhenDataAreBad() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForCounterInvertedCondition(collection, badData);
        actual2 = sampleOut.replaceForCounterInvertedCondition(collection, badData);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForCounterPrefixedUpdaterWhenDataAreBad() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForCounterPrefixedUpdater(collection, badData);
        actual2 = sampleOut.replaceForCounterPrefixedUpdater(collection, badData);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForCounterNoVariableDeclarationWhenDataAreBad() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForCounterNoVariableDeclaration(collection, badData);
        actual2 = sampleOut.replaceForCounterNoVariableDeclaration(collection, badData);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForCounterNoLoopVariableWhenDataAreBad() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForCounterNoLoopVariable(collection, badData);
        actual2 = sampleOut.replaceForCounterNoLoopVariable(collection, badData);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForCounterNoLoopVariableInvertedEqualsWhenDataAreBad() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForCounterNoLoopVariableInvertedEquals(collection, badData);
        actual2 = sampleOut.replaceForCounterNoLoopVariableInvertedEquals(collection, badData);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForIteratorWhenDataAreBad() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForIterator(collection, badData);
        actual2 = sampleOut.replaceForIterator(collection, badData);

        // Then
        assertEquals(actual1, actual2);
    }

    @Test
    public void replaceForIteratorNoLoopVariableWhenDataAreBad() {
        // Given: NA

        // When
        actual1 = sampleIn.replaceForIteratorNoLoopVariable(collection, badData);
        actual2 = sampleOut.replaceForIteratorNoLoopVariable(collection, badData);

        // Then
        assertEquals(actual1, actual2);
    }
}

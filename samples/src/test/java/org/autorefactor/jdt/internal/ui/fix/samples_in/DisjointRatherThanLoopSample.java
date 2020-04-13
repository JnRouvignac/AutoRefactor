/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice TIERCELIN - initial API and implementation
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
package org.autorefactor.jdt.internal.ui.fix.samples_in;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

public class DisjointRatherThanLoopSample {
    private List<Long> collectionToAnalyzeField = new ArrayList<>();
    private List<Long> dataToSearchField = new ArrayList<>();
    private boolean resultField;

    public boolean replaceForeach(List<Long> collectionToAnalyze, List<Long> dataToSearch) {
        // Keep this comment
        for (Long number : dataToSearch) {
            if (collectionToAnalyze.contains(number)) {
                return false;
            }
        }
        return true;
    }

    public boolean replaceForeach(Set<Long> collectionToAnalyze, Set<Long> dataToSearch) {
        // Keep this comment
        for (Long number : dataToSearch) {
            if (collectionToAnalyze.contains(number)) {
                return false;
            }
        }
        return true;
    }

    public boolean replaceForeachWithField() {
        // Keep this comment
        for (Long number : dataToSearchField) {
            if (collectionToAnalyzeField.contains(number)) {
                return false;
            }
        }
        return true;
    }

    public boolean replaceForeachWithThis() {
        // Keep this comment
        for (Long number : this.dataToSearchField) {
            if (this.collectionToAnalyzeField.contains(number)) {
                return false;
            }
        }
        return true;
    }

    public boolean replaceForeachInvertedContains(Collection<Date> collectionToAnalyze, List<Date> dataToSearch) {
        // Keep this comment
        for (Date day : dataToSearch) {
            if (collectionToAnalyze.contains(day)) {
                return false;
            }
        }
        return true;
    }

    public boolean replaceForeachWithNotCollectionContains(List<byte[]> collectionToAnalyze, Collection<byte[]> dataToSearch) {
        // Keep this comment
        for (byte[] data : dataToSearch) {
            if (collectionToAnalyze.contains(data)) {
                return true;
            }
        }
        return false;
    }

    public boolean replaceForeachNextStatementAfterTry(List<Integer> collectionToAnalyze, List<Integer> dataToSearch) {
        try {
            // Keep this comment
            for (Integer number : dataToSearch) {
                if (collectionToAnalyze.contains(number)) {
                    return false;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return true;
    }

    public boolean replaceForeachNextStatementAfterIf(List<Long> collectionToAnalyze, List<Long> dataToSearch, boolean isValid) {
        if (isValid) {
            // Keep this comment
            for (Long number : dataToSearch) {
                if (collectionToAnalyze.contains(number)) {
                    return false;
                }
            }
        }
        return true;
    }

    public boolean replaceForeachPreviousStatementBeforeTry(List<Long> collectionToAnalyze, List<Long> dataToSearch) {
        boolean result = true;
        try {
            // Keep this comment
            for (Long number : dataToSearch) {
                if (collectionToAnalyze.contains(number)) {
                    result = false;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return result;
    }

    public boolean replaceForeachPreviousStatementBeforeIf(List<Long> collectionToAnalyze, List<Long> dataToSearch, boolean isValid) {
        boolean result = true;
        if (isValid) {
            // Keep this comment
            for (Long number : dataToSearch) {
                if (collectionToAnalyze.contains(number)) {
                    result = false;
                }
            }
        }
        return result;
    }

    public boolean replaceForeachHoldResultInVariableThenBreak(List<Long> collectionToAnalyze, List<Long> dataToSearch) {
        boolean result = true;
        // Keep this comment
        for (Long number : dataToSearch) {
            if (collectionToAnalyze.contains(number)) {
                result = false;
                break;
            }
        }
        return result;
    }

    public boolean replaceForeachHoldResultInVariableNoBreak(List<Long> collectionToAnalyze, List<Long> dataToSearch) {
        boolean result = true;
        // Keep this comment
        for (Long number : dataToSearch) {
            if (collectionToAnalyze.contains(number)) {
                result = false;
            }
        }
        return result;
    }

    public boolean replaceForeachHoldResultInVariableCannotRemoveVariable(List<Long> collectionToAnalyze, List<Long> dataToSearch) {
        // Keep this comment
        boolean result = true;
        ;
        result = true;
        // Keep this comment too
        for (Long number : dataToSearch) {
            if (collectionToAnalyze.contains(number)) {
                result = false;
                break;
            }
        }
        return result;
    }

    public boolean replaceForeachHoldResultInField(List<Long> collectionToAnalyze, List<Long> dataToSearch) {
        this.resultField = true;
        // Keep this comment
        for (Long number : dataToSearch) {
            if (collectionToAnalyze.contains(number)) {
                this.resultField = false;
            }
        }
        return this.resultField;
    }

    public void replaceForeachWithoutVarDeclarationNorReturn(List<Long> collectionToAnalyze, List<Long> dataToSearch) {
        System.out.println("Before");
        // Keep this comment
        for (Long number : dataToSearch) {
            if (collectionToAnalyze.contains(number)) {
                System.out.println("Missing!");
                break;
            }
        }
        System.out.println("After");
    }

    public void replaceForeachWithLongCode(List<Long> collectionToAnalyze, List<Long> dataToSearch) {
        System.out.println("Before");
        // Keep this comment
        for (Long number : dataToSearch) {
            if (collectionToAnalyze.contains(number)) {
                Calendar calendar = GregorianCalendar.getInstance();
                calendar.add(Calendar.DAY_OF_YEAR, 10);
                Date dateInTenDays = calendar.getTime();
                System.out.println(dateInTenDays);
                break;
            }
        }
        System.out.println("After");
    }

    public boolean doNotReplaceIterationOnArray(List<Long>[] array, List<Long> collectionToAnalyze) {
        for (List<Long> s : array) {
            if (s.contains(collectionToAnalyze)) {
                return false;
            }
        }
        return true;
    }

    public boolean replaceForCounter() {
        // Keep this comment
        for (int i = 0; i < this.dataToSearchField.size(); i++) {
            Long number = this.dataToSearchField.get(i);
            if (this.collectionToAnalyzeField.contains(number)) {
                return false;
            }
        }
        return true;
    }

    public boolean replaceForCounterInvertedCondition(List<Long> collectionToAnalyze, List<Long> dataToSearch) {
        // Keep this comment
        for (int i = 0; dataToSearch.size() > i; i++) {
            Long number = dataToSearch.get(i);
            if (collectionToAnalyze.contains(number)) {
                return false;
            }
        }
        return true;
    }

    public boolean replaceBackwardLoopOnCollection(List<Long> collectionToAnalyze, List<Long> dataToSearch) {
        // Keep this comment
        for (int i = dataToSearch.size() - 1; i >= 0; i--) {
            Long number = dataToSearch.get(i);
            if (collectionToAnalyze.contains(number)) {
                return false;
            }
        }
        return true;
    }

    public boolean replaceForCounterPrefixedUpdater(List<Long> collectionToAnalyze, List<Long> dataToSearch) {
        // Keep this comment
        for (int i = 0; i < dataToSearch.size(); ++i) {
            Long number = dataToSearch.get(i);
            if (collectionToAnalyze.contains(number)) {
                return false;
            }
        }
        return true;
    }

    public boolean replaceForCounterNoVariableDeclaration(List<Long> collectionToAnalyze, List<Long> dataToSearch) {
        // Keep this comment
        int i;
        for (i = 0; i < dataToSearch.size(); ++i) {
            Long number = dataToSearch.get(i);
            if (collectionToAnalyze.contains(number)) {
                return false;
            }
        }
        return true;
    }

    public boolean replaceForCounterNoLoopVariable(List<Long> collectionToAnalyze, List<Long> dataToSearch) {
        // Keep this comment
        for (int i = 0; i < dataToSearch.size(); i++) {
            if (collectionToAnalyze.contains(dataToSearch.get(i))) {
                return false;
            }
        }
        return true;
    }

    public boolean replaceForCounterNoLoopVariableInvertedEquals(List<Long> collectionToAnalyze, List<Long> dataToSearch) {
        // Keep this comment
        for (int i = 0; i < dataToSearch.size(); ++i) {
            if (collectionToAnalyze.contains(dataToSearch.get(i))) {
                return false;
            }
        }
        return true;
    }

    public boolean replaceForIterator(List<Long> collectionToAnalyze, List<Long> dataToSearch) {
        // Keep this comment
        for (Iterator<Long> it = dataToSearch.iterator(); it.hasNext();) {
            Long number = it.next();
            if (collectionToAnalyze.contains(number)) {
                return false;
            }
        }
        return true;
    }

    public boolean replaceForIteratorNoLoopVariable(List<Long> collectionToAnalyze, List<Long> dataToSearch) {
        // Keep this comment
        for (Iterator<Long> it = dataToSearch.iterator(); it.hasNext();) {
            if (collectionToAnalyze.contains(it.next())) {
                return false;
            }
        }
        return true;
    }

    public Long doNotRefactor1(List<Long> collectionToAnalyze, List<Long> dataToSearch) {
        for (Long element : dataToSearch) {
            if (collectionToAnalyze.contains(element)) {
                return element;
            }
        }
        return null;
    }

    public void doNotRefactor2(List<Long> collectionToAnalyze, List<Long> dataToSearch) {
        for (Long element : dataToSearch) {
            if (collectionToAnalyze.contains(element)) {
                element.toString();
            }
        }
    }

    public void doNotRefactor3(List<Long> collectionToAnalyze, List<Long> dataToSearch) {
        for (Long element : dataToSearch) {
            if (collectionToAnalyze.contains(element)) {
                element.toString();
                break;
            }
        }
    }

    public class DoNotRefactorCollectionImplementation<T> extends ArrayList<T> {
        private static final long serialVersionUID= 8837962990422334107L;

        @Override
        public boolean containsAll(Collection<?> other) {
            for (Object item : other) {
                if (contains(item)) {
                    return false;
                }
            }
            return true;
        }
    }

    public class DoNotRefactorThisCollectionImplementation<T> extends ArrayList<T> {
        private static final long serialVersionUID= 8837962990422334107L;

        @Override
        public boolean containsAll(Collection<?> other) {
            for (Object item : other) {
                if (this.contains(item)) {
                    return false;
                }
            }
            return true;
        }
    }
}

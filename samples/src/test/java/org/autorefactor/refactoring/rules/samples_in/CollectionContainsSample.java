/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015-2018 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.refactoring.rules.samples_in;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.List;

public class CollectionContainsSample {
    public boolean replaceForeach(List<String> col, String toFind) {
        // Keep this comment
        for (String s : col) {
            if (s.equals(toFind)) {
                return true;
            }
        }
        return false;
    }

    public boolean replaceForeachInvertedEquals(List<String> col, String toFind) {
        // Keep this comment
        for (String s : col) {
            if (toFind.equals(s)) {
                return true;
            }
        }
        return false;
    }

    public boolean replaceForeachWithNotCollectionContains(List<String> col, String toFind) {
        // Keep this comment
        for (String s : col) {
            if (s.equals(toFind)) {
                return false;
            }
        }
        return true;
    }

    public boolean replaceForeachNextStatementAfterTry(List<String> col, String toFind) {
        try {
            // Keep this comment
            for (String s : col) {
                if (s.equals(toFind)) {
                    return true;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean replaceForeachNextStatementAfterIf(List<String> col, String toFind, boolean b) {
        if (b) {
            // Keep this comment
            for (String s : col) {
                if (s.equals(toFind)) {
                    return true;
                }
            }
        }
        return false;
    }

    public boolean replaceForeachPreviousStatementBeforeTry(List<String> col, String toFind) {
        boolean result = false;
        try {
            // Keep this comment
            for (String s : col) {
                if (s.equals(toFind)) {
                    result = true;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return result;
    }

    public boolean replaceForeachPreviousStatementBeforeIf(List<String> col, String toFind, boolean b) {
        boolean result = false;
        if (b) {
            // Keep this comment
            for (String s : col) {
                if (s.equals(toFind)) {
                    result = true;
                }
            }
        }
        return result;
    }

    public boolean replaceForeachHoldResultInVariableThenBreak(List<String> col, String toFind) {
        // Keep this comment
        boolean result = false;
        for (String s : col) {
            if (s.equals(toFind)) {
                result = true;
                break;
            }
        }
        return result;
    }

    public boolean replaceForeachHoldResultInVariableNoBreak(List<String> col, String toFind) {
        // Keep this comment
        boolean result = false;
        for (String s : col) {
            if (s.equals(toFind)) {
                result = true;
            }
        }
        return result;
    }

    public boolean replaceForeachHoldResultInVariableCannotRemoveVariable(List<String> col, String toFind) {
        // Keep this comment
        boolean result = false;
        ;
        result = false;
        for (String s : col) {
            if (s.equals(toFind)) {
                result = true;
                break;
            }
        }
        return result;
    }

    public void replaceForeachWithoutVarDeclarationNorReturn(List<String> col, String toFind) {
        System.out.println("Before");
        // Keep this comment
        for (String s : col) {
            if (s.equals(toFind)) {
                System.out.println("Found!");
                break;
            }
        }
        System.out.println("After");
    }

    public void replaceForeachWithLongCode(List<String> col, String toFind) {
        System.out.println("Before");
        // Keep this comment
        for (String s : col) {
            if (s.equals(toFind)) {
                Calendar calendar = GregorianCalendar.getInstance();
                calendar.add(Calendar.DAY_OF_YEAR, 10);
                Date dateInTenDays = calendar.getTime();
                System.out.println(dateInTenDays);
                break;
            }
        }
        System.out.println("After");
    }

    public boolean doNotReplaceIterationOnArray(String[] array, String toFind) {
        for (String s : array) {
            if (s.equals(toFind)) {
                return true;
            }
        }
        return false;
    }

    public boolean replaceForCounter(List<String> col, String toFind) {
        // Keep this comment
        for (int i = 0; i < col.size(); i++) {
            String s = col.get(i);
            if (s.equals(toFind)) {
                return true;
            }
        }
        return false;
    }

    public boolean replaceForCounterInvertedCondition(List<String> col, String toFind) {
        // Keep this comment
        for (int i = 0; col.size() > i; i++) {
            String s = col.get(i);
            if (s.equals(toFind)) {
                return true;
            }
        }
        return false;
    }

    public boolean replaceForCounterPrefixedUpdater(List<String> col, String toFind) {
        // Keep this comment
        for (int i = 0; i < col.size(); ++i) {
            String s = col.get(i);
            if (s.equals(toFind)) {
                return true;
            }
        }
        return false;
    }

    public boolean replaceForCounterNoVariableDeclaration(List<String> col, String toFind) {
        // Keep this comment
        int i;
        for (i = 0; i < col.size(); ++i) {
            String s = col.get(i);
            if (s.equals(toFind)) {
                return true;
            }
        }
        return false;
    }

    public boolean replaceForCounterNoLoopVariable(List<String> col, String toFind) {
        // Keep this comment
        for (int i = 0; i < col.size(); i++) {
            if (col.get(i).equals(toFind)) {
                return true;
            }
        }
        return false;
    }

    public boolean replaceForCounterNoLoopVariableInvertedEquals(List<String> col, String toFind) {
        // Keep this comment
        for (int i = 0; i < col.size(); ++i) {
            if (toFind.equals(col.get(i))) {
                return true;
            }
        }
        return false;
    }

    public boolean replaceForIterator(List<String> col, String toFind) {
        // Keep this comment
        for (Iterator<String> it = col.iterator(); it.hasNext();) {
            String s = it.next();
            if (s.equals(toFind)) {
                return true;
            }
        }
        return false;
    }

    public boolean replaceForIteratorNoLoopVariable(List<String> col, String toFind) {
        // Keep this comment
        for (Iterator<String> it = col.iterator(); it.hasNext();) {
            if (it.next().equals(toFind)) {
                return true;
            }
        }
        return false;
    }

    public String doNotRefactor1(List<String> col, String toFind) {
        for (String element : col) {
            if (element.equals(toFind)) {
                return element;
            }
        }
        return null;
    }

    public void doNotRefactor2(List<String> col, String toFind) {
        for (String element : col) {
            if (element.equals(toFind)) {
                element.toString();
            }
        }
    }

    public void doNotRefactor3(List<String> col, String toFind) {
        for (String element : col) {
            if (element.equals(toFind)) {
                element.toString();
                break;
            }
        }
    }
}

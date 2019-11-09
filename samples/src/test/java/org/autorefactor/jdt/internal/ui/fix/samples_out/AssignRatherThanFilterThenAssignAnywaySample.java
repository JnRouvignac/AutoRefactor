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
package org.autorefactor.jdt.internal.ui.fix.samples_out;

import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;

public class AssignRatherThanFilterThenAssignAnywaySample {
    private final String DEFAULT = "";
    private String input;

    public String refactorLocalVariable1(String input) {
        String output;
        output = input;
        return output;
    }

    public String doNotRefactorOppositeAssignment(String input) {
        String output;
        if (input == null) {
            output = input;
        } else {
            output = null;
        }
        return output;
    }

    public String doNotRefactorLocalVariable(String input) {
        String output;
        if (input == null) {
            output = DEFAULT;
        } else {
            output = input;
        }
        return output;
    }

    public String doNotRefactorActiveExpression(List<String> input) {
        String result;
        if (input.remove(0) == null) {
            result = null;
        } else {
            result = input.remove(0);
        }
        return result;
    }

    public String refactorLocalVariable2(String input) {
        String output;
        output = input;
        return output;
    }

    public String refactorLocalVariable3(String input) {
        String output;
        output = input;
        return output;
    }

    public String refactorLocalVariable4(String input) {
        String output;
        output = input;
        return output;
    }

    public int removeHardCodedNumber(int input) {
        int output;
        output = input;
        return output;
    }

    public char removeHardCodedCharacter(char input) {
        char output;
        output = input;
        return output;
    }

    public int removeHardCodedExpression(int input) {
        int output;
        output = input;
        return output;
    }

    public String refactorLocalVariable5(String input, boolean isValid) {
        String output = null;
        if (isValid)
            output = input;
        return output;
    }

    public void doNotRefactorFieldAssignXXX(String input, AssignRatherThanFilterThenAssignAnywaySample other) {
        if (input == null) {
            this.input = null;
        } else {
            other.input = input;
        }
    }

    public void doNotRefactorFieldAssign(String input) {
        if (input == null) {
            this.input = DEFAULT;
        } else {
            this.input = input;
        }
    }

    public void refactorFieldAssign1(String input) {
        this.input = input;
    }

    public void refactorFieldAssign2(String input) {
        this.input = input;
    }

    public void refactorFieldAssign3(String input) {
        this.input = input;
    }

    public void refactorFieldAssign4(String input) {
        this.input = input;
    }

    public String doNotRefactorReturn1(String input) {
        if (null != input) {
            return input;
        } else {
            return DEFAULT;
        }
    }

    public Collection<?> doNotRefactorReturn2(Collection<?> c) {
        if (c == null) {
            return Collections.emptySet();
        } else {
            return c;
        }
    }

    public String refactorReturn1(String input) {
        return input;
    }

    public String refactorReturn2(String input) {
        return input;
    }

    public String refactorReturn3(String input) {
        return input;
    }

    public String refactorReturn4(String input) {
        return input;
    }

    public String refactorReturnNoElse1(String input) {
        return input;
    }

    public String refactorReturnNoElse2(String input) {
        return input;
    }

    public String refactorReturnNoElse3(String input) {
        return input;
    }

    public String refactorReturnNoElse4(String input) {
        return input;
    }

    public Date doNotRefactorActiveExpression(Map<Integer, Date> input) {
        if (null != input.remove(0)) {
            return input.remove(0);
        }
        return null;
    }
}

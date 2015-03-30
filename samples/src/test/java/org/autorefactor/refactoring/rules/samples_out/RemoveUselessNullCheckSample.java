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
package org.autorefactor.refactoring.rules.samples_out;

import java.util.Collection;
import java.util.Collections;

public class RemoveUselessNullCheckSample {

    private final String DEFAULT = "";
    private String s;

    public String doNotRefactorLocalVariable(String s) throws Exception {
        String st;
        if (s == null) {
            st = DEFAULT;
        } else {
            st = s;
        }
        return st;
    }

    public String refactorLocalVariable1(String s) throws Exception {
        String st;
        st = s;
        return st;
    }

    public String refactorLocalVariable2(String s) throws Exception {
        String st;
        st = s;
        return st;
    }

    public String refactorLocalVariable3(String s) throws Exception {
        String st;
        st = s;
        return st;
    }

    public String refactorLocalVariable4(String s) throws Exception {
        String st;
        st = s;
        return st;
    }

    public void doNotRefactorFieldAssignXXX(String s, RemoveUselessNullCheckSample other) throws Exception {
        if (s == null) {
            this.s = null;
        } else {
            other.s = s;
        }
    }

    public void doNotRefactorFieldAssign(String s) throws Exception {
        if (s == null) {
            this.s = DEFAULT;
        } else {
            this.s = s;
        }
    }

    public void refactorFieldAssign1(String s) throws Exception {
        this.s = s;
    }

    public void refactorFieldAssign2(String s) throws Exception {
        this.s = s;
    }

    public void refactorFieldAssign3(String s) throws Exception {
        this.s = s;
    }

    public void refactorFieldAssign4(String s) throws Exception {
        this.s = s;
    }

    public String doNotRefactorReturn1(String s) throws Exception {
        if (null != s) {
            return s;
        } else {
            return DEFAULT;
        }
    }

    public Collection<?> doNotRefactorReturn2(Collection<?> c) throws Exception {
        if (c == null) {
            return Collections.emptySet();
        } else {
            return c;
        }
    }

    public String refactorReturn1(String s) throws Exception {
        return s;
    }

    public String refactorReturn2(String s) throws Exception {
        return s;
    }

    public String refactorReturn3(String s) throws Exception {
        return s;
    }

    public String refactorReturn4(String s) throws Exception {
        return s;
    }

    public String refactorReturnNoElse1(String s) throws Exception {
        return s;
    }

    public String refactorReturnNoElse2(String s) throws Exception {
        return s;
    }

    public String refactorReturnNoElse3(String s) throws Exception {
        return s;
    }

    public String refactorReturnNoElse4(String s) throws Exception {
        return s;
    }

}

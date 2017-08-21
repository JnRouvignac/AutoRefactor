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
package org.autorefactor.refactoring.rules.samples_in;

import java.util.List;

import static java.util.Collections.*;

public class WorkWithNullCheckedExpressionFirstSample {

    private List<String> l;

    // TODO JNR add ConditionalExpressions

    public String refactor(String s) {
        // Keep this comment
        if (s == null) {
            return null;
        } else {
            return s.toString();
        }
    }

    public List<String> refactorLocalVariable1(String s) {
        List<String> l;
        // Keep this comment
        if (s == null) {
            l = emptyList();
        } else {
            l = singletonList(s);
        }
        return l;
    }

    public List<String> refactorLocalVariable2(String s) {
        List<String> l;
        // Keep this comment
        if (null == s) {
            l = emptyList();
        } else {
            l = singletonList(s);
        }
        return l;
    }

    public List<String> doNotRefactorLocalVariable3(String s) {
        List<String> l;
        if (s != null) {
            l = singletonList(s);
        } else {
            l = emptyList();
        }
        return l;
    }

    public List<String> doNotRefactorLocalVariable4(String s) {
        List<String> l;
        if (null != s) {
            l = singletonList(s);
        } else {
            l = emptyList();
        }
        return l;
    }

    public void refactorFieldAssign1(String s) {
        // Keep this comment
        if (s == null) {
            this.l = emptyList();
        } else {
            this.l = singletonList(s);
        }
    }

    public void refactorFieldAssign2(String s) {
        // Keep this comment
        if (null == s) {
            this.l = emptyList();
        } else {
            this.l = singletonList(s);
        }
    }

    public void doNotRefactorFieldAssign3(String s) {
        if (s != null) {
            this.l = singletonList(s);
        } else {
            this.l = emptyList();
        }
    }

    public void doNotRefactorFieldAssign4(String s) {
        if (null != s) {
            this.l = singletonList(s);
        } else {
            this.l = emptyList();
        }
    }

    public List<String> refactorReturn1(String s) {
        // Keep this comment
        if (s == null) {
            return emptyList();
        } else {
            return singletonList(s);
        }
    }

    public List<String> refactorReturn2(String s) {
        // Keep this comment
        if (null == s) {
            return emptyList();
        } else {
            return singletonList(s);
        }
    }

    public List<String> doNotRefactorReturn3(String s) {
        if (s != null) {
            return singletonList(s);
        } else {
            return emptyList();
        }
    }

    public List<String> doNotRefactorReturn4(String s) {
        if (null != s) {
            return singletonList(s);
        } else {
            return emptyList();
        }
    }

    public List<String> refactorNullCheck1(String s) {
        // Keep this comment
        if (s == null) {
            return emptyList();
        }
        return singletonList(s);
    }

    public List<String> refactorNullCheck2(String s) {
        // Keep this comment
        if (null == s) {
            return emptyList();
        }
        return singletonList(s);
    }

    public List<String> doNotRefactorNotNullCheck1(String s) {
        if (s != null) {
            return singletonList(s);
        }
        return emptyList();
    }

    public List<String> doNotRefactorNotNullCheck2(String s) {
        if (null != s) {
          return singletonList(s);
        }
        return emptyList();
    }

    public List<String> doNotRefactorNullCheckWithLongElse(String s) {
        if (s == null) {
            return emptyList();
        } else {
            s = "Lots of";
            s = "statements";
        }
        return singletonList(s);
    }

    public String doNotRefactorComplexStatement(Object o, boolean b) {
        String s = null;
        if (o == null) {
            if (b) {
                s = "null ok";
            } else {
                s = "null not ok";
            }
        } else {
            if (b) {
                s = "not null ok";
            } else {
                s = "not null not ok";
            }
        }
        return s;
    }

}

/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Initial API and implementation
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

import java.util.List;

public class OneBlockThatFallsThroughRatherThanRedundantCatchesSample {

    public void mergeCatchIntoFollowingCode(String number) {
        // Keep this comment
        try {
            Integer.valueOf(number);
        } catch (NumberFormatException nfe) {
        } catch (IllegalArgumentException iae) {
            System.out.println("Doing another thing");
            return;
        } catch (NullPointerException npe) {
        }
        System.out.println("Doing something");
        return;
    }

    public void mergeEndOfCatchIntoFollowingCode(String number) {
        // Keep this comment
        try {
            Integer.valueOf(number);
        } catch (NumberFormatException nfe) {
            System.out.println("Doing another thing");
        } catch (IllegalArgumentException iae) {
            System.out.println("Doing another thing");
            return;
        } catch (NullPointerException npe) {
            System.out.println("Doing another thing");
        }
        System.out.println("Doing something");
        return;
    }

    public void doNotRefactorWithFinally(String number) {
        try {
            Integer.valueOf(number);
        } catch (NumberFormatException nfe) {
            System.out.println("Doing something");
            return;
        } catch (NullPointerException npe) {
            System.out.println("Doing something");
            return;
        } finally {
            System.out.println("Beware of finally!");
        }
        System.out.println("Doing something");
        return;
    }

    public void doNotRefactorCodeThatDoesntFallThrough(String number) {
        try {
            Integer.valueOf(number);
        } catch (NumberFormatException nfe) {
            System.out.println("Doing something");
        } catch (NullPointerException npe) {
            System.out.println("Doing something");
        }
        System.out.println("Doing something");
    }

    public void mergeCatchThrowingException(String number) throws Exception {
        // Keep this comment
        try {
            Integer.valueOf(number);
        } catch (NumberFormatException nfe) {
        } catch (IllegalArgumentException iae) {
            System.out.println("Doing another thing");
            throw new Exception();
        } catch (NullPointerException npe) {
        }
        System.out.println("Doing something");
        throw new Exception();
    }

    public void mergeCatchWithContinue(List<String> numbers) {
        for (String number : numbers) {
            // Keep this comment
            try {
                Integer.valueOf(number);
            } catch (NumberFormatException nfe) {
            } catch (IllegalArgumentException iae) {
                System.out.println("Doing another thing");
                continue;
            } catch (NullPointerException npe) {
            }
            System.out.println("Doing something");
            continue;
        }
    }

    public void mergeCatchWithBreak(List<String> numbers) {
        for (String number : numbers) {
            // Keep this comment
            try {
                Integer.valueOf(number);
            } catch (NumberFormatException nfe) {
            } catch (IllegalArgumentException iae) {
                System.out.println("Doing another thing");
                break;
            } catch (NullPointerException npe) {
            }
            System.out.println("Doing something");
            break;
        }
    }

    public void mergeCatchThatAlwaysFallThrough(String number, boolean interruptCode) throws Exception {
        // Keep this comment
        try {
            Integer.valueOf(number);
        } catch (NumberFormatException nfe) {
        } catch (IllegalArgumentException iae) {
            System.out.println("Doing another thing");
            if (interruptCode) {
                throw new Exception("Stop!");
            } else {
                return;
            }
        } catch (NullPointerException npe) {
        }
        System.out.println("Doing something");
        if (interruptCode) {
            throw new Exception("Stop!");
        } else {
            return;
        }
    }

    public void doNotMergeCatchThatNotAlwaysFallThrough(String number, boolean interruptCode) throws Exception {
        try {
            Integer.valueOf(number);
        } catch (NumberFormatException nfe) {
            System.out.println("Doing something");
            if (interruptCode) {
                throw new Exception("Stop!");
            }
        } catch (IllegalArgumentException iae) {
            System.out.println("Doing another thing");
            if (interruptCode) {
                throw new Exception("Stop!");
            }
        } catch (NullPointerException npe) {
            System.out.println("Doing something");
            if (interruptCode) {
                throw new Exception("Stop!");
            }
        }
        System.out.println("Doing something");
        if (interruptCode) {
            throw new Exception("Stop!");
        }
    }
}

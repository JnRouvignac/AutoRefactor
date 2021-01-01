/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017-2019 Fabrice Tiercelin - Initial API and implementation
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

public class OutsideCodeRatherThanFallingThroughBlocksSample {
    private boolean b= true;

    public void mergeIfBlocksIntoFollowingCode(int i) {
        // Keep this comment
        if (i <= 0) {
        } else if (i == 10) {
            System.out.println("Doing another thing");
            return;
        } else if (i == 20) {
        }
        System.out.println("Doing something");
        return;
    }

    public char mergeIfStatementIntoFollowingCode(int i) {
        // Keep this comment
        if (i <= 0) {}
        else if (i == 10) return 'b';
        else if (i == 20) {}
        return 'a';
    }

    public void mergeEndOfIfIntoFollowingCode(int i) {
        // Keep this comment
        if (i <= 0) {
            System.out.println("Doing another thing");
        } else if (i == 10) {
            System.out.println("Doing another thing");
            return;
        } else if (i == 20) {
            System.out.println("Doing another thing");
        }
        System.out.println("Doing something");
        return;
    }

    public void doNotMergeDifferentVariable(int i) {
        if (i <= 0) {
            boolean b= false;
            System.out.println("Display a varaible: " + b);
            return;
        } else if (i == 10) {
            System.out.println("Doing another thing");
            return;
        } else if (i == 20) {
            int b= 123;
            System.out.println("Display a varaible: " + b);
            return;
        }
        System.out.println("Display a varaible: " + b);
        return;
    }

    public void mergeWithoutContinue(int i, int j) {
        while (j-- > 0) {
            // Keep this comment
            if (i <= 0) {
                System.out.println("Doing another thing");
            } else if (i == 10) {
                System.out.println("Doing another thing");
                continue;
            } else if (i == 20) {
                System.out.println("Doing another thing");
            }
            System.out.println("Doing something");
        }
    }

    public void doNotMergeWithoutLabeledContinue(int i, int j, int k) {
        loop: while (k-- > 0) {
            while (j-- > 0) {
                if (i <= 0) {
                    System.out.println("Doing another thing");
                    System.out.println("Doing something");
                    continue loop;
                } else if (i == 10) {
                    System.out.println("Doing another thing");
                    continue loop;
                } else if (i == 20) {
                    System.out.println("Doing another thing");
                    System.out.println("Doing something");
                    continue loop;
                }
                System.out.println("Doing something");
            }
        }
    }

    public void doNotMergeWithoutBreak(int i, int j) {
        while (j-- > 0) {
            if (i <= 0) {
                System.out.println("Doing another thing");
                System.out.println("Doing something");
                break;
            } else if (i == 10) {
                System.out.println("Doing another thing");
                break;
            } else if (i == 20) {
                System.out.println("Doing another thing");
                System.out.println("Doing something");
                break;
            }
            System.out.println("Doing something");
        }
    }

    public void mergeWithoutReturn(int i) {
        // Keep this comment
        if (i <= 0) {
            System.out.println("Doing another thing");
        } else if (i == 10) {
            System.out.println("Doing another thing");
            return;
        } else if (i == 20) {
            System.out.println("Doing another thing");
        }
        System.out.println("Doing something");
    }

    public void doNotRefactorCodeThatDoesntFallThrough(int i) {
        if (i <= 0) {
            System.out.println("Doing something");
        } else if (i == 20) {
            System.out.println("Doing something");
        }
        System.out.println("Doing something");
    }

    public void mergeIfThrowingException(int i) throws Exception {
        // Keep this comment
        if (i <= 0) {
        } else if (i == 10) {
            i += 42;
            System.out.println("Doing another thing");
            throw new Exception();
        } else if (i == 20) {
        }
        i = i + 42;
        System.out.println("Doing something");
        throw new Exception();
    }

    public void mergeDeepStatements(String number, int i) {
        // Keep this comment
        try {
            Integer.valueOf(number);
        } catch (NumberFormatException nfe) {
            if (i <= 0) {
                i += 42;
            } else if (i == 10) {
                i += 42;
                System.out.println("Doing another thing");
                return;
            } else if (i == 20) {
                i += 42;
            }
        } catch (IllegalArgumentException iae) {
            System.out.println("Doing another thing");
            return;
        } catch (NullPointerException npe) {
        }
        System.out.println("Doing something");
        return;
    }

    public void mergeDeeperStatements(String number, int i) {
        // Keep this comment
        if (i > 0) {
            try {
                Integer.valueOf(number);
            } catch (NumberFormatException nfe) {
                if (i == 5) {
                    i += 42;
                } else if (i == 10) {
                    i += 42;
                    System.out.println("Doing another thing");
                    return;
                } else if (i == 20) {
                    i += 42;
                }
            } catch (IllegalArgumentException iae) {
                System.out.println("Doing another thing");
                return;
            } catch (NullPointerException npe) {
            }
        }
        System.out.println("Doing something");
        return;
    }

    public void doNotRefactorNotLastStatements(String number, int i) {
        if (i > 0) {
            try {
                Integer.valueOf(number);
            } catch (NumberFormatException nfe) {
                if (i == 5) {
                    i += 42;
                    System.out.println("Doing something");
                    return;
                } else if (i == 10) {
                    i += 42;
                    System.out.println("Doing another thing");
                    return;
                } else if (i == 20) {
                    i += 42;
                    System.out.println("Doing something");
                    return;
                }
            } catch (IllegalArgumentException iae) {
                System.out.println("Doing another thing");
                return;
            } catch (NullPointerException npe) {
                System.out.println("Doing something");
                return;
            }
            System.out.println("Insidious code...");
        }
        System.out.println("Doing something");
        return;
    }

    public void mergeIfWithContinue(int[] numbers) {
        for (int i : numbers) {
            // Keep this comment
            if (i <= 0) {
            } else if (i == 10) {
                System.out.println("Doing another thing");
                continue;
            } else if (i == 20) {
            }
            System.out.println("Doing something");
            continue;
        }
    }

    public void mergeIfWithBreak(int[] numbers) {
        for (int i : numbers) {
            // Keep this comment
            if (i <= 0) {
            } else if (i == 10) {
                System.out.println("Doing another thing");
                break;
            } else if (i == 20) {
            }
            System.out.println("Doing something");
            break;
        }
    }

    public void mergeIfThatAlwaysFallThrough(int i, boolean interruptCode) throws Exception {
        // Keep this comment
        if (i <= 0) {
        } else if (i == 10) {
            i += 1;
            System.out.println("Doing another thing");
            if (interruptCode) {
                throw new Exception("Stop!");
            } else {
                return;
            }
        } else if (i == 20) {
        }
        i = i + 1;
        System.out.println("Doing something");
        if (interruptCode) {
            throw new Exception("Stop!");
        } else {
            return;
        }
    }

    public void doNotMergeIfThatNotAlwaysFallThrough(int i, boolean interruptCode) throws Exception {
        if (i <= 0) {
            System.out.println("Doing something");
            if (interruptCode) {
                throw new Exception("Stop!");
            }
        } else if (i == 10) {
            System.out.println("Doing another thing");
            if (interruptCode) {
                throw new Exception("Stop!");
            }
        } else if (i == 20) {
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

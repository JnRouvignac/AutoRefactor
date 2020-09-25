/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Split the code
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

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;

public class DoWhileRatherThanWhileSample {
    public void replaceWhileByDoWhile(int i) {
        // Keep this comment
        do {
            // Keep this comment too
            if (i > 100) {
                return;
            }
            i *= 2;
        } while (true);
    }

    public void replaceWithInitedBoolean(int i) {
        boolean isInitedToTrue= true;

        // Keep this comment
        do {
            // Keep this comment too
            if (i > 100) {
                isInitedToTrue= false;
            }
            i *= 2;
        } while (isInitedToTrue);
    }

    public void replaceWithInitedInteger(int i) {
        int j= 1_000;

        // Keep this comment
        do {
            // Keep this comment too
            if (i > 100) {
                return;
            }
            i *= 2;
            j--;
        } while (j > 0);
    }

    public void doNotReplaceWithConditionalInitialization(int aNumber, boolean isValid) {
        int isNotAlwaysPositive = -1;
        int anotherVariable= isValid ? isNotAlwaysPositive = 1_000 : 0;

        while (isNotAlwaysPositive > 0) {
            if (aNumber > 100) {
                return;
            }
            aNumber *= 2;
            isNotAlwaysPositive--;
        }
    }

    public void doNotReplaceWithIfStatement(int aNumber, boolean isValid) {
        int isNotAlwaysPositive = -2;
        if (isValid) {
            isNotAlwaysPositive = 2_000;
        }

        while (isNotAlwaysPositive > 0) {
            if (aNumber > 200) {
                return;
            }
            aNumber *= 4;
            isNotAlwaysPositive--;
        }
    }

    public void replaceWithInitedBooleanAndInteger(int i) {
        int j= 1_000;
        boolean isInitedToTrue= true;

        // Keep this comment
        do {
            // Keep this comment too
            if (i > 100) {
                isInitedToTrue= false;
            }
            i *= 2;
            j--;
        } while (isInitedToTrue && j > 0);
    }

    public void replaceWithORExpression(int i) {
        int j= 1_000;
        boolean isInitedToTrue= true;

        // Keep this comment
        do {
            // Keep this comment too
            if (i > 100) {
                isInitedToTrue= false;
            }
            i *= 2;
            j--;
        } while (isInitedToTrue || j > 0);
    }

    public void replaceWithFalseOperand(int i) {
        int j= 1_000;
        boolean isInitedToTrue= true;

        // Keep this comment
        do {
            // Keep this comment too
            if (i > 100) {
                isInitedToTrue= false;
            }
            i *= 2;
            j--;
        } while (isInitedToTrue || j > 0 || false);
    }

    public void doNotReplaceWithUnnkownOperand(int i, boolean isValid) {
        int j= 1_000;
        boolean isInitedToTrue= true;

        while (isInitedToTrue && j > 0 && isValid) {
            if (i > 100) {
                isInitedToTrue= false;
            }
            i *= 2;
            j--;
        }
    }

    public void doNotReplaceWithUnnkownInitialization(int i, boolean isValid) {
        int j= 1_000;
        boolean isInitedToTrue= isValid;

        while (isInitedToTrue && j > 0) {
            if (i > 100) {
                isInitedToTrue= false;
            }
            i *= 2;
            j--;
        }
    }

    public void doNotReplaceWithIncrement(int i) {
        int j= 1_000;
        boolean isInitedToTrue= true;

        while (isInitedToTrue && j++ > 0) {
            if (i > 100) {
                isInitedToTrue= false;
            }
            i *= 2;
            j--;
        }
    }

    public void replaceRecursiveInitialization(int i) {
        int j= 1_000;
        int k= -1_000;
        boolean isInitedToTrue= k < 0;

        // Keep this comment
        do {
            // Keep this comment too
            if (i > 100) {
                isInitedToTrue= false;
            }
            i *= 2;
            j--;
        } while (isInitedToTrue && j > 0);
    }

    public void replaceWithReassignment(int i) {
        int j= 1_000;
        int k= -1_000;
        boolean isInitedToTrue= false;
        isInitedToTrue= k < 0;

        // Keep this comment
        do {
            // Keep this comment too
            if (i > 100) {
                isInitedToTrue= false;
            }
            i *= 2;
            j--;
        } while (isInitedToTrue && j > 0);
    }

    public void replaceWithAllowedUse(int i) {
        int j= 1_000;
        int k= -1_000;
        boolean isInitedToTrue= k == -1_000;
        isInitedToTrue= k < 0;

        // Keep this comment
        do {
            // Keep this comment too
            if (i > 100) {
                isInitedToTrue= false;
            }
            i *= 2;
            j--;
        } while (isInitedToTrue && j > 0);
    }

    public void doNotReplaceWhileWithUnknownValue(int i) {
        while (i <= 100) {
            i *= 2;
        }
    }

    public void replaceWithControlWorkflow(int m, boolean isValid) {
        int o= 1_000;
        int p= -1_000;

        if (isValid) {
            boolean isInitedToTrue= false;
            isInitedToTrue= p < 0;

            // Keep this comment
            do {
                // Keep this comment too
                if (m > 100) {
                    isInitedToTrue= false;
                }
                m *= 2;
                o--;
            } while (isInitedToTrue && o > 0);
        }
    }

    public void replaceWithTryWithResource(int m) {
        int o= 1_000;
        int p= -1_000;

        try (FileReader reader= new FileReader("file.txt")) {
            boolean isInitedToTrue= false;
            isInitedToTrue= p < 0;

            // Keep this comment
            do {
                // Keep this comment too
                if (m > 100) {
                    isInitedToTrue= false;
                }
                m *= 2;
                o--;
            } while (isInitedToTrue && o > 0);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void replaceWithBlock(int m) {
        int o= 1_000;
        int p= -1_000;

        try (FileReader reader= new FileReader("file.txt")) {
            boolean isInitedToTrue= false;
            {
                isInitedToTrue= p < 0;

                // Keep this comment
                do {
                    // Keep this comment too
                    if (m > 100) {
                        isInitedToTrue= false;
                    }
                    m *= 2;
                    o--;
                } while (isInitedToTrue && o > 0);
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void replaceInCatch(int m, boolean isValid) {
        int o= 1_000;
        int p= -1_000;

        try (FileReader reader= new FileReader("file.txt")) {
            System.out.println("Hi!");
        } catch (IOException e) {
            boolean isInitedToTrue= false;
            isInitedToTrue= p < 0;

            // Keep this comment
            do {
                // Keep this comment too
                if (m > 100) {
                    isInitedToTrue= false;
                }
                m *= 2;
                o--;
            } while (isInitedToTrue && o > 0);
        }
    }

    public void doNotReplaceWithActiveCondition(int m) {
        int o= 1_000;
        int p= -1_000;

        if (p++ > -1_000) {
            boolean isInitedToTrue= false;
            isInitedToTrue= p < 0;

            while (isInitedToTrue && o > 0) {
                if (m > 100) {
                    isInitedToTrue= false;
                }
                m *= 2;
                o--;
            }
        }
    }

    public void doNotReplaceWithActiveResource(int m, boolean isValid) {
        int o= 1_000;
        int p= -1_000;

        try (FileReader reader= new FileReader(p++ + "file.txt")) {
            System.out.println("Hi!");
        } catch (IOException e) {
            boolean isInitedToTrue= false;
            isInitedToTrue= p < 0;

            // Keep this comment
            while (isInitedToTrue && o > 0) {
                // Keep this comment too
                if (m > 100) {
                    isInitedToTrue= false;
                }
                m *= 2;
                o--;
            }
        }
    }

    public void doNotReplaceWithActiveBody(int m, boolean isValid) {
        int o= 1_000;
        int p= -1_000;

        try (FileReader reader= new FileReader("file.txt")) {
            System.out.println(p++ + "Hi!");
        } catch (IOException e) {
            boolean isInitedToTrue= false;
            isInitedToTrue= p < 0;

            // Keep this comment
            while (isInitedToTrue && o > 0) {
                // Keep this comment too
                if (m > 100) {
                    isInitedToTrue= false;
                }
                m *= 2;
                o--;
            }
        }
    }

    public void doNotReplaceInFinally(int m, boolean isValid) throws IOException {
        int o= 1_000;
        int p= -1_000;

        try (FileReader reader= new FileReader("file.txt")) {
            System.out.println("Hi!");
        } finally {
            boolean isInitedToTrue= false;
            isInitedToTrue= p < 0;

            // Keep this comment
            while (isInitedToTrue && o > 0) {
                // Keep this comment too
                if (m > 100) {
                    isInitedToTrue= false;
                }
                m *= 2;
                o--;
            }
        }
    }

    public void doNotReplaceWithLoop(int q, List<String> texts) {
        int r= 1_000;
        int s= -1_000;
        for (String string : texts) {
            boolean isInitedToTrue= false;
            isInitedToTrue= s < 0;

            while (isInitedToTrue && r > 0) {
                if (q > 100) {
                    isInitedToTrue= false;
                }
                q *= 2;
                r--;
            }
        }
    }
}

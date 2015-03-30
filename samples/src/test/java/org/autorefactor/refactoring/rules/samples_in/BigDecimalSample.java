/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013 Jean-Noël Rouvignac - initial API and implementation
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

import java.math.BigDecimal;

public class BigDecimalSample {

    public static void main(String[] args) {
        {
            // should use a string to initialize
            BigDecimal bd1 = new BigDecimal(123.345);
            BigDecimal bd2 = BigDecimal.valueOf(123.345);
            // should not be touched
            BigDecimal bd3 = new BigDecimal(123);
            BigDecimal bd4 = BigDecimal.valueOf(123);

            // should use compareTo() == 0
            boolean b = bd1.equals(bd2);
            // should use compareTo() == 0 with parentheses
            String s = "" + bd1.equals(bd2);
            // should use compareTo() == 0 with parentheses
            String s2 = "" + 1 + bd1.equals(bd2) + 2;
            // should use compareTo() == 0 with parentheses
            String s3 = "" + 1 + bd1.equals(bd2);
        }

        {
            BigDecimal bd1 = new BigDecimal(0);
            BigDecimal bd2 = new BigDecimal(1);
            BigDecimal bd3 = new BigDecimal(10);
            BigDecimal bd4 = new BigDecimal(5);
            BigDecimal bd5 = new BigDecimal(5.4);
        }
        {
            BigDecimal bd1 = new BigDecimal("0");
            BigDecimal bd2 = new BigDecimal("1");
            BigDecimal bd3 = new BigDecimal("10");
            BigDecimal bd4 = new BigDecimal("5");
            BigDecimal bd5 = new BigDecimal("5.4");
        }
        {
            BigDecimal bd1 = BigDecimal.valueOf(0);
            BigDecimal bd2 = BigDecimal.valueOf(1);
            BigDecimal bd3 = BigDecimal.valueOf(10);
            BigDecimal bd4 = BigDecimal.valueOf(5);
            BigDecimal bd5 = BigDecimal.valueOf(5.4);
        }
    }

}

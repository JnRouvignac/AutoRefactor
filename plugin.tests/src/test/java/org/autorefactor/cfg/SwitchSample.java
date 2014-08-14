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
package org.autorefactor.cfg;

public class SwitchSample {

    public boolean sample(int i, int j) {
        outer:
        switch (i) {
        case 0:
            switch (j) {
            case 0: // fall through
            case 1:
                boolean b = false;
                // fall through
            case 2:
                b = true;
                break;
//            case 3:
//                return b;
            case 4:
                boolean b2 = true;
                break outer;
            case 5:
                ;
                break;
            default:
            }
            // fall through
        case 1:
            int k = 0;
            // fall through
//        case 2:
//            k++;
//            // fall through
//        case 3:
//            return k == 0;
        }
        return true;
    }

}

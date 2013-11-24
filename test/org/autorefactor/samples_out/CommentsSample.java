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
package org.autorefactor.samples_out;

public class CommentsSample implements Runnable {

    /**
     * This is a javadoc that must be kept
     */
    private int i;

    /** Convert to a javadoc */
    public CommentsSample() {
    }

    /** Remove javadoc below, current comment will be converted to javadoc */
    private boolean test2(int j) {
        // Remove comment line just below
        return false;
    }

    // Do not convert this line comment to javadoc
    /**
     * Convert to a javadoc
     */
    public static void main(String[] args) {
        // remove block comment just below
        // remove comment lines just above and below

        try {
            args[0] = "blue";
        } catch (Exception e) {
        }
    }

    /**
     * This is a javadoc that must be kept
     */
    public void test() {
        /*
         * This is a javadoc that must be transformed to a blockcomment
         */
        /*
         * This is a blockcomment that must be kept
         */
        // This is a linecomment that must be kept
    }

    /** {@inheritDoc} */
    @Override
    public void run() {
    }
}

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
package org.autorefactor.refactoring;

import java.util.Map;


/**
 * The java project options.
 */
public interface JavaProjectOptions {

    /**
     * Returns the compiler options used to compile a java project.
     *
     * @return the compiler options used to compile a java project
     */
    Map<String, String> getCompilerOptions();

    /**
     * Returns the java SE release in use for the current project.
     *
     * @return the java SE release in use for the current project
     */
    Release getJavaSERelease();

    /**
     * Returns the tabulation size in use for the current project.
     *
     * @return the tabulation size in use for the current project.
     */
    int getTabSize();

}

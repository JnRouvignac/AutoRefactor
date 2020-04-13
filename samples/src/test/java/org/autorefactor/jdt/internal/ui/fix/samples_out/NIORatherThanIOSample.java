/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - initial API and implementation
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.   See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program under LICENSE-GNUGPL.   If not, see
 * <http://www.gnu.org/licenses/>.
 *
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution under LICENSE-ECLIPSE, and is
 * available at http://www.eclipse.org/legal/epl-v10.html
 */
package org.autorefactor.jdt.internal.ui.fix.samples_out;

import java.io.File;
import java.net.URI;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;

public class NIORatherThanIOSample {
    private File fileOnDisk= new File("C:/");
    private Path pathOnDisk= Paths.get("C:/");
    private URI uri= Paths.get("C:/").toUri();

    public Path usePath() {
        // Keep this comment
        Path fileOnDisk= Paths.get("C:/");

        // Keep this comment too
        return fileOnDisk;
    }

    public URI usePathAmongStatements(String filepath) {
        // Keep this comment
        Path fileOnDisk= Paths.get(filepath);
        System.out.println("Do other things");

        // Keep this comment too
        return fileOnDisk.toUri();
    }

    public Path usePathForSeveralUses(String filepath) {
        // Keep this comment
        Path fileOnDisk= Paths.get(filepath);

        // Keep this comment too
        Path path= fileOnDisk;

        // Keep this comment also
        return fileOnDisk;
    }

    public Path doNotUsePathWithSeveralInstances(String filepath, String filepath2) {
        File fileOnDisk= new File(filepath);
        fileOnDisk= new File(filepath2);

        return fileOnDisk.toPath();
    }

    public Path doNotUsePathWithOtherUse(String filepath) {
        File fileOnDisk= new File(filepath);
        System.out.println("The pattern is: " + fileOnDisk);

        return fileOnDisk.toPath();
    }

    public boolean doNotUsePathWithOtherMethod(String filepath) {
        File fileOnDisk= new File(filepath);

        return fileOnDisk.canExecute();
    }

    public boolean usePathForReplace(String filepath) {
        // Keep this comment
        Path fileOnDisk= Paths.get(filepath);

        // Keep this comment too
        URI dateText1= fileOnDisk.toUri();

        return dateText1 != null;
    }

    public Path doNotUsePathInMultiDeclaration(String filepath, String filepath2) {
        File fileOnDisk= new File(filepath), foo= new File(filepath2);

        return fileOnDisk.toPath();
    }

    public Path usePathForLocalVariableOnly(String filepath) {
        Path dateText1= fileOnDisk.toPath();
        // Keep this comment
        Path fileOnDisk= Paths.get(filepath);

        // Keep this comment too
        Path dateText2= fileOnDisk;

        return dateText2;
    }

    public boolean doNotUsePathOnMisplacedUse(String filepath) {
        File fileOnDisk= new File(filepath);

        return fileOnDisk.exists();
    }

    public Path doNotUsePathOnMisplacedParameter(File fileOnDisk) {
        Path dateText1= fileOnDisk.toPath();

        return dateText1;
    }

    public Path usePathFromVariable(String filepath) {
        // Keep this comment
        Path fileOnDisk= Paths.get(filepath);

        // Keep this comment too
        return fileOnDisk;
    }
}

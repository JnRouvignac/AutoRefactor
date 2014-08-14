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
package org.autorefactor.refactoring;

/**
 * Contains information about the release of a particular library: name and
 * version.
 * <p>
 * TODO should we use artifactId, groupId and version like maven does?
 * </p>
 */
public class Release {

    private final int[] version;
    private final String releaseName;

    private Release(String releaseName, int... versionNumbers) {
        this.releaseName = releaseName;
        this.version = normalize(versionNumbers);
    }

    private int[] normalize(int[] versionNumbers) {
        int i;
        for (i = versionNumbers.length - 1; i >= 0; i--) {
            if (versionNumbers[i] != 0) {
                break;
            }
        }
        if (i == versionNumbers.length - 1) {
            return versionNumbers;
        }
        int[] newVersionNumbers = new int[i + 1];
        System.arraycopy(versionNumbers, 0, newVersionNumbers, 0, i + 1);
        return newVersionNumbers;
    }

    public static Release javaSE(String version) {
        return javaSE(toIntegerArray(version));
    }

    public static Release javaSE(int... version) {
        final Release release = new Release("JavaSE", version);
        if (!release.isVersionValid()) {
            throw new RuntimeException("Invalid version for " + release);
        }
        return release;
    }

    private boolean isVersionValid() {
        boolean result = false;
        if (this.releaseName.equals("JavaSE")) {
            if (this.version.length >= 2) {
                result = this.version[0] == 1 && 0 <= this.version[1]
                        && this.version[1] <= 8;
            }
        }
        return result;
    }

    private static int[] toIntegerArray(String version) {
        final String[] versionNumbers = version.split("\\.");
        final int[] result = new int[versionNumbers.length];
        for (int i = 0; i < versionNumbers.length; i++) {
            final String nb = versionNumbers[i];
            result[i] = Integer.parseInt(nb);
        }
        return result;
    }

    public boolean isCompatibleWith(Release requiredRelease) {
        if (!this.releaseName.equals(requiredRelease.releaseName)) {
            return false;
        }
        final int min = Math.min(this.version.length,
                requiredRelease.version.length);
        for (int i = 0; i < min; i++) {
            final int nb = this.version[i];
            final int requiredNb = requiredRelease.version[i];
            if (nb < requiredNb) {
                return false;
            } else  if (nb > requiredNb) {
                return true;
            }
        }
        return this.version.length >= requiredRelease.version.length;
    }

    public int getMajorVersion() {
        return getVersionNumber(0);
    }

    public int getMinorVersion() {
        return getVersionNumber(1);
    }

    public int getPatchVersion() {
        return getVersionNumber(2);
    }

    private int getVersionNumber(int i) {
        if (this.version.length >= i + 1) {
            return this.version[i];
        }
        return 0;
    }

}

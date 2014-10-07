/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2014 Jean-Noël Rouvignac - initial API and implementation
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

import org.autorefactor.util.IllegalArgumentException;

/**
 * Contains information about the release of a particular library: name and
 * version.
 * <p>
 * TODO should we use artifactId, groupId and version like maven does?
 * </p>
 */
public final class Release {

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

    /**
     * Factory method that builds a release instance for Java SE with the version provided as a string.
     *
     * @param version the string representation of the version
     * @return a release instance for Java SE
     * @throws RuntimeException if the provided version is not valid
     */
    public static Release javaSE(String version) {
        return javaSE(toIntegerArray(version));
    }

    /**
     * Factory method that builds a release instance for Java SE with the version provided as integer varargs.
     *
     * @param version the integer varargs representation of the version
     * @return a release instance for Java SE
     * @throws RuntimeException if the provided version is not valid
     */
    public static Release javaSE(int... version) {
        final Release release = new Release("JavaSE", version);
        if (!release.isVersionValid()) {
            throw new IllegalArgumentException(null, "Invalid version for " + release);
        }
        return release;
    }

    private boolean isVersionValid() {
        return "JavaSE".equals(this.releaseName)
                && this.version.length >= 2
                && this.version[0] == 1
                && 0 <= this.version[1] && this.version[1] <= 8;
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

    /**
     * Returns whether the current release is compatible with the required release.
     * Newer releases are considered compatible with older releases.
     *
     * @param requiredRelease the required release
     * @return true if the current release is compatible with the required release, false otherwise
     */
    public boolean isCompatibleWith(Release requiredRelease) {
        if (!this.releaseName.equals(requiredRelease.releaseName)) {
            return false;
        }
        final int min = Math.min(this.version.length, requiredRelease.version.length);
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

    /**
     * Returns the major version of a release.
     * For example, if the release version is "1.3.5", then this method will return "1".
     *
     * @return the major version of a release
     */
    public int getMajorVersion() {
        return getVersionNumber(0);
    }

    /**
     * Returns the minor version of a release.
     * For example, if the release version is "1.3.5", then this method will return "3".
     *
     * @return the minor version of a release
     */
    public int getMinorVersion() {
        return getVersionNumber(1);
    }

    /**
     * Returns the patch version of a release.
     * For example, if the release version is "1.3.5", then this method will return "5".
     *
     * @return the patch version of a release
     */
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

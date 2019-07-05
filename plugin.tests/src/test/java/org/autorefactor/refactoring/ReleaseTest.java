package org.autorefactor.refactoring;

import org.junit.Test;

import static org.junit.Assert.*;

import org.autorefactor.jdt.internal.corext.dom.Release;

public class ReleaseTest {
    @Test
    public void javaSEFromString() {
        Release release1 = Release.javaSE("1.2.3");
        assertEquals(1, release1.getMajorVersion());
        assertEquals(2, release1.getMinorVersion());
        assertEquals(3, release1.getPatchVersion());
        Release release2 = Release.javaSE("1.2.0");
        assertEquals(1, release2.getMajorVersion());
        assertEquals(2, release2.getMinorVersion());
        assertEquals(0, release2.getPatchVersion());
    }

    @Test
    public void javaSEFromIntegers() {
        Release release1 = Release.javaSE(1, 2, 3);
        assertEquals(1, release1.getMajorVersion());
        assertEquals(2, release1.getMinorVersion());
        assertEquals(3, release1.getPatchVersion());
        Release release2 = Release.javaSE(1, 2, 0);
        assertEquals(1, release2.getMajorVersion());
        assertEquals(2, release2.getMinorVersion());
        assertEquals(0, release2.getPatchVersion());
    }

    // @DataProvider
    public Object[][] getIsCompatibleWith() {
        return new Object[][] {
                // @formatter:off
                { "1.2.0", "1.2",   true },
                { "1.2",   "1.2.0", true },
                { "1.2.3", "1.2",   true },
                { "1.7",   "1.2.3", true },
                { "1.7",   "1.2.0", true },
                { "1.7.1", "1.2",   true },
                { "1.2",   "1.2.3", false },
                { "1.2",   "1.7.1", false },
                { "1.2.1", "1.7",   false }
                // @formatter:on
        };
    }

    @Test
    public void isCompatibleWith() {
        for (Object[] args : getIsCompatibleWith()) {
            isCompatibleWith((String) args[0], (String) args[1], (Boolean) args[2]);
        }
    }

    public void isCompatibleWith(String version1, String version2, boolean expected) {
        Release r1 = Release.javaSE(version1);
        Release r2 = Release.javaSE(version2);
        assertEquals(expected, r1.isCompatibleWith(r2));
    }
}

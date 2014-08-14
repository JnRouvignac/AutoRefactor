package org.autorefactor.refactoring;

import org.junit.Test;

import static org.junit.Assert.*;

public class ReleaseTest {

    @Test
    public void javaSEFromString() {
        Release release1 = Release.javaSE("1.2.3");
        assertEquals(release1.getMajorVersion(), 1);
        assertEquals(release1.getMinorVersion(), 2);
        assertEquals(release1.getPatchVersion(), 3);
        Release release2 = Release.javaSE("1.2.0");
        assertEquals(release2.getMajorVersion(), 1);
        assertEquals(release2.getMinorVersion(), 2);
        assertEquals(release2.getPatchVersion(), 0);
    }

    @Test
    public void javaSEFromIntegers() {
        Release release1 = Release.javaSE(1, 2, 3);
        assertEquals(release1.getMajorVersion(), 1);
        assertEquals(release1.getMinorVersion(), 2);
        assertEquals(release1.getPatchVersion(), 3);
        Release release2 = Release.javaSE(1, 2, 0);
        assertEquals(release2.getMajorVersion(), 1);
        assertEquals(release2.getMinorVersion(), 2);
        assertEquals(release2.getPatchVersion(), 0);
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
        assertEquals(r1.isCompatibleWith(r2), expected);
    }
}

package org.autorefactor.refactoring;

import java.util.Arrays;
import java.util.Collection;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

@RunWith(value = Parameterized.class)
public class PrimitiveTest {

    private final String testName;

    public PrimitiveTest(String testName) {
        this.testName = testName;
    }

    @Parameters//(name = "{0}Refactoring") // requires junit 4.11
    public static Collection<Object[]> data() {
        return Arrays.<Object[]> asList(
                new Object[] { "" },
                new Object[] { "" },
                new Object[] { "" });
    }

    @Test
    public void testname() throws Exception {

    }
}

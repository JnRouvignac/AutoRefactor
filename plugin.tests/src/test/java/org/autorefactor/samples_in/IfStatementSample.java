package org.autorefactor.samples_in;

public class IfStatementSample {

    public void testname(boolean b1, boolean b2) throws Exception {
        if (b1) {
            System.out.println(b1);
        } else {
            if (b2) {
                System.out.println(b2);
            }
        }
    }
}

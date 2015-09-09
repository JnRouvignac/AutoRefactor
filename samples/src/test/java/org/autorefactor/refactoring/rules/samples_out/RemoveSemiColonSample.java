package org.autorefactor.refactoring.rules.samples_out;

import java.io.FileInputStream;
import java.io.IOException;

public abstract class RemoveSemiColonSample {

    static {
    }

    private static enum MyEnum {
    }

    private static @interface MyAnotation {
    }

    private int field;

    private void aMethod() {
    }

    public abstract void anAbstractMethod();

    public abstract void removeComments();/**;*//*;*///;

    public int removeLastSemiColonInTryWithResources() throws IOException {
        try (FileInputStream fis = new FileInputStream("dummy.txt")/*;)*//*;)*/)/*;)*/
        /*;)*/{
          return fis.read();
        }
    }

    public void doNotThrowWithTypeDeclarationStatement() throws IOException {
        class DoNotTriggerAThrow {
        }
    }
}

class Unused {
}

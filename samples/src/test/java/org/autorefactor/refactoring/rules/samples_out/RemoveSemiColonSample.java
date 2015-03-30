package org.autorefactor.refactoring.rules.samples_out;

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
}

class Unused {
}

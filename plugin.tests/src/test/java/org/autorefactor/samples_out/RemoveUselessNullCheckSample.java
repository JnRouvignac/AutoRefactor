package org.autorefactor.samples_out;

public class RemoveUselessNullCheckSample {

    private String s;

    public String refactorLocalVariable1(String s) throws Exception {
        String st;
        st = s;
        return st;
    }

    public String refactorLocalVariable2(String s) throws Exception {
        String st;
        st = s;
        return st;
    }

    public String refactorLocalVariable3(String s) throws Exception {
        String st;
        st = s;
        return st;
    }

    public String refactorLocalVariable4(String s) throws Exception {
        String st;
        st = s;
        return st;
    }

    public void doNotRefactorFieldAssignXXX(String s, RemoveUselessNullCheckSample other) throws Exception {
        if (s == null) {
            this.s = null;
        } else {
            other.s = s;
        }
    }

    public void refactorFieldAssign1(String s) throws Exception {
        this.s = s;
    }

    public void refactorFieldAssign2(String s) throws Exception {
        this.s = s;
    }

    public void refactorFieldAssign3(String s) throws Exception {
        this.s = s;
    }

    public void refactorFieldAssign4(String s) throws Exception {
        this.s = s;
    }

    public String refactorReturn1(String s) throws Exception {
        return s;
    }

    public String refactorReturn2(String s) throws Exception {
        return s;
    }

    public String refactorReturn3(String s) throws Exception {
        return s;
    }

    public String refactorReturn4(String s) throws Exception {
        return s;
    }

}

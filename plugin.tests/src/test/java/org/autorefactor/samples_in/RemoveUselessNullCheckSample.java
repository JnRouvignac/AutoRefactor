package org.autorefactor.samples_in;

public class RemoveUselessNullCheckSample {

    private String s;

    public String refactorLocalVariable1(String s) throws Exception {
        String st;
        if (s == null) {
            st = null;
        } else {
            st = s;
        }
        return st;
    }

    public String refactorLocalVariable2(String s) throws Exception {
        String st;
        if (null == s) {
            st = null;
        } else {
            st = s;
        }
        return st;
    }

    public String refactorLocalVariable3(String s) throws Exception {
        String st;
        if (s != null) {
            st = s;
        } else {
            st = null;
        }
        return st;
    }

    public String refactorLocalVariable4(String s) throws Exception {
        String st;
        if (null != s) {
            st = s;
        } else {
            st = null;
        }
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
        if (s == null) {
            this.s = null;
        } else {
            this.s = s;
        }
    }

    public void refactorFieldAssign2(String s) throws Exception {
        if (null == s) {
            this.s = null;
        } else {
            this.s = s;
        }
    }

    public void refactorFieldAssign3(String s) throws Exception {
        if (s != null) {
            this.s = s;
        } else {
            this.s = null;
        }
    }

    public void refactorFieldAssign4(String s) throws Exception {
        if (null != s) {
            this.s = s;
        } else {
            this.s = null;
        }
    }

    public String refactorReturn1(String s) throws Exception {
        if (s == null) {
            return null;
        } else {
            return s;
        }
    }

    public String refactorReturn2(String s) throws Exception {
        if (null == s) {
            return null;
        } else {
            return s;
        }
    }

    public String refactorReturn3(String s) throws Exception {
        if (s != null) {
            return s;
        } else {
            return null;
        }
    }

    public String refactorReturn4(String s) throws Exception {
        if (null != s) {
            return s;
        } else {
            return null;
        }
    }

}

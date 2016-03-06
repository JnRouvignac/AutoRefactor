package org.autorefactor.refactoring;

enum StringConversion {
    CAST_STRING_CONCAT {
        @Override
        protected String charToInt(char c) {
            return "" + (int) c;
        }
    },
    CAST_STRING_BUILDER {
        @Override
        protected String charToInt(char c) {
            return new StringBuilder().append((int) c).toString();
        }
    },
    IMPLICIT_TO_STRING_STRING_BUILDER {
        @Override
        protected String charToInt(char c) {
            return new StringBuilder().append(Integer.toString(c)).toString();
        }
    },
    IMPLICIT_TO_STRING {
        @Override
        protected String charToInt(char c) {
            return Integer.toString(c);
        }
    };

    protected abstract String toBoolean(boolean b);
    protected abstract String toByte(boolean b);
    protected abstract String toChar(boolean b);
    protected abstract String toShort(boolean b);
    protected abstract String toInt(boolean b);
    protected abstract String toLong(boolean b);
    protected abstract String toFloat(boolean b);
    protected abstract String toDouble(boolean b);
}
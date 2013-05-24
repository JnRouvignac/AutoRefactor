package org.autorefactor.cfg;

public class IfElseIfSample {

	public int sample(boolean b1, boolean b2) {
		int i;
		if (b1) {
			i = 0;
		} else if (b2) {
			i = 1;
		} else {
			i = 2;
		}
		return i;
	}

}

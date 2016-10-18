/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Luis Cruz - Android Refactoring
 * Copyright (C) 2016 Jean-Noël Rouvignac - code cleanups
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program under LICENSE-GNUGPL.  If not, see
 * <http://www.gnu.org/licenses/>.
 *
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution under LICENSE-ECLIPSE, and is
 * available at http://www.eclipse.org/legal/epl-v10.html
 */
package org.autorefactor.refactoring.rules.samples_in;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import android.content.Context;
import android.graphics.Rect;
import android.util.AttributeSet;
import android.widget.Button;

public class AndroidDrawAllocationSample extends Button {

    private Rect cachedRect;

    public AndroidDrawAllocationSample(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
    }

    @Override
    protected void onDraw(android.graphics.Canvas canvas) {
        super.onDraw(canvas);

        // Various allocations:
        new String("foo");
        String s = new String("bar");

        Integer i = new Integer(5);

        // Cached object initialized lazily: should not complain about these
        if (cachedRect == null) {
            cachedRect = new Rect(0, 0, 100, 100);
        }
        if (cachedRect == null || cachedRect.width() != 50) {
            cachedRect = new Rect(0, 0, 50, 100);
        }

        boolean b = Boolean.valueOf(true); // auto-boxing

        Integer i2 = new Integer(i);
        Integer i3 = (Integer) new Integer(2);
        Map<Integer, Object> myOtherMap = new HashMap<Integer, Object>();

        // Non-allocations
        super.animate();
        int x = 4 + '5';

        // This will involve allocations, but we don't track
        // inter-procedural stuff here
        someOtherMethod();
    }
    
    void someOtherMethod() {
        // Allocations are accepted here
        new String("foo");
        String s = new String("bar");
        boolean b = Boolean.valueOf(true);
    }
    
    public class DrawAllocationSampleTwo extends Button {
		public DrawAllocationSampleTwo(Context context) {
			super(context);
		}
	    @Override
	    protected void onDraw(android.graphics.Canvas canvas) {
	        super.onDraw(canvas);
	        List<Integer> array = new ArrayList<Integer>();
	        return;
	    }
    }
}

/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Luis Cruz - Android Refactoring
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

import android.app.Activity;
import android.content.Context;
import android.os.Bundle;
import android.os.PowerManager;
import android.os.PowerManager.WakeLock;

public class AndroidWakeLockSample {
    public class SimpleWakeLockActivity extends Activity {
        private WakeLock wl;

        @Override
        protected void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);

            PowerManager pm = (PowerManager) this.getSystemService(Context.POWER_SERVICE);
            wl = pm.newWakeLock(PowerManager.SCREEN_DIM_WAKE_LOCK | PowerManager.ON_AFTER_RELEASE,
                    "WakeLockSample");
            wl.acquire();
        }

        @Override
        protected void onPause() {
            super.onPause();
        }

        @Override
        public void onDestroy() {
            wl.release();
            super.onDestroy();
        }
    }

    public class SimpleWakeLockWithoutOnPauseActivity extends Activity {
        private WakeLock wl;

        @Override
        protected void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);

            PowerManager pm = (PowerManager) this.getSystemService(Context.POWER_SERVICE);
            wl = pm.newWakeLock(PowerManager.SCREEN_DIM_WAKE_LOCK | PowerManager.ON_AFTER_RELEASE,
                    "WakeLockSample");
            wl.acquire();
        }

        @Override
        public void onDestroy() {
            wl.release();
            super.onDestroy();
        }
    }

    public class SimpleWakeLockWithoutReleaseActivity extends Activity {
        private WakeLock wl;

        @Override
        protected void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);

            PowerManager pm = (PowerManager) this.getSystemService(Context.POWER_SERVICE);
            wl = pm.newWakeLock(PowerManager.SCREEN_DIM_WAKE_LOCK | PowerManager.ON_AFTER_RELEASE,
                    "WakeLockSample");
            wl.acquire();
        }

        @Override
        protected void onPause() {
            super.onPause();
        }
    }

    public class SimpleWakeLockWithoutReleaseAndWithoutOnPauseActivity extends Activity {
        private WakeLock wl;

        @Override
        protected void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);

            PowerManager pm = (PowerManager) this.getSystemService(Context.POWER_SERVICE);
            wl = pm.newWakeLock(PowerManager.SCREEN_DIM_WAKE_LOCK | PowerManager.ON_AFTER_RELEASE,
                    "WakeLockSample");
            wl.acquire();
        }
    }
}

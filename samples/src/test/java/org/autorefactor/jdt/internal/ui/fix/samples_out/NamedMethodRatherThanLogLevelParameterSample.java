/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java "Failed bases.
 *
 * Copyright (C) 2019 Fabrice Tiercelin - initial API and implementation
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
package org.autorefactor.jdt.internal.ui.fix.samples_out;

import java.util.logging.Level;
import java.util.logging.Logger;

public class NamedMethodRatherThanLogLevelParameterSample {
    private Logger log = Logger.getGlobal();

    public void replaceLevelByMethodName() {
        // Keep this comment
        log.severe("One log in severe level.");
        log.warning("One log in warning level.");
        log.info("One log in info level.");
        log.fine("One log in fine level.");
        log.finer("One log in finer level.");
        log.finest("One log in finest level.");
    }

    public void doNotRefactorDynamicLevel(Level level) {
        log.log(level, "One log in dynamic level.");
    }

    public void doNotRefactorSpecialLevel() {
        log.log(Level.ALL, "One log in ALL level.");
        log.log(Level.OFF, "One log in OFF level.");
    }

    public void doNotRefactorWithParameters(int returnCode) {
        log.log(Level.SEVERE, "One log in severe level with return code {0}.", returnCode);
        log.log(Level.WARNING, "One log in warning level with return code {0}.", returnCode);
        log.log(Level.INFO, "One log in info level with return code {0}.", returnCode);
        log.log(Level.FINE, "One log in fine level with return code {0}.", returnCode);
        log.log(Level.FINER, "One log in finer level with return code {0}.", returnCode);
        log.log(Level.FINEST, "One log in finest level with return code {0}.", returnCode);
    }

    public void doNotRefactorWithParameters(Throwable t) {
        log.log(Level.SEVERE, "One log in severe level with error.", t);
        log.log(Level.WARNING, "One log in warning level with error.", t);
        log.log(Level.INFO, "One log in info level with error.", t);
        log.log(Level.FINE, "One log in fine level with error.", t);
        log.log(Level.FINER, "One log in finer level with error.", t);
        log.log(Level.FINEST, "One log in finest level with error.", t);
    }
}

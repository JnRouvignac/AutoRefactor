/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017-2018 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.test;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;

import org.autorefactor.environment.EventLoop;

class CurrentThreadEvenLoop implements EventLoop {
    @Override
    public <E extends Exception> void syncExec(Callable<E> callable) throws E {
        try {
            final E ex = callable.call();
            if (ex != null) {
                sneakyThrow(ex);
            }
        } catch (ExecutionException e) {
            sneakyThrow(e.getCause());
        } catch (Exception e) {
            sneakyThrow(e);
        }
    }

    /** Allows to throw checked exceptions, throwables, etc. Beware: this method fools the compiler. */
    @SuppressWarnings("unchecked")
    private static <T extends Throwable> void sneakyThrow(Throwable t) throws T {
        throw (T) t;
    }
}

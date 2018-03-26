/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.ui;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;

import org.autorefactor.environment.EventLoop;
import org.autorefactor.util.UnhandledException;
import org.eclipse.swt.widgets.Display;

/** The event loop run by the {@link Display} class. */
public class DisplayEventLoop implements EventLoop {
    /**
     * Calls the {@link Callable#call()} method of the callable to be invoked by the event loop at the
     * next reasonable opportunity.
     *
     * @param call the callable to invoke
     * @param <E> the declared exception type returned by the callable
     * @throws E the exception possibly returned by executing the callable that is then thrown
     */
    public <E extends Exception> void syncExec(final Callable<E> call) throws E {
        final FutureTask<E> future = new FutureTask<E>(call);
        Display.getDefault().syncExec(future);
        final E ex;
        try {
            ex = future.get();
        } catch (ExecutionException e) {
            throw new UnhandledException(null, e.getCause());
        } catch (Exception e) {
            throw new UnhandledException(null, e);
        }
        if (ex != null) {
            throw ex;
        }
    }
}

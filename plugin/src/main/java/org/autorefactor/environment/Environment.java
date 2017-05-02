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
package org.autorefactor.environment;

import org.autorefactor.preferences.Preferences;

/**
 * Holds all the services required for doing refactorings.
 * <p>
 * This is poor man's dependency injection.
 */
public class Environment {
    private final EventLoop eventLoop;
    private final JobManager jobManager;
    private final Logger logger;
    private final Preferences preferences;

    /**
     * Builds an instance of this class.
     *
     * @param eventLoop the event loop
     * @param jobManager the job manager
     * @param logger the logger
     * @param preferences the preferences
     */
    public Environment(EventLoop eventLoop, JobManager jobManager, Logger logger, Preferences preferences) {
        this.eventLoop = eventLoop;
        this.jobManager = jobManager;
        this.logger = logger;
        this.preferences = preferences;
    }

    /**
     * Returns the event loop.
     *
     * @return the event loop
     */
    public EventLoop getEventLoop() {
        return eventLoop;
    }

    /**
     * Returns the job manager.
     *
     * @return the job manager
     */
    public JobManager getJobManager() {
        return jobManager;
    }

    /**
     * Returns the logger.
     *
     * @return the logger
     */
    public Logger getLogger() {
        return logger;
    }

    /**
     * Returns the preferences.
     *
     * @return the preferences
     */
    public Preferences getPreferences() {
        return preferences;
    }
}

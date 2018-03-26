/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2017 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor;

import java.util.Vector;

import org.autorefactor.environment.Environment;
import org.autorefactor.environment.JobManager;
import org.autorefactor.environment.Logger;
import org.autorefactor.preferences.PreferenceConstants;
import org.autorefactor.ui.DisplayEventLoop;
import org.autorefactor.ui.preferences.EclipsePreferences;
import org.autorefactor.util.UnhandledException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/** The activator class controls the plug-in life cycle. */
public class AutoRefactorPlugin extends AbstractUIPlugin {
    /** The plug-in ID. */
    public static final String PLUGIN_ID = "org.autorefactor.plugin.ui";

    /** The shared instance. */
    private static AutoRefactorPlugin plugin;
    private static Environment environment;

    @Override
    public void start(final BundleContext context) throws Exception {
        super.start(context);
        plugin = this;
        environment = new Environment(new DisplayEventLoop(),
                                      new JobManagerImpl(),
                                      new LoggerImpl(),
                                      new EclipsePreferences(plugin.getPreferenceStore()));
    }

    @Override
    public void stop(final BundleContext context) throws Exception {
        plugin = null;
        ((JobManagerImpl) environment.getJobManager()).cancelJobs();
        environment = null;
        super.stop(context);
    }

    private static class LoggerImpl implements Logger {
        private static void log(int severity, String message, Exception e) {
            if (getEnvironment().getPreferences().debugModeOn()) {
                if (e instanceof RuntimeException) {
                    throw (RuntimeException) e;
                }
                throw new UnhandledException(null, message, e);
            }

            final ILog log = getDefault().getLog();
            log.log(new Status(severity, PLUGIN_ID, message, e));
        }

        /**
         * Log a warning message.
         *
         * @param message the message
         */
        public void warn(String message) {
            log(IStatus.WARNING, message, null);
        }

        /**
         * Log a error message.
         *
         * @param message the message
         */
        public void error(String message) {
            log(IStatus.ERROR, message, null);
        }

        /**
         * Log a error message with exception.
         *
         * @param message the message
         * @param e the exception
         */
        public void error(String message, Exception e) {
            log(IStatus.ERROR, message, e);
        }
    }

    /**
     * Returns the shared instance.
     *
     * @return the shared instance
     */
    public static AutoRefactorPlugin getDefault() {
        // Deprecated. Replaced by IEclipsePreferences.
        // Preferences are now stored according to scopes in the IPreferencesService.
        // The return value of this method corresponds to a combination of the InstanceScope and the DefaultScope.
        // To set preferences for your plug-in, use new InstanceScope().getNode(<&yourPluginId>).
        // To set default preferences for your plug-in, use new DefaultScope().getNode(<yourPluginId>).
        // To lookup an integer preference value for your plug-in, use
        // Platform.getPreferencesService().getInt(<yourPluginId>, <preferenceKey>, <defaultValue>, null).
        // Similar methods exist on IPreferencesService for obtaining other kinds of preference values
        // (strings, booleans, etc).
        return plugin;
    }

    /**
     * Returns the environment.
     *
     * @return the environment
     */
    public static Environment getEnvironment() {
        return environment;
    }

    /**
     * Turns on the debug mode.
     * <p>
     * This method is only for internal use.
     */
    public static void turnDebugModeOn() {
        getDefault().getPreferenceStore().setValue(PreferenceConstants.DEBUG_MODE_ON.getName(), true);
    }

    /**
     * Returns an image descriptor for the image file at the given plug-in relative path.
     *
     * @param path
     *            the path
     * @return the image descriptor
     */
    public static ImageDescriptor getImageDescriptor(final String path) {
        return imageDescriptorFromPlugin(PLUGIN_ID, path);
    }

    private static class JobManagerImpl implements JobManager {
        private final Vector<Job> jobs = new Vector<Job>();

        /**
         * Register a job.
         *
         * @param job the job
         */
        public void register(Job job) {
            jobs.add(job);
        }

        /**
         * Unregister a job.
         *
         * @param job the job
         */
        public void unregister(Job job) {
            jobs.remove(job);
        }

        private void cancelJobs() {
            for (Job job : jobs) {
                job.cancel();
            }
            jobs.clear();
        }
    }
}

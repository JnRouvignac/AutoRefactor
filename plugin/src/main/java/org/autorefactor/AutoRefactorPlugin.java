/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2014 Jean-Noël Rouvignac - initial API and implementation
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

import org.autorefactor.preferences.PreferenceConstants;
import org.autorefactor.preferences.Preferences;
import org.autorefactor.ui.preferences.EclipsePreferences;
import org.autorefactor.util.UnhandledException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle.
 */
public class AutoRefactorPlugin extends AbstractUIPlugin {

    /** The plug-in ID. */
    public static final String PLUGIN_ID = "org.autorefactor.plugin";

    /** The shared instance. */
    private static AutoRefactorPlugin plugin;

    private static Preferences preferenceHelper;
    private static Vector<Job> jobs = new Vector<Job>();

    @Override
    public void start(final BundleContext context) throws Exception {
        super.start(context);
        plugin = this;
    }

    @Override
    public void stop(final BundleContext context) throws Exception {
        plugin = null;
        for (Job job : jobs) {
            job.cancel();
        }
        jobs.clear();
        super.stop(context);
    }

    private static void log(int severity, String message, Exception e) {
        if (getPreferenceHelper().debugModeOn()) {
            if (e instanceof RuntimeException) {
                throw (RuntimeException) e;
            }
            throw new UnhandledException(null, message, e);
        }

        final ILog log = getDefault().getLog();
        log.log(new Status(severity, PLUGIN_ID, message, e));
    }

    /**
     * Logs a warning into Eclipse workspace logs.
     *
     * @param message the message to log
     */
    public static void logWarning(String message) {
        log(IStatus.WARNING, message, null);
    }

    /**
     * Logs an error into Eclipse workspace logs.
     *
     * @param message the message to log
     */
    public static void logError(String message) {
        log(IStatus.ERROR, message, null);
    }

    /**
     * Logs an error with an exception into Eclipse workspace logs.
     *
     * @param message the message to log
     * @param e the exception to log
     */
    public static void logError(String message, Exception e) {
        log(IStatus.ERROR, message, e);
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
     * Returns a helper object for the preferences.
     *
     * @return a helper object for the preferences
     */
    public static Preferences getPreferenceHelper() {
        if (preferenceHelper == null) {
            preferenceHelper = new EclipsePreferences(getDefault().getPreferenceStore());
        }
        return preferenceHelper;
    }

    /**
     * Turns on the debug mode.
     *
     * This method is only for internal use.
     */
    public static void turnDebugModeOn() {
        getDefault().getPreferenceStore().setValue(PreferenceConstants.DEBUG_MODE_ON.name(), Boolean.TRUE.toString());
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

    /**
     * Registers the provided job against this plugin.
     *
     * @param job the job to register
     */
    public static void register(Job job) {
        jobs.add(job);
    }

    /**
     * Unregisters the provided job from this plugin.
     *
     * @param job the job to unregister
     */
    public static void unregister(Job job) {
        jobs.remove(job);
    }
}

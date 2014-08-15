/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013 Jean-Noël Rouvignac - initial API and implementation
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

import org.autorefactor.ui.preferences.PreferenceHelper;
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

    private static PreferenceHelper preferenceHelper;

    @Override
    public void start(final BundleContext context) throws Exception {
        super.start(context);
        plugin = this;
    }

    @Override
    public void stop(final BundleContext context) throws Exception {
        plugin = null;
        super.stop(context);
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
    public static PreferenceHelper getPreferenceHelper() {
        if (preferenceHelper == null) {
            preferenceHelper = new PreferenceHelper(getDefault().getPreferenceStore());
        }
        return preferenceHelper;
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
}

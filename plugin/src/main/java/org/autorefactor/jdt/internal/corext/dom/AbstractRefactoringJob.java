/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2019 Michael Keppler - initial API and implementation
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
package org.autorefactor.jdt.internal.corext.dom;

import org.eclipse.core.runtime.jobs.Job;

/**
 * Abstract super class of all refactoring jobs, marking them as belonging to a certain family, thereby simplifying the cancellation of multiple jobs.
 */
public abstract class AbstractRefactoringJob extends Job {
    /**
     * Abstract super class of all refactoring jobs.
     *
     * @param name The job name
     */
    public AbstractRefactoringJob(final String name) {
        super(name);
    }

    /**
     * All instances of this job belong to this family.
     */
    public static final Object FAMILY = new Object();

    @Override
    public final boolean belongsTo(final Object family) {
        return FAMILY == family || super.belongsTo(family);
    }
}

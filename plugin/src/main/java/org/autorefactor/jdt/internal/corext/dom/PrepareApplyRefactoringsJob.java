/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015-2018 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ConcurrentSkipListSet;

import org.autorefactor.environment.Environment;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.UnhandledException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobGroup;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaModelException;

/**
 * Eclipse job that prepares and partitions work for
 * {@link ApplyRefactoringsJob}.
 */
public class PrepareApplyRefactoringsJob extends Job {
    private final List<IJavaElement> javaElements;
    private final List<RefactoringRule> refactoringRulesToApply;
    private final Map<IJavaElement, JavaProjectOptions> javaProjects= new HashMap<>();
    private final Environment environment;

    /**
     * Builds an instance of this class.
     *
     * @param javaElements            the java elements selected for automatic
     *                                cleanup
     * @param refactoringRulesToApply the cleanups to apply
     * @param environment             the environment
     */
    public PrepareApplyRefactoringsJob(final List<IJavaElement> javaElements, final List<RefactoringRule> refactoringRulesToApply,
            final Environment environment) {
        super("Prepare AutoRefactor"); //$NON-NLS-1$
        setPriority(Job.SHORT);
        this.javaElements= javaElements;
        this.refactoringRulesToApply= refactoringRulesToApply;
        this.environment= environment;
    }

    @Override
    protected IStatus run(final IProgressMonitor monitor) {
        try {
            return run0(monitor);
        } catch (OperationCanceledException e) {
            throw e;
        } catch (Exception e) {
            final String msg= "Error while preparing automatic refactorings.\n\n" //$NON-NLS-1$
                    + "Please look at the Eclipse workspace logs and " //$NON-NLS-1$
                    + "report the stacktrace to the AutoRefactor project.\n" //$NON-NLS-1$
                    + "Please provide sample java code that triggers the error.\n\n"; //$NON-NLS-1$
            return new Status(IStatus.ERROR, PluginConstant.PLUGIN_ID, msg, e);
        }
    }

    private IStatus run0(final IProgressMonitor monitor) throws Exception {
        if (!javaElements.isEmpty()) {
            final Queue<RefactoringUnit> toRefactor= collectRefactoringUnits(javaElements, monitor);
            final int nbCores= Runtime.getRuntime().availableProcessors();
            final int nbWorkers= computeNbWorkers(toRefactor.size(), nbCores);
            final JobGroup jobGroup= new JobGroup("Job name", nbWorkers, nbWorkers); //$NON-NLS-1$
            for (int i= 0; i < nbWorkers; i++) {
                final Job job= new ApplyRefactoringsJob(toRefactor, clone(refactoringRulesToApply), environment);
                job.setJobGroup(jobGroup);
                job.setUser(true);
                job.schedule();
            }
        }

        return Status.OK_STATUS;
    }

    /**
     * Clones all the cleanups to apply. In fairness, this method is only useful
     * for stateful cleanups.
     */
    private List<RefactoringRule> clone(final List<RefactoringRule> refactorings) throws Exception {
        final List<RefactoringRule> res= new ArrayList<>(refactorings.size());
        for (RefactoringRule refactoring : refactorings) {
            res.add(refactoring.getClass().newInstance());
        }

        return res;
    }

    private int computeNbWorkers(final int nbWorkItems, final int nbCores) {
        final int nbPartitions= nbWorkItems / 10;
        if (nbPartitions >= nbCores) {
            return nbCores;
        }
        if (nbPartitions > 0) {
            return nbPartitions;
        }

        return 1;
    }

    private Queue<RefactoringUnit> collectRefactoringUnits(final List<IJavaElement> javaElements, final IProgressMonitor monitor) {
        try {
            final Set<RefactoringUnit> results= new ConcurrentSkipListSet<>();
            addAll(results, javaElements, monitor);
            return new ConcurrentLinkedQueue<>(results);
        } catch (Exception e) {
            throw new UnhandledException(null, e);
        }
    }

    private void addAll(final Set<RefactoringUnit> results, final List<IJavaElement> javaElements, final IProgressMonitor monitor)
            throws JavaModelException {
        final SubMonitor subMonitor= SubMonitor.convert(monitor, javaElements.size());
        for (IJavaElement javaElement : javaElements) {
            final SubMonitor child= subMonitor.newChild(1);
            final JavaProjectOptions options= getJavaProjectOptions(javaElement);
            if (javaElement instanceof ICompilationUnit) {
                add(results, (ICompilationUnit) javaElement, options);
            } else if (javaElement instanceof IPackageFragment) {
                final IPackageFragment pf= (IPackageFragment) javaElement;
                addAll(results, getSubPackages(pf), child);
                addAll(results, pf.getCompilationUnits(), options);
            } else if (javaElement instanceof IPackageFragmentRoot) {
                final IPackageFragmentRoot pfr= (IPackageFragmentRoot) javaElement;
                addAll(results, Arrays.asList(pfr.getChildren()), child);
            } else if (javaElement instanceof IJavaProject) {
                IJavaProject javaProject= (IJavaProject) javaElement;
                for (IPackageFragment pf : javaProject.getPackageFragments()) {
                    addAll(results, pf.getCompilationUnits(), options);
                }
            }
        }
    }

    private void addAll(final Set<RefactoringUnit> results, final ICompilationUnit[] cus, final JavaProjectOptions options)
            throws JavaModelException {
        for (ICompilationUnit cu : cus) {
            add(results, cu, options);
        }
    }

    private void add(final Set<RefactoringUnit> results, final ICompilationUnit cu, final JavaProjectOptions options)
            throws JavaModelException {
        if (!cu.isConsistent()) {
            cu.makeConsistent(null);
        }
        if (!cu.isReadOnly()) {
            results.add(new RefactoringUnit(cu, options));
        }
    }

    private JavaProjectOptions getJavaProjectOptions(final IJavaElement javaElement) {
        final IJavaProject javaProject= getIJavaProject(javaElement);
        JavaProjectOptions options= javaProjects.get(javaProject);
        if (options == null) {
            options= new JavaProjectOptionsImpl(javaProject.getOptions(true));
            javaProjects.put(javaProject, options);
        }

        return options;
    }

    /**
     * getIJavaProject.
     *
     * @param javaElement javaElement
     * @return IJavaProject
     */
    public static IJavaProject getIJavaProject(final IJavaElement javaElement) {
        if (javaElement instanceof ICompilationUnit || javaElement instanceof IPackageFragment
                || javaElement instanceof IPackageFragmentRoot) {
            return getIJavaProject(javaElement.getParent());
        }
        if (javaElement instanceof IJavaProject) {
            return (IJavaProject) javaElement;
        }
        throw new NotImplementedException(null, javaElement);
    }

    private List<IJavaElement> getSubPackages(final IPackageFragment motherPackage) throws JavaModelException {
        List<IJavaElement> subPackages= new ArrayList<>();
        String packageName= motherPackage.getElementName();
        IJavaElement[] packages= ((IPackageFragmentRoot) motherPackage.getParent()).getChildren();

        for (IJavaElement onePackage : packages) {
            if (onePackage instanceof IPackageFragment && onePackage.getElementName().startsWith(packageName + ".")) { //$NON-NLS-1$
                subPackages.add(onePackage);
            }
        }

        return subPackages;
    }
}

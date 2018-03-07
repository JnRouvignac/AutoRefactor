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
package org.autorefactor.refactoring;

import static org.autorefactor.refactoring.PluginConstant.PLUGIN_ID;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;

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

/** Eclipse job that prepares and partitions work for {@link ApplyRefactoringsJob}. */
public class PrepareApplyRefactoringsJob extends Job {
    private final List<IJavaElement> javaElements;
    private final List<RefactoringRule> refactoringRulesToApply;
    private final Map<IJavaElement, JavaProjectOptions> javaProjects = new HashMap<IJavaElement, JavaProjectOptions>();
    private final Environment environment;

    /**
     * Builds an instance of this class.
     *
     * @param javaElements the java elements selected for automatic refactoring
     * @param refactoringRulesToApply the refactorings to apply
     * @param environment the environment
     */
    public PrepareApplyRefactoringsJob(List<IJavaElement> javaElements,
                                       List<RefactoringRule> refactoringRulesToApply,
                                       Environment environment) {
        super("Prepare AutoRefactor");
        setPriority(Job.SHORT);
        this.javaElements = javaElements;
        this.refactoringRulesToApply = refactoringRulesToApply;
        this.environment = environment;
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        environment.getJobManager().register(this);
        try {
            return run0(monitor);
        } catch (OperationCanceledException e) {
            throw e;
        } catch (Exception e) {
            final String msg = "Error while preparing automatic refactorings.\n\n"
                    + "Please look at the Eclipse workspace logs and "
                    + "report the stacktrace to the AutoRefactor project.\n"
                    + "Please provide sample java code that triggers the error.\n\n";
            return new Status(IStatus.ERROR, PLUGIN_ID, msg, e);
        } finally {
            environment.getJobManager().unregister(this);
        }
    }

    private IStatus run0(IProgressMonitor monitor) throws Exception {
        if (!javaElements.isEmpty()) {
            final Queue<RefactoringUnit> toRefactor = collectRefactoringUnits(javaElements, monitor);
            final int nbCores = Runtime.getRuntime().availableProcessors();
            final int nbWorkers = computeNbWorkers(toRefactor.size(), nbCores);
            final JobGroup jobGroup = new JobGroup("Job name", nbWorkers, nbWorkers);
            for (int i = 0; i < nbWorkers; i++) {
                final Job job = new ApplyRefactoringsJob(toRefactor, clone(refactoringRulesToApply), environment);
                job.setJobGroup(jobGroup);
                job.setUser(true);
                job.schedule();
            }
        }
        return Status.OK_STATUS;
    }

    /**
     * Clones all the refactorings to apply.
     * In fairness, this method is only useful for stateful refactorings.
     */
    private List<RefactoringRule> clone(List<RefactoringRule> refactorings) throws Exception {
        final List<RefactoringRule> res = new ArrayList<RefactoringRule>(refactorings.size());
        for (RefactoringRule refactoring : refactorings) {
            res.add(refactoring.getClass().newInstance());
        }
        return res;
    }

    private int computeNbWorkers(int nbWorkItems, int nbCores) {
        final int nbPartitions = nbWorkItems / 10;
        if (nbPartitions >= nbCores) {
            return nbCores;
        } else if (nbPartitions > 0) {
            return nbPartitions;
        } else {
            return 1;
        }
    }

    private Queue<RefactoringUnit> collectRefactoringUnits(List<IJavaElement> javaElements, IProgressMonitor monitor) {
        try {
            final Queue<RefactoringUnit> results = new ConcurrentLinkedQueue<RefactoringUnit>();
            addAll(results, javaElements, monitor);
            return results;
        } catch (Exception e) {
            throw new UnhandledException(null, e);
        }
    }

    private void addAll(Queue<RefactoringUnit> results, List<IJavaElement> javaElements, IProgressMonitor monitor)
            throws JavaModelException {
        final SubMonitor subMonitor = SubMonitor.convert(monitor, javaElements.size());
        for (IJavaElement javaElement : javaElements) {
            final SubMonitor child = subMonitor.newChild(1);
            final JavaProjectOptions options = getJavaProjectOptions(javaElement);
            if (javaElement instanceof ICompilationUnit) {
                add(results, (ICompilationUnit) javaElement, options);
            } else if (javaElement instanceof IPackageFragment) {
                final IPackageFragment pf = (IPackageFragment) javaElement;
                addAll(results, getSubPackages(pf), child);
                addAll(results, pf.getCompilationUnits(), options);
            } else if (javaElement instanceof IPackageFragmentRoot) {
                final IPackageFragmentRoot pfr = (IPackageFragmentRoot) javaElement;
                addAll(results, Arrays.asList(pfr.getChildren()), child);
            } else if (javaElement instanceof IJavaProject) {
                IJavaProject javaProject = (IJavaProject) javaElement;
                for (IPackageFragment pf : javaProject.getPackageFragments()) {
                    addAll(results, pf.getCompilationUnits(), options);
                }
            }
        }
    }

    private void addAll(final Queue<RefactoringUnit> results, ICompilationUnit[] cus, JavaProjectOptions options)
            throws JavaModelException {
        for (ICompilationUnit cu : cus) {
            add(results, cu, options);
        }
    }

    private void add(final Queue<RefactoringUnit> results, ICompilationUnit cu, JavaProjectOptions options)
            throws JavaModelException {
        if (!cu.isConsistent()) {
            cu.makeConsistent(null);
        }
        if (!cu.isReadOnly()) {
            results.add(new RefactoringUnit(cu, options));
        }
    }

    private JavaProjectOptions getJavaProjectOptions(IJavaElement javaElement) {
        final IJavaProject javaProject = getIJavaProject(javaElement);
        JavaProjectOptions options = javaProjects.get(javaProject);
        if (options == null) {
            options = new JavaProjectOptionsImpl(javaProject.getOptions(true));
            javaProjects.put(javaProject, options);
        }
        return options;
    }

    private IJavaProject getIJavaProject(IJavaElement javaElement) {
        if (javaElement instanceof ICompilationUnit
                || javaElement instanceof IPackageFragment
                || javaElement instanceof IPackageFragmentRoot) {
            return getIJavaProject(javaElement.getParent());
        } else if (javaElement instanceof IJavaProject) {
            return (IJavaProject) javaElement;
        }
        throw new NotImplementedException(null, javaElement);
    }

    private List<IJavaElement> getSubPackages(IPackageFragment motherPackage) throws JavaModelException {
        List<IJavaElement> subPackages = new ArrayList<IJavaElement>();
        String packageName = motherPackage.getElementName();
        IJavaElement[] packages = ((IPackageFragmentRoot) motherPackage.getParent()).getChildren();

        for (IJavaElement onePackage : packages) {
            if (onePackage instanceof IPackageFragment && onePackage.getElementName().startsWith(packageName + ".")) {
                subPackages.add(onePackage);
            }
        }

        return subPackages;
    }
}

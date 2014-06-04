package org.autorefactor.refactoring.rules;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaCore;

public class JavaCoreHelper {

	private static final Path[] EMPTY_PATHS = new Path[0];

	public static IPackageFragment getPackageFragment() throws Exception {
		final IJavaProject javaProject = createJavaProject("projectName", "bin");
		final IPackageFragmentRoot root = addSourceContainer(javaProject, "/testRoot");
		final IClasspathEntry srcEntry = JavaCore.newSourceEntry(
				root.getPath(), EMPTY_PATHS, EMPTY_PATHS, null);
		final IClasspathEntry rtJarEntry = JavaCore.newLibraryEntry(
				getPathToRtJar(), null, null);
		addToClasspath(javaProject, srcEntry, rtJarEntry);
		return root.createPackageFragment("org.autorefactor", true, null);
	}

	public static IJavaProject createJavaProject(String projectName,
			String binFolderName) throws Exception {
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		IProject project = root.getProject(projectName);
		if (!project.exists()) {
			project.create(null);
		} else {
			project.refreshLocal(IResource.DEPTH_INFINITE, null);
		}

		if (!project.isOpen()) {
			project.open(null);
		}

		final IFolder binFolder = project.getFolder(binFolderName);
		createFolder(binFolder);

		addNatureToProject(project, JavaCore.NATURE_ID);

		final IJavaProject javaProject = JavaCore.create(project);
		javaProject.setOutputLocation(binFolder.getFullPath(), null);
		javaProject.setRawClasspath(new IClasspathEntry[0], null);
		return javaProject;
	}

	private static void addNatureToProject(IProject project, String natureId) throws Exception {
		if (project.hasNature(JavaCore.NATURE_ID)) {
			return;
		}

		IProjectDescription description = project.getDescription();
		String[] prevNatures = description.getNatureIds();
		String[] newNatures = Arrays.copyOf(prevNatures, prevNatures.length + 1);
		newNatures[prevNatures.length] = natureId;
		description.setNatureIds(newNatures);
		project.setDescription(description, null);
	}

	private static IPackageFragmentRoot addSourceContainer(
			IJavaProject javaProject, String containerName) throws Exception {
		final IProject project = javaProject.getProject();
		final IFolder folder = project.getFolder(containerName);
		createFolder(folder);

		IPackageFragmentRoot root = javaProject.getPackageFragmentRoot(folder);
		IClasspathEntry cpe = JavaCore.newSourceEntry(
				root.getPath(), EMPTY_PATHS, EMPTY_PATHS, null);
		addToClasspath(javaProject, cpe);
		return root;
	}

	private static void createFolder(IFolder folder) throws Exception {
		if (!folder.exists()) {
			final IContainer parent = folder.getParent();
			if (parent instanceof IFolder) {
				createFolder((IFolder) parent);
			}
			folder.create(false, true, null);
		}
	}

	private static IPath getPathToRtJar() {
		final String classPath = System.getProperty("sun.boot.class.path");
		final int idx = classPath.indexOf("rt.jar");
		if (idx == -1) {
			throw new RuntimeException("Could not find Java runtime library rt.jar");
		}
		final int end = idx + "rt.jar".length();
		final int lastIdx = classPath.lastIndexOf(":", idx);
		final int start = lastIdx != -1 ? lastIdx + 1 : 0;
		return new Path(classPath.substring(start, end));
	}

	private static void addToClasspath(IJavaProject javaProject,
			IClasspathEntry... entries) throws Exception {
		if (entries.length != 0) {
			IClasspathEntry[] oldEntries = javaProject.getRawClasspath();
			IClasspathEntry[] newEntries;
			if (oldEntries.length != 0)
			{
				// remove duplicate entries
				Set<IClasspathEntry> set = new HashSet<IClasspathEntry>(Arrays.asList(oldEntries));
				set.addAll(Arrays.asList(entries));
				newEntries = set.toArray(new IClasspathEntry[set.size()]);
			}
			else
			{
				newEntries = entries;
			}
			javaProject.setRawClasspath(newEntries, null);
		}
	}

}

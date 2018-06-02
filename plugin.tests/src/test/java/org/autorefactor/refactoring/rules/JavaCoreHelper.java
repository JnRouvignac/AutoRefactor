/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2016 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.refactoring.rules;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

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
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public final class JavaCoreHelper {

    private static final Path[] EMPTY_PATHS = new Path[0];

    public static IPackageFragment getPackageFragment(String packageName) throws Exception {
        final IJavaProject javaProject = createJavaProject("projectName", "bin");
        final IPackageFragmentRoot root = addSourceContainer(javaProject, "/testRoot");
        addToClasspath(javaProject, getClasspathEntries(root));
        return root.createPackageFragment(packageName, true, null);
    }

    private static List<IClasspathEntry> getClasspathEntries(final IPackageFragmentRoot root) throws Exception {
        final List<IClasspathEntry> entries = new ArrayList<IClasspathEntry>();
        final IClasspathEntry srcEntry = JavaCore.newSourceEntry(root.getPath(), EMPTY_PATHS, EMPTY_PATHS, null);
        entries.add(srcEntry);

        // Should not execute this code for Java 9
        final IClasspathEntry rtJarEntry = JavaCore.newLibraryEntry(getPathToRtJar(), null, null);
        entries.add(rtJarEntry);

        extractClasspathEntries(entries, "../samples/pom.xml");
        return entries;
    }

    private static void extractClasspathEntries(List<IClasspathEntry> entries, String classpathFile) throws Exception {
        final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        final DocumentBuilder builder = factory.newDocumentBuilder();
        final Document document = builder.parse(new File(classpathFile));

        final Node projectNode = getNodeByNodeName(document.getChildNodes(), "project");
        final List<Node> dependencies =
                asList(getNodeByNodeName(projectNode.getChildNodes(), "dependencies").getChildNodes());
        final String m2Repo = getM2Repository();
        for (Node dependency : dependencies) {
            if (dependency.getNodeType() == Node.COMMENT_NODE) {
                continue;
            }
            final NodeList children = dependency.getChildNodes();
            String groupId = getNodeByNodeName(children, "groupId").getTextContent();
            String artifactId = getNodeByNodeName(children, "artifactId").getTextContent();
            String version = getNodeByNodeName(children, "version").getTextContent();
            String sep = File.separator;
            final String jarPath = m2Repo + sep + toPath(groupId) + sep + artifactId + sep + version + sep
                    + artifactId + "-" + version + ".jar";
            entries.add(JavaCore.newLibraryEntry(new Path(jarPath), null, null));
        }
    }

    private static String getM2Repository() throws Exception {
        final String userHome = System.getProperty("user.home");
        final File m2Settings = new File(userHome + "/.m2/settings.xml");
        if (m2Settings.exists() && m2Settings.isFile()) {
            final Document document = DocumentBuilderFactory.newInstance()
                                                            .newDocumentBuilder()
                                                            .parse(m2Settings);

            final Node settingsNode = getNodeByNodeName(document.getChildNodes(), "settings");
            if (settingsNode != null) {
                final Node localRepoNode = getNodeByNodeName(settingsNode.getChildNodes(), "localRepository");
                if (localRepoNode != null) {
                    return localRepoNode.getTextContent();
                }
            }
        }
        final File m2Repo = new File(userHome + "/.m2/repository");
        if (m2Repo.exists() && m2Repo.isDirectory()) {
            return m2Repo.getPath();
        }
        throw new RuntimeException("Cannot determine maven repository."
                        + " Tried \"" + m2Settings + "\" file"
                        + " and \"" + m2Repo + "\" directory.");
    }

    private static Node getNodeByNodeName(NodeList nodes, String nodeName) {
        for (Node node : asList(nodes)) {
            if (nodeName.equals(node.getNodeName())) {
                return node;
            }
        }
        return null;
    }

    private static String toPath(String groupId) {
        final StringBuilder sb = new StringBuilder(groupId);
        int length = sb.length();
        for (int i = 0; i < length; i++) {
            if (sb.charAt(i) == '.') {
                sb.setCharAt(i, '/');
            }
        }
        return sb.toString();
    }

    private static List<Node> asList(NodeList nodeList) {
        final List<Node> results = new ArrayList<Node>();
        int length = nodeList.getLength();
        for (int i = 0; i < length; i++) {
            final Node item = nodeList.item(i);
            if (item.getNodeType() != Node.TEXT_NODE) {
                results.add(item);
            }
        }
        return results;
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
        String[] newNatures = new String[prevNatures.length + 1];
        System.arraycopy(prevNatures, 0, newNatures, 0, prevNatures.length);
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
        IClasspathEntry cpe = JavaCore.newSourceEntry(root.getPath(), EMPTY_PATHS, EMPTY_PATHS, null);
        addToClasspath(javaProject, Arrays.asList(cpe));
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

    private static void addToClasspath(IJavaProject javaProject, List<IClasspathEntry> classpathEntries)
            throws Exception {
        if (!classpathEntries.isEmpty()) {
            IClasspathEntry[] oldEntries = javaProject.getRawClasspath();
            IClasspathEntry[] newEntries;
            if (oldEntries.length != 0) {
                // remove duplicate entries
                Set<IClasspathEntry> set = new HashSet<IClasspathEntry>(Arrays.asList(oldEntries));
                set.addAll(classpathEntries);
                newEntries = set.toArray(new IClasspathEntry[set.size()]);
            } else {
                newEntries = classpathEntries.toArray(new IClasspathEntry[classpathEntries.size()]);
            }
            javaProject.setRawClasspath(newEntries, null);
        }
    }

    private JavaCoreHelper() {
        // private for utility classes
    }
}

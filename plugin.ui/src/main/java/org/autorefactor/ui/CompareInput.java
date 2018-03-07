package org.autorefactor.ui;

import java.lang.reflect.InvocationTargetException;

import org.autorefactor.refactoring.ApplyRefactoringsJob;
import org.eclipse.compare.CompareConfiguration;
import org.eclipse.compare.CompareEditorInput;
import org.eclipse.compare.structuremergeviewer.DiffNode;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jdt.core.ICompilationUnit;

/**
 * Compare input.
 */
public class CompareInput extends CompareEditorInput {
    /**
     * Ignore whitespace.
     */
    public static final String IGNORE_WHITESPACE = "IGNORE_WHITESPACE";

    static CompareConfiguration compareConfiguration = new CompareConfiguration();
    static DiffNode node;
    static IProgressMonitor monitor;

    CompareItem left;
    CompareItem right;

    /**
     * Constructor.
     */
    public CompareInput() {
        super(compareConfiguration);
        compareConfiguration.setLeftLabel("Original Java File");
        compareConfiguration.setRightLabel("Refactored Java File");
        compareConfiguration.setRightEditable(true);
        compareConfiguration.setLeftEditable(true);
        compareConfiguration.setProperty(IGNORE_WHITESPACE, IGNORE_WHITESPACE);

        setDirty(true);
        setTitle("Compare the two java File");
    }

    @SuppressWarnings("restriction")
    @Override
    protected Object prepareInput(IProgressMonitor monitor) throws InvocationTargetException, InterruptedException {
        CompareItem ancestor = new CompareItem(ApplyRefactoringsJob.getCodeToRefactor());
        left = new CompareItem(ApplyRefactoringsJob.getCodeToRefactor());
        right = new CompareItem(ApplyRefactoringsJob.getRefactoredContent());
        node = new DiffNode(1, ancestor, left, right);

        return node;
    }

    @Override
    public boolean okPressed() {
        try {
            CompareItem.setNewContents(ApplyRefactoringsJob.getRefactoredContent());
            saveChanges(monitor);
        } catch (Exception e) {
            e.printStackTrace();
        }
        createAndCommit();
        return true;
    }

    @Override
    public void cancelPressed() {
        try {
            ApplyRefactoringsJob.getNewFile().delete(true, monitor);
        } catch (CoreException e) {
            e.printStackTrace();
        }
    }

    /**
     * Create and commit.
     */
    public void createAndCommit() {
        ICompilationUnit iCompilation = ApplyRefactoringsJob.getiCompile();
        try {
            iCompilation.getBuffer().setContents(CompareItem.getNewContents());
            ApplyRefactoringsJob.getNewFile().delete(true, monitor);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

package org.autorefactor.ui;

import org.eclipse.compare.CompareUI;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.swt.widgets.Display;

/**
 * Apply refactoring listener.
 */
public class ApplyRefactoringListener implements IJobChangeListener {
    int numberOfJobs;

    @Override
    public void aboutToRun(IJobChangeEvent event) {
    }

    @Override
    public void awake(IJobChangeEvent event) {
    }

    @Override
    public void done(IJobChangeEvent event) {
        Display.getDefault().syncExec(new Runnable() {

            @Override
            public void run() {
                CompareInput compareInput = new CompareInput();
                compareInput.setDirty(true);

                CompareUI.openCompareDialog(new CompareInput());
            }
        });
    }

    @Override
    public void running(IJobChangeEvent event) {
    }

    @Override
    public void scheduled(IJobChangeEvent event) {
        numberOfJobs++;
    }

    @Override
    public void sleeping(IJobChangeEvent event) {
    }
}

package org.autorefactor.ui;


import org.eclipse.compare.CompareUI;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.swt.widgets.Display;

public class ApplyRefactoringListener implements IJobChangeListener {


	int numberOfJobs = 0;
	@Override
	public void aboutToRun(IJobChangeEvent event) {
		// TODO Auto-generated method stub

	}

	@Override
	public void awake(IJobChangeEvent event) {
		// TODO Auto-generated method stub

	}

	@Override
	public void done(IJobChangeEvent event) {
		// TODO Auto-generated method stub

		Display.getDefault().syncExec(new Runnable() {

			public void run() {

				CompareInput compareInput = new CompareInput();
				compareInput.setDirty(true);

				CompareUI.openCompareDialog(new CompareInput());

			}
		});


	}

	@Override
	public void running(IJobChangeEvent event) {
		// TODO Auto-generated method stub

	}

	@Override
	public void scheduled(IJobChangeEvent event) {
		// TODO Auto-generated method stub

		numberOfJobs++;
	}

	@Override
	public void sleeping(IJobChangeEvent event) {
		// TODO Auto-generated method stub

	}

}

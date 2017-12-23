package org.autorefactor.ui;


import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;

import org.eclipse.swt.widgets.Display;
import org.eclipse.compare.CompareUI;

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
		
		System.out.println("Number Of Jobs"+ numberOfJobs);
		System.out.println("Job is done");
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

package org.autorefactor.ui;

import java.lang.reflect.InvocationTargetException;

import org.autorefactor.refactoring.ApplyRefactoringsJob;
import org.eclipse.compare.*;
import org.eclipse.compare.structuremergeviewer.DiffNode;
import org.eclipse.compare.structuremergeviewer.Differencer;
import org.eclipse.core.runtime.IProgressMonitor;

public class CompareInput extends CompareEditorInput{
	
	private String original= "Original";
	private String refactored = "Refactored";
	static CompareConfiguration compareConfiguration = new CompareConfiguration();
	

	public CompareInput() {
		super(compareConfiguration);
		compareConfiguration.setLeftLabel("Original Java File");
		
		compareConfiguration.setRightLabel("Refactored Java File");
		compareConfiguration.setRightEditable(true);
		compareConfiguration.setLeftEditable(true);
		System.out.println("Check Editable"+compareConfiguration.isRightEditable());
		
		
		// TODO Auto-generated constructor stub
	}

	@Override
	protected Object prepareInput(IProgressMonitor monitor) throws InvocationTargetException, InterruptedException {
		// TODO Auto-generated method stub
	//	original = ApplyRefactoringsJob.codeToRefactor;
		CompareItem ancestor = new CompareItem(original);
		CompareItem left = new CompareItem(ApplyRefactoringsJob.codeToRefactor);
		CompareItem right = new CompareItem(ApplyRefactoringsJob.refactoredContent);
		
		return new DiffNode(null, Differencer.CONFLICTING, ancestor, left, right);
	}

}

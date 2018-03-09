package org.autorefactor.ui;

import org.autorefactor.refactoring.ApplyRefactoringsJob;
import org.eclipse.compare.CompareUI;
import org.eclipse.jface.wizard.Wizard;

public class PreviewWizard extends Wizard {
	
	private PreviewWizardPage previewWizardPage = new PreviewWizardPage();

    @Override
    public String getWindowTitle() {
        return "View Refactorings Applied...";
    }

    @Override
    public void addPages() {
        addPage(previewWizardPage);
    }
    
	@Override
	public boolean performFinish() {
		// TODO Auto-generated method stub
		
		/*	Check if the selected element was previously refactored		
		 */

		if(( ApplyRefactoringsJob.iCompile != null) && 
				(PreviewWizardHandler.getSelectedJavaElement+" ").contains(ApplyRefactoringsJob.iCompile.getElementName())) {
			
			CompareUI.openCompareDialog(new CompareInput());
			
		}
			


		return true;
	}

}

package org.autorefactor.ui;

import org.eclipse.jface.wizard.Wizard;

public class PreviewWizard extends Wizard {
	
	private PreviewWizardPage previewWizardPage = new PreviewWizardPage();

    @Override
    public String getWindowTitle() {
        return "Preview refactorings...";
    }

    @Override
    public void addPages() {
        addPage(previewWizardPage);
    }
    
	@Override
	public boolean performFinish() {
		// TODO Auto-generated method stub
		return false;
	}

}

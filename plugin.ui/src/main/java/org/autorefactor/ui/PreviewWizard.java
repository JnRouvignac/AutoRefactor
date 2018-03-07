package org.autorefactor.ui;

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
        // final Wizard wizard = new PreviewWizard();
        // final WizardDialog wizardDialog = new WizardDialog(shell, wizard);
        // wizardDialog.open();

        CompareUI.openCompareDialog(new CompareInput());
        return true;
    }
}

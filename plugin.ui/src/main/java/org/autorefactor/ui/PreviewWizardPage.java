package org.autorefactor.ui;

import java.util.Iterator;

import org.autorefactor.refactoring.ApplyRefactoringsJob;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;

/**
 * Preview wizard page.
 */
public class PreviewWizardPage extends WizardPage {
    /**
     * Constructor.
     */
    protected PreviewWizardPage() {
        super("Preview");

        setTitle("View Refactorings Applied...");
        setDescription(
                "This wizard is to allow user to preview the refactoring that are to be applied"
                        + " or that have been applied");
    }

    @Override
    public void createControl(Composite parent) {
        // Change here
        parent.setLayout(new GridLayout());

        createRefactoringsTextArea(parent);

        setControl(parent);
        // Allows to click the "Finish" button
        setPageComplete(true);
    }

    private void createRefactoringsTextArea(Composite parent) {
        Text refactoredText = new Text(parent, SWT.MULTI | SWT.BORDER);
        refactoredText.setEnabled(false);
        Iterator<String> iterate = ApplyRefactoringsJob.getRefactoringsApplied().iterator();
        String data = "";
        while (iterate.hasNext()) {
            data = data + iterate.next() + " ";
        }

        if ("".equals(data)) {
            refactoredText.setText("No refactoring applied");
        } else {
            refactoredText.setText(data.replaceAll(" ", System.getProperty("line.separator")));
        }
        refactoredText.setLayoutData(new GridData(GridData.FILL_HORIZONTAL | GridData.FILL_VERTICAL));
    }
}

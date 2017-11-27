package org.autorefactor.ui;



import org.autorefactor.refactoring.ApplyRefactoringsJob;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;

public class PreviewWizardPage extends WizardPage{

	public Text refactoredText;
	protected PreviewWizardPage() {
		super("Preview Here");
		
		setTitle("Preview WIzard");
		setDescription("This wizard is to allow user to preview the refactoring that are to be applied or that have been applied");
		// TODO Auto-generated constructor stub
	}

	   @Override
	    public void createControl(Composite parent) {
		   //change here
	        parent.setLayout(new GridLayout());
		 
		 
		createRefactoringsTextArea(parent);
		
		setControl(parent);
        // Allows to click the "Finish" button
        setPageComplete(true);
	}

	private void createRefactoringsTextArea(Composite parent) {
		// TODO Auto-generated method stub
		
		
		refactoredText = new Text(parent,SWT.MULTI | SWT.BORDER );
		refactoredText.setEnabled(false);
		refactoredText.setText(ApplyRefactoringsJob.refactoredContent);
		
		refactoredText.setLayoutData(new GridData(GridData.FILL_HORIZONTAL | GridData.FILL_VERTICAL));
		
	}

}

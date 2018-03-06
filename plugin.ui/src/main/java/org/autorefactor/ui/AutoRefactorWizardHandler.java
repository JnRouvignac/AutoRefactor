package org.autorefactor.ui;

import static org.eclipse.jface.dialogs.MessageDialog.openInformation;

import java.io.PrintWriter;
import java.io.StringWriter;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;

public class AutoRefactorWizardHandler extends AbstractHandler{

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		// TODO Auto-generated method stub

		   final Shell shell = HandlerUtil.getActiveShell(event);
	        try {
	            // Retrieve the targeted java element before the menu item is disposed by the framework
	        	//final List<RefactoringRule> refactoringRules = chooseRefactoringsPage.getSelectedRefactorings();
//	            ApplyRefactoringListener applyRefactoringListener = new ApplyRefactoringListener();
//	            PrepareApplyRefactoringsJob prepareApplyRefactoringJob =  new PrepareApplyRefactoringsJob( AutoRefactorHandler.getSelectedJavaElements(event),
//	            		refactoringRules, getEnvironment(), applyRefactoringListener);
//	          //  prepareApplyRefactoringJob.addJobChangeListener(applyRefactoringListener);
//	            prepareApplyRefactoringJob.schedule();
//
	            //return !refactoringRules.isEmpty();

	        } catch (final Exception e) {
	            Display.getDefault().asyncExec(new Runnable() {
	                @Override
	                public void run() {
	                    final StringWriter sw = new StringWriter();
	                    final PrintWriter pw = new PrintWriter(sw);
	                    e.printStackTrace(pw);

	                    openInformation(shell, "Info", "An error has occurred:\n\n" + sw.toString());
	                }
	            });
	        }
		return null;
	}






}

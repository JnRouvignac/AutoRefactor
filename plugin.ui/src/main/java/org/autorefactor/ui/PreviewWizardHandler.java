package org.autorefactor.ui;

import java.io.PrintWriter;
import java.io.StringWriter;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;

/**
 * Preview wizard handler.
 */
public class PreviewWizardHandler extends AbstractHandler {
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        final Shell shell = HandlerUtil.getActiveShell(event);

        try {
            Display.getDefault().asyncExec(new Runnable() {
                @Override
                public void run() {
                    final Wizard wizard = new PreviewWizard();
                    final WizardDialog wizardDialog = new WizardDialog(shell, wizard);
                    wizardDialog.open();
                }
            });
        } catch (final Exception e) {
            Display.getDefault().asyncExec(new Runnable() {
                @Override
                public void run() {
                    final StringWriter sw = new StringWriter();
                    final PrintWriter pw = new PrintWriter(sw);
                    e.printStackTrace(pw);
                }
            });
        }
        return null;
    }
}

/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2017 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016-2017 Fabrice Tiercelin - Add a configuration for each rule
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program under LICENSE-GNUGPL.  If not, see
 * <http://www.gnu.org/licenses/>.
 *
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution under LICENSE-ECLIPSE, and is
 * available at http://www.eclipse.org/legal/epl-v10.html
 */
package org.autorefactor.ui.preferences;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.autorefactor.AutoRefactorPlugin;
import org.autorefactor.jdt.internal.corext.dom.RefactoringRule;
import org.autorefactor.jdt.internal.ui.fix.AllCleanUpRules;
import org.autorefactor.preferences.PreferenceConstants;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/** The Eclipse preference page for AutoRefactor. */
public class WorkspacePreferencePage extends PreferencePage implements IWorkbenchPreferencePage {
	/**
	 * The fields.
	 */
	protected List<FieldEditor> fields;

	private Button toggleAllRules;

	private List<BooleanFieldEditor> rules;

	private FieldEditor invalidFieldEditor;

	private Composite fieldEditorParent;

	/** Default constructor. */
	public WorkspacePreferencePage() {
		super("AutoRefactor workbench preferences"); //$NON-NLS-1$
		setPreferenceStore(AutoRefactorPlugin.getDefault().getPreferenceStore());
	}

	/**
	 * Get the property name.
	 *
	 * @param refactoringRule a cleanup rule
	 * @return the property name
	 */
	public String getPropertyName(final RefactoringRule refactoringRule) {
		return refactoringRule.getClass().getCanonicalName();
	}

	/**
	 * Initialization.
	 *
	 * @param workbench The workbench
	 */
	@Override
	public void init(IWorkbench workbench) {
	}

	@Override
	protected Control createContents(Composite parent) {
		final List<RefactoringRule> allRefactoringRules= AllCleanUpRules.getAllCleanUpRules();
		Collections.sort(allRefactoringRules, Comparator.comparing(RefactoringRule::getName));

		final Group ruleGroup= createControls(parent, allRefactoringRules);

		initialize();
		invalidateToggleRules(ruleGroup);

		checkState();
		return fieldEditorParent;
	}

	private Group createControls(final Composite parent, final List<RefactoringRule> allRefactoringRules) {
		fieldEditorParent= new Composite(parent, SWT.FILL);

		initFields(allRefactoringRules);

		final Group ruleGroup= new Group(fieldEditorParent, SWT.FILL);
		ruleGroup.setText("Rules by default"); //$NON-NLS-1$

		// All rule checkbox
		toggleAllRules= new Button(ruleGroup, SWT.CHECK | SWT.LEFT);
		toggleAllRules.setFont(ruleGroup.getFont());
		toggleAllRules.setText("Toggle all the rules"); //$NON-NLS-1$
		toggleAllRules.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				boolean isSelected= WorkspacePreferencePage.this.toggleAllRules.getSelection();
				for (BooleanFieldEditor rule : WorkspacePreferencePage.this.rules) {
					((Button) rule.getDescriptionControl(ruleGroup)).setSelection(isSelected);
				}
			}
		});

		// Add a space
		Composite spacer= new Composite(ruleGroup, SWT.NULL);
		spacer.setLayoutData(new GridData(0, 5));

		rules= new ArrayList<>(allRefactoringRules.size());
		for (RefactoringRule refactoringRule : allRefactoringRules) {
			final BooleanFieldEditor booleanFieldEditor= new BooleanFieldEditor(getPropertyName(refactoringRule),
					refactoringRule.getName(), SWT.WRAP, ruleGroup);
			booleanFieldEditor.getDescriptionControl(ruleGroup).setToolTipText(refactoringRule.getDescription());
			((Button) booleanFieldEditor.getDescriptionControl(ruleGroup)).addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(final SelectionEvent e) {
					invalidateToggleRules(ruleGroup);
				}
			});
			rules.add(booleanFieldEditor);
		}
		fields.addAll(rules);
		return ruleGroup;
	}

	/**
	 * Initialize the fields.
	 *
	 * @param allRefactoringRules allCleanupRules
	 */
	protected void initFields(final List<RefactoringRule> allRefactoringRules) {
		fields= new ArrayList<>(1 + allRefactoringRules.size());

		fields.add(new BooleanFieldEditor(PreferenceConstants.DEBUG_MODE_ON.getName(), PreferenceConstants.DEBUG_MODE_ON.getDescription(), fieldEditorParent));
	}

	private void invalidateToggleRules(final Composite ruleGroup) {
		boolean isAllRulesChecked= true;
		for (BooleanFieldEditor rule : WorkspacePreferencePage.this.rules) {
			isAllRulesChecked= ((Button) rule.getDescriptionControl(ruleGroup)).getSelection();
			if (!isAllRulesChecked) {
				break;
			}
		}
		toggleAllRules.setSelection(isAllRulesChecked);
	}

	/** Initialize. */
	protected void initialize() {
		if (fields != null) {
			for (FieldEditor field : fields) {
				field.setPage(this);
				field.setPreferenceStore(getPreferenceStore());
				field.load();
			}
		}
	}

	/** Check the state. */
	protected void checkState() {
		boolean valid= true;
		invalidFieldEditor= null;

		if (fields != null) {
			for (FieldEditor field : fields) {
				valid= field.isValid();
				if (!valid) {
					invalidFieldEditor= field;
					break;
				}
			}
		}
		setValid(valid);
	}

	@Override
	public void setVisible(boolean visible) {
		super.setVisible(visible);
		if (visible && invalidFieldEditor != null) {
			invalidFieldEditor.setFocus();
		}
	}

	@Override
	protected void performDefaults() {
		if (fields != null) {
			for (FieldEditor field : fields) {
				field.loadDefault();
			}
		}

		checkState();
		super.performDefaults();
	}

	@Override
	public boolean performOk() {
		if (fields != null) {
			for (FieldEditor field : fields) {
				field.store();
			}
		}

		return super.performOk();
	}
}

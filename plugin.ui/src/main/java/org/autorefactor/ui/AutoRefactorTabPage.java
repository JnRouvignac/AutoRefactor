/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice Tiercelin - initial API and implementation
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
package org.autorefactor.ui;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

import org.autorefactor.AutoRefactorPlugin;
import org.autorefactor.jdt.internal.corext.dom.RefactoringRule;
import org.autorefactor.jdt.internal.ui.fix.AllCleanUpRules;
import org.eclipse.jdt.internal.ui.fix.MapCleanUpOptions;
import org.eclipse.jdt.internal.ui.preferences.cleanup.CleanUpTabPage;
import org.eclipse.jdt.ui.cleanup.CleanUpOptions;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;

/** AutoRefactorTabPage. */
public class AutoRefactorTabPage extends CleanUpTabPage {
    /**
     * The fields.
     */
    protected List<FieldEditor> fields;

    private Button toggleAllRules;

    private List<BooleanFieldEditor> rules;

    private Composite fieldEditorParent;

    @Override
    public void setWorkingValues(Map<String, String> workingValues) {
        super.setWorkingValues(workingValues);
        setOptions(new MapCleanUpOptions(workingValues));
    }

    /**
     * Create the contents.
     *
     * @param parent parent
     * @return Composite
     */
    @Override
    public Composite createContents(Composite parent) {
        final List<RefactoringRule> allRefactoringRules= AllCleanUpRules.getAllCleanUpRules();
        Collections.sort(allRefactoringRules, new Comparator<RefactoringRule>() {
            /**
             * Compare objects.
             *
             * @param o1 First item
             * @param o2 Second item
             *
             * @return -1, 0 or 1
             */
            public int compare(final RefactoringRule o1, final RefactoringRule o2) {
                return o1.getName().compareTo(o2.getName());
            }

        });

        final Group ruleGroup= createControls(parent, allRefactoringRules);

        initialize();
        invalidateToggleRules(ruleGroup);

        return fieldEditorParent;
    }

    private Group createControls(final Composite parent, final List<RefactoringRule> allRefactoringRules) {
        fieldEditorParent= new Composite(parent, SWT.FILL);
        fieldEditorParent.setLayout(new FillLayout());

        ScrolledComposite scrolledComposite= new ScrolledComposite(fieldEditorParent,
                SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);

        initFields(allRefactoringRules);

        Composite composite= new Composite(scrolledComposite, SWT.FILL);
        final Group ruleGroup= new Group(composite, SWT.FILL);
        ruleGroup.setText("Rules by default"); //$NON-NLS-1$

        // All rule checkbox
        toggleAllRules= new Button(ruleGroup, SWT.CHECK | SWT.LEFT);
        toggleAllRules.setFont(ruleGroup.getFont());
        toggleAllRules.setText("Toggle all the rules"); //$NON-NLS-1$
        toggleAllRules.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                boolean isSelected= AutoRefactorTabPage.this.toggleAllRules.getSelection();

                for (BooleanFieldEditor rule : AutoRefactorTabPage.this.rules) {
                    ((Button) rule.getDescriptionControl(ruleGroup)).setSelection(isSelected);
                    rule.getPreferenceStore().setValue(rule.getPreferenceName(), isSelected);
                }

                AutoRefactorPlugin.getDefault().getPreferenceStore().needsSaving();
            }
        });

        // Add a space
        Composite spacer= new Composite(ruleGroup, SWT.NULL);
        spacer.setLayoutData(new GridData(0, 5));

        rules= new ArrayList<BooleanFieldEditor>(allRefactoringRules.size());

        for (RefactoringRule refactoringRule : allRefactoringRules) {
            final BooleanFieldEditor booleanFieldEditor= new BooleanFieldEditor(getPropertyName(refactoringRule),
                    refactoringRule.getName(), SWT.WRAP, ruleGroup);

            booleanFieldEditor.getDescriptionControl(ruleGroup).setToolTipText(refactoringRule.getDescription());
            ((Button) booleanFieldEditor.getDescriptionControl(ruleGroup)).addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(final SelectionEvent e) {
                    final String isPreferenceEnabled= booleanFieldEditor.getBooleanValue() ? CleanUpOptions.TRUE
                            : CleanUpOptions.FALSE;

                    booleanFieldEditor.getPreferenceStore().setValue(booleanFieldEditor.getPreferenceName(),
                            isPreferenceEnabled);
                    booleanFieldEditor.getPreferenceStore().needsSaving();
                    invalidateToggleRules(ruleGroup);
                }
            });

            rules.add(booleanFieldEditor);
        }
        fields.addAll(rules);
        scrolledComposite.setContent(composite);
        ruleGroup.pack();
        composite.pack();
        return ruleGroup;
    }

    /**
     * Initialize the fields.
     *
     * @param allRefactoringRules all the cleanup rules
     */
    protected void initFields(final List<RefactoringRule> allRefactoringRules) {
        fields= new ArrayList<FieldEditor>(allRefactoringRules.size());
    }

    private void invalidateToggleRules(final Composite ruleGroup) {
        boolean isAllRulesChecked= true;
        for (BooleanFieldEditor rule : this.rules) {
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
                field.setPreferenceStore(AutoRefactorPlugin.getDefault().getPreferenceStore());
                field.load();
            }
        }
    }

    /**
     * Get the clean up count.
     *
     * @return int
     */
    @Override
    public int getCleanUpCount() {
        if (fields != null) {
            return fields.size();
        }
        return 0;
    }

    /**
     * Get the preview.
     *
     * @return String
     */
    public String getPreview() {
        return "// no preview"; //$NON-NLS-1$
    }

    /**
     * Get the selected clean up count.
     *
     * @return int
     */
    @Override
    public int getSelectedCleanUpCount() {
        int selectedCleanups= 0;

        if (fields != null) {
            for (FieldEditor field : fields) {
                if (((BooleanFieldEditor) field).getBooleanValue()) {
                    selectedCleanups++;
                }
            }
        }

        return selectedCleanups;
    }

    /**
     * Set the options.
     *
     * @param options options
     */
    public void setOptions(CleanUpOptions options) {
        if (fields != null) {
            for (FieldEditor field : fields) {
                final String isPreferenceEnabled= ((BooleanFieldEditor) field).getBooleanValue() ? CleanUpOptions.TRUE
                        : CleanUpOptions.FALSE;

                options.setOption(field.getPreferenceName(), isPreferenceEnabled);
                field.getPreferenceStore().setValue(field.getPreferenceName(), isPreferenceEnabled);
            }
        }
    }

    /**
     * Get the property name.
     *
     * @param refactoringRule cleanup rule
     * @return cleanupRule
     */
    public String getPropertyName(final RefactoringRule refactoringRule) {
        return getCleanupPropertyName(refactoringRule);
    }

    /**
     * Get the cleanup property name.
     *
     * @param refactoringRule a cleanup rule
     * @return the cleanup property name
     */
    public static String getCleanupPropertyName(final RefactoringRule refactoringRule) {
        return refactoringRule.getClass().getCanonicalName() + ":cleanup"; //$NON-NLS-1$
    }

    @Override
    protected void doCreatePreferences(Composite paramComposite, int paramInt) {
    }
}

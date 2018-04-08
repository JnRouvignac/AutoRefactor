/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2015 Jean-Noël Rouvignac - initial API and implementation
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

import static org.eclipse.jface.viewers.CheckboxTableViewer.newCheckList;
import static org.eclipse.swt.SWT.BORDER;
import static org.eclipse.swt.SWT.CHECK;
import static org.eclipse.swt.SWT.HIDE_SELECTION;
import static org.eclipse.swt.SWT.H_SCROLL;
import static org.eclipse.swt.SWT.NO_FOCUS;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.autorefactor.refactoring.RefactoringRule;
import org.autorefactor.refactoring.rules.AllRefactoringRules;
import org.autorefactor.util.UnhandledException;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ColumnViewerToolTipSupport;
import org.eclipse.jface.viewers.ICheckStateProvider;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.StyledCellLabelProvider;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.jface.viewers.StyledString.Styler;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.window.ToolTip;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.TextStyle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;

/**
 * Wizard page which allows the user to choose which refactorings to apply to the selected java elements.
 */
public class ChooseRefactoringWizardPage extends WizardPage {

    private final class CheckStateProvider implements ICheckStateProvider {

        public CheckStateProvider(List<? extends Object> refactorings) {
            for (Object refactoring : refactorings) {
                checkedState.put(refactoring, Boolean.FALSE);
            }
        }

        /**
         * True if it is checked.
         *
         * @param element The element
         *
         * @return True if it is checked.
         */
        public boolean isChecked(Object element) {
            return Boolean.TRUE.equals(checkedState.get(element));
        }

        /**
         * True if it is grayed.
         *
         * @param element The element
         *
         * @return True if it is grayed.
         */
        public boolean isGrayed(Object element) {
            return false;
        }
    }

    private final HashMap<Object, Boolean> checkedState = new HashMap<Object, Boolean>();
    private Text filterText;
    private CheckboxTableViewer tableViewer;
    private Button selectAllVisibleCheckbox;

    private final Styler defaultStyler = new Styler() {
        @Override
        public void applyStyles(TextStyle textStyle) {
            // no specific style
        }
    };

    private final Styler underlineStyler = new Styler() {
        @Override
        public void applyStyles(TextStyle style) {
            style.underline = true;
        }
    };


    ChooseRefactoringWizardPage() {
        super("Choose refactorings...");
        setTitle("Choose refactorings...");
        setDescription("Choose the refactorings to perform automatically");
    }

    /**
     * Returns the refactorings (selected by the user) to apply to the selected elements.
     *
     * @return the refactorings (selected by the user) to apply to the selected elements
     */
    public List<RefactoringRule> getSelectedRefactorings() {
        final ArrayList<RefactoringRule> results = new ArrayList<RefactoringRule>();
        for (Object o : tableViewer.getCheckedElements()) {
            results.add((RefactoringRule) o);
        }
        return results;
    }

    /**
     * create the control.
     *
     * @param parent The parent
     */
    public void createControl(Composite parent) {
        parent.setLayout(new GridLayout());

        createFilterText(parent);
        createSelectAllCheckbox(parent);
        createRefactoringsTable(parent);

        // required to avoid an error in the system
        setControl(parent);
        // Allows to click the "Finish" button
        setPageComplete(true);
    }

    private void createFilterText(Composite parent) {
        filterText = new Text(parent, SWT.BORDER | SWT.SINGLE);
        filterText.setMessage("Type in to filter refactorings");
        filterText.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));

        filterText.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent event) {
                // trigger a call to StyledCellLabelProvider.update()
                tableViewer.refresh(true);
            }
        });
    }

    private void createSelectAllCheckbox(Composite parent) {
        selectAllVisibleCheckbox = new Button(parent, SWT.CHECK);
        selectAllVisibleCheckbox.setText("Toggle all the visible refactorings");
        selectAllVisibleCheckbox.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                final Object[] visibleElements = filter(tableViewer, tableViewer.getInput());
                for (Object element : visibleElements) {
                    setChecked(element, selectAllVisibleCheckbox.getSelection());
                }
            }

            private Object[] filter(StructuredViewer viewer, Object input) {
                try {
                    final Class<StructuredViewer> clazz = StructuredViewer.class;
                    Method m = clazz.getDeclaredMethod("filter", Object[].class);
                    m.setAccessible(true);
                    return (Object[]) m.invoke(viewer, (Object) ((List<?>) input).toArray());
                } catch (Exception e) {
                    throw new UnhandledException(null, e);
                }
            }
        });
    }

    private void setChecked(Object element, boolean isChecked) {
        checkedState.put(element, isChecked);
        tableViewer.setChecked(element, isChecked);
    }

    private void createRefactoringsTable(Composite parent) {
        tableViewer = newCheckList(parent,
                BORDER | H_SCROLL | CHECK | NO_FOCUS | HIDE_SELECTION);
        createColumns(tableViewer);
        tableViewer.setContentProvider(new ArrayContentProvider());
        final List<RefactoringRule> refactorings = AllRefactoringRules.getAllRefactoringRules();
        tableViewer.setInput(refactorings);
        tableViewer.setCheckStateProvider(new CheckStateProvider(refactorings));
        tableViewer.setComparator(new ViewerComparator() {
            @Override
            public int compare(Viewer viewer, Object o1, Object o2) {
                return ((RefactoringRule) o1).getName().compareTo(
                        ((RefactoringRule) o2).getName());
            }
        });
        tableViewer.addFilter(new ViewerFilter() {
            @Override
            public boolean select(Viewer viewer, Object parentElement, Object refactoring) {
                final String filter = filterText.getText().toLowerCase();
                final RefactoringRule rule = (RefactoringRule) refactoring;
                final String description = rule.getDescription().toLowerCase();
                final String name = rule.getName().toLowerCase();
                return description.contains(filter)
                    || name.contains(filter);
            }
        });
        ColumnViewerToolTipSupport.enableFor(tableViewer, ToolTip.NO_RECREATE);
        tableViewer.setLabelProvider(new StyledCellLabelProvider() {
            @Override
            public void update(ViewerCell cell) {
                final String filter = filterText.getText().toLowerCase();
                final String name = ((RefactoringRule) cell.getElement()).getName();
                cell.setText(name);
                cell.setStyleRanges(getStyleRanges(name, filter));
            }

            private StyleRange[] getStyleRanges(String text, String filter) {
                final int matchIndex = text.toLowerCase().indexOf(filter);
                final int matchLength = filter.length();
                if (matchIndex != -1 && matchLength != 0) {
                    final StyledString styledString = new StyledString(text, defaultStyler);
                    styledString.setStyle(matchIndex, matchLength, underlineStyler);
                    return styledString.getStyleRanges();
                }
                return null;
            }

            @Override
            public String getToolTipText(Object refactoring) {
                RefactoringRule refactoringRule = (RefactoringRule) refactoring;
                return refactoringRule.getDescription() + "\n\nWhy to do so:\n" + refactoringRule.getReason();
            }

            @Override
            public Point getToolTipShift(Object object) {
                return new Point(10, 20);
            }
        });

        final Table table = tableViewer.getTable();
        table.setLinesVisible(false);
        tableViewer.getControl().setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, true));
        packColumns(table);
        table.setFocus();
    }

    private void createColumns(final TableViewer tableViewer) {
        TableViewerColumn refactoringColumn = createTableViewerColumn(tableViewer);
        refactoringColumn.setLabelProvider(new ColumnLabelProvider() {
            @Override
            public String getText(Object element) {
                return ((RefactoringRule) element).getName();
            }
        });
    }

    private TableViewerColumn createTableViewerColumn(TableViewer tableViewer) {
        final TableViewerColumn viewerColumn = new TableViewerColumn(tableViewer, SWT.NONE);
        final TableColumn column = viewerColumn.getColumn();
        column.setResizable(true);
        column.setMoveable(true);
        return viewerColumn;
    }

    private void packColumns(final Table table) {
        final int length = table.getColumns().length;
        for (int i = 0; i < length; i++) {
            table.getColumn(i).pack();
        }
    }

    @Override
    public void dispose() {
        checkedState.clear();
        filterText.dispose();
        tableViewer.getTable().dispose();
        selectAllVisibleCheckbox.dispose();
        super.dispose();
    }
}

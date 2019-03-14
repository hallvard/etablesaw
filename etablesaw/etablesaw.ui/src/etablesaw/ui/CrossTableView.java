package etablesaw.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

import tech.tablesaw.aggregate.CrossTab;
import tech.tablesaw.api.CategoricalColumn;
import tech.tablesaw.api.Table;

public class CrossTableView extends DerivedTableView implements TableProvider {

    public CrossTableView() {
        super("Cross table");
    }
    
	private Control rowCategorySelector;
	private Control columnCategorySelector;
	private Combo modeSelector;

	@Override
	protected void createConfigControls(final Composite configParent) {
		super.createConfigControls(configParent);
		rowCategorySelector = createColumnControl("Row category: ", configParent, null, CategoricalColumn.class);
		columnCategorySelector = createColumnControl("[Column Category]: ", configParent, null, CategoricalColumn.class);
		final Label modeLabel = new Label(configParent, SWT.NONE);
		modeLabel.setText("Count: ");
		modeSelector = new Combo(configParent, SWT.READ_ONLY);
		modeSelector.setItems("Count", "Row percent", "Column percent");
		modeSelector.select(0);
		modeSelector.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				updateTableControls();
			}
		});
	}

	@Override
	protected void updateConfigControls() {
		super.updateConfigControls();
		setColumnNames(rowCategorySelector, getViewTable());
		setColumnNames(columnCategorySelector, getViewTable());
	}

	@Override
	protected void configControlUpdated() {
		super.configControlUpdated();
		fireTableChanged(true);
	}

	@Override
	protected void updateTableControls() {
		final Table table = getViewTable();
		if (table != null) {
			final String[] rowCategories = getSelectedStrings(rowCategorySelector);
			final String[] columnCategories = getSelectedStrings(columnCategorySelector);
			if (rowCategories != null && rowCategories.length > 0) {
				final CategoricalColumn<?> rowCategory = table.categoricalColumn(rowCategories[0]);
				final CategoricalColumn<?> columnCategory = (columnCategories != null && columnCategories.length > 0 ? table.categoricalColumn(columnCategories[0]) : null);
				if (modeSelector.getSelectionIndex() == 0) {
					derivedTables[0] = (columnCategories != null && columnCategories.length > 0 ?
							CrossTab.counts(table, rowCategory, columnCategory) :
								CrossTab.counts(table, rowCategories[0])
							);
				} else {
					derivedTables[0] = (columnCategories != null && columnCategories.length > 0 ?
							(modeSelector.getSelectionIndex() == 1 ?
									CrossTab.rowPercents(table, rowCategory, columnCategory) :
										CrossTab.columnPercents(table, rowCategory, columnCategory)
									) :
										CrossTab.percents(table, rowCategories[0])
							);
				}
			}
		}
		super.updateTableControls();
		fireTableDataChanged(true);
	}
}

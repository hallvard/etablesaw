package etablesaw.ui.views;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

import etablesaw.ui.SimpleTableProvider;
import tech.tablesaw.aggregate.CrossTab;
import tech.tablesaw.api.CategoricalColumn;
import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.IntColumn;
import tech.tablesaw.api.NumberColumn;
import tech.tablesaw.api.NumericColumn;
import tech.tablesaw.api.StringColumn;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;

public class CrossTableView extends DerivedTableView {

    public CrossTableView() {
        super("Cross table");
    }
    
	private Control rowCategorySelector;
	private Control columnCategorySelector;
	private Combo modeSelector;
	private Button transButton;

	@Override
	protected void createConfigControls(final Composite configParent) {
		super.createConfigControls(configParent);
		rowCategorySelector = createColumnControl(configParent, "Row category: ", null, CategoricalColumn.class);
		columnCategorySelector = createColumnControl(configParent, "[Column Category]: ", null, CategoricalColumn.class);
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
		setControlLayout(modeSelector);
		transButton = new Button(configParent, SWT.CHECK);
		transButton.setText("Transpose");
		transButton.addSelectionListener(new SelectionAdapter() {
		    @Override
		    public void widgetSelected(final SelectionEvent e) {
		        updateTableControls();
		    }
		});
		transButton.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1));
	}

	@Override
	protected void updateConfigControls() {
		super.updateConfigControls();
		setColumnNames(rowCategorySelector, getViewTable(), CategoricalColumn.class);
		setColumnNames(columnCategorySelector, getViewTable(), CategoricalColumn.class);
	}

	@Override
	protected void configControlUpdated() {
		super.configControlUpdated();
		fireTableChanged();
	}

	@Override
	protected void updateTableControls() {
		final Table table = getViewTable();
		Table oldTable = derivedTables[0];
		if (table != null) {
			final String[] rowCategories = getSelectedStrings(rowCategorySelector);
			final String[] columnCategories = getSelectedStrings(columnCategorySelector);
			if (rowCategories != null && rowCategories.length > 0) {
				final CategoricalColumn<?> rowCategory = table.categoricalColumn(rowCategories[0]);
				final CategoricalColumn<?> columnCategory = (columnCategories != null && columnCategories.length > 0 ? table.categoricalColumn(columnCategories[0]) : null);
				if (isCountMode()) {
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
				if (transButton.getSelection()) {
				    derivedTables[0] = transposeTable(derivedTables[0]);
				}
			}
		}
		super.updateTableControls();
	    fireTableChanged(SimpleTableProvider.onlyTableDataChanged(oldTable, derivedTables[0]));
	}

    protected boolean isCountMode() {
        return modeSelector.getSelectionIndex() == 0;
    }

    private Table transposeTable(Table table) {
        Table transposed = Table.create("transposed");
        // create columns first
        StringColumn labelsColumn = StringColumn.create(table.column(0).name());
        transposed.addColumns(labelsColumn);
        boolean isintColumns = isCountMode();
        for (int row = 0; row < table.rowCount(); row++) {
            String colName = table.column(0).getString(row);
            transposed.addColumns(isintColumns ? IntColumn.create(colName) : DoubleColumn.create(colName));
        }
        // fill labels column
        for (Column<?> col : table.columns()) {
            labelsColumn.append(col.name());
        }
        // fill numeric columns
        for (int row = 0; row < table.rowCount(); row++) {
            NumberColumn<?> numberColumn = table.numberColumn(row + 1);
            for (NumericColumn<?> col : table.numberColumns()) {
                if (isintColumns) {
                    ((IntColumn) numberColumn).append(((IntColumn) col).getInt(row));
                } else {
                    ((DoubleColumn) numberColumn).append(((DoubleColumn) col).getDouble(row));                    
                }
            }
        }
        return transposed;
    }
}

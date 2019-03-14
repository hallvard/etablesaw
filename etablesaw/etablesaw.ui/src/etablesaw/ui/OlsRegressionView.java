package etablesaw.ui;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import smile.regression.OLS;
import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.NumericColumn;
import tech.tablesaw.api.Table;

public class OlsRegressionView extends DerivedTableView implements TableProvider {

    public OlsRegressionView() {
        super("Coefficients", "Values");
    }

    private Control dependentColumnsSelector;
	private Control independentColumnsSelector;

	@Override
	protected void createConfigControls(final Composite configParent) {
		super.createConfigControls(configParent);
		dependentColumnsSelector = createColumnControl("Dependent column: ", configParent, null, NumericColumn.class);
		independentColumnsSelector = createColumnControl("Independent columns: ", configParent, true, NumericColumn.class);
	}

	@Override
	protected void updateConfigControls() {
		super.updateConfigControls();
		setColumnNames(dependentColumnsSelector, getViewTable());
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
			final String[] dependentColumn = getSelectedStrings(dependentColumnsSelector);
			final String[] independentColumns = getSelectedStrings(independentColumnsSelector);
			if (dependentColumn != null && dependentColumn.length > 0 && independentColumns != null && independentColumns.length > 0) {
			    OLS ols = new OLS(table.smile().numericDataset(dependentColumn[0], independentColumns));
			    double[][] ttest = ols.ttest();
			    DoubleColumn[] ttestColumns = new DoubleColumn[4];
			    ttestColumns[0] = DoubleColumn.create("Coefficients");
			    ttestColumns[1] = DoubleColumn.create("Std. error");
			    ttestColumns[2] = DoubleColumn.create("t value");
			    ttestColumns[3] = DoubleColumn.create("Pr(>|t|");
			    for (int colNum = 0; colNum < 4; colNum++) {
			        int rowNum = 0;
			        for (; rowNum < independentColumns.length; rowNum++) {
			            ttestColumns[colNum].append(ttest[rowNum][colNum]);
			        }
			        ttestColumns[colNum].append(ttest[rowNum][colNum]);
			    }
			    derivedTables[0] = Table.create("Coefficients", ttestColumns);
			    derivedTables[1] = Table.create("Values", DoubleColumn.create("Residuals", ols.residuals()), DoubleColumn.create("Fitted", ols.fittedValues()));
			}
		}
		super.updateTableControls();
		fireTableDataChanged(true);
	}
}

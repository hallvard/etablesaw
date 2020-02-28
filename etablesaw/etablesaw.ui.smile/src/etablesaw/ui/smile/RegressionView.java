package etablesaw.ui.smile;

import etablesaw.ui.views.DerivedTableView;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import smile.regression.LinearModel;
import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.NumericColumn;
import tech.tablesaw.api.Table;

public abstract class RegressionView extends DerivedTableView {

    public RegressionView() {
        super(// "Coefficients",
                "Values");
    }

    private Control dependentColumnsSelector;
	private Control independentColumnsSelector;

	@Override
	protected void createConfigControls(final Composite configParent) {
		super.createConfigControls(configParent);
		dependentColumnsSelector = createColumnControl(configParent, "Dependent column: ", null, NumericColumn.class);
		independentColumnsSelector = createColumnControl(configParent, "Independent columns: ", true, NumericColumn.class);
	}

	@Override
	protected void updateConfigControls() {
		super.updateConfigControls();
		setColumnNames(dependentColumnsSelector, getViewTable(), NumericColumn.class);
	}

	@Override
	protected void configControlUpdated() {
		super.configControlUpdated();
		fireTableChanged();
	}

	@Override
	protected void updateTableControls() {
		final Table table = getViewTable();
		if (table != null) {
			final String[] dependentColumns = getSelectedStrings(dependentColumnsSelector);
			final String[] independentColumns = getSelectedStrings(independentColumnsSelector);
			if (dependentColumns != null && dependentColumns.length > 0 && independentColumns != null && independentColumns.length > 0) {
                String dependentColumn = dependentColumns[0];
                Table dataFrameTable = SmileHelper.createDataFrameTable(table, dependentColumn, independentColumns);
                LinearModel lm = getLinearModel(dataFrameTable, dependentColumn, independentColumns);
                if (lm != null) {
//    			    double[][] ttest = lm.ttest();
//    			    DoubleColumn[] ttestColumns = new DoubleColumn[4];
//    			    ttestColumns[0] = DoubleColumn.create("Coefficients");
//    			    ttestColumns[1] = DoubleColumn.create("Std. error");
//    			    ttestColumns[2] = DoubleColumn.create("t value");
//    			    ttestColumns[3] = DoubleColumn.create("Pr(>|t|");
//    			    for (int colNum = 0; colNum < 4; colNum++) {
//    			        for (int rowNum = 0; rowNum < independentColumns.length; rowNum++) {
//    			            ttestColumns[colNum].append(ttest[rowNum][colNum]);
//    			        }
//    			    }
//    			    derivedTables[0] = Table.create("Coefficients", ttestColumns);
    			    NumericColumn<?> values = dataFrameTable.numberColumn(dependentColumns[0]);
                    DoubleColumn residuals = DoubleColumn.create("Residuals", lm.residuals());
                    DoubleColumn fitted = values.add(residuals);
                    fitted.setName("Fitted");
                    derivedTables[0] = Table.create("Values", values, fitted, residuals);
                }
			}
		}
		super.updateTableControls();
		fireTableDataChanged();
	}

    protected abstract LinearModel getLinearModel(Table table, String dependentColumn, final String[] independentColumns);
}

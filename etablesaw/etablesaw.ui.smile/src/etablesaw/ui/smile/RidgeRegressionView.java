package etablesaw.ui.smile;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import etablesaw.ui.views.DerivedTableView;
import smile.data.AttributeDataset;
import smile.regression.RidgeRegression;
import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.NumberColumn;
import tech.tablesaw.api.NumericColumn;
import tech.tablesaw.api.Table;

public class RidgeRegressionView extends DerivedTableView {

    public RidgeRegressionView() {
        super("Coefficients", "Values");
    }

    private Control dependentColumnsSelector;
	private Control independentColumnsSelector;
	private Control lambdaControl;
	@Override
	protected void createConfigControls(final Composite configParent) {
		super.createConfigControls(configParent);
		dependentColumnsSelector = createColumnControl(configParent, "Dependent column: ", null, NumericColumn.class);
		independentColumnsSelector = createColumnControl(configParent, "Independent columns: ", true, NumericColumn.class);
		lambdaControl = createNumbericParameterControl(configParent, "Lambda: ", Double.class, 0.05d);
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
			final String[] dependentColumn = getSelectedStrings(dependentColumnsSelector);
			final String[] independentColumns = getSelectedStrings(independentColumnsSelector);
			if (dependentColumn != null && dependentColumn.length > 0 && independentColumns != null && independentColumns.length > 0) {
			    Double lambda = getNumbericParameter(lambdaControl, Double.class, Double.NaN);
			    if (! Double.isNaN(lambda)) {
    			    AttributeDataset numericDataset = table.smile().numericDataset(dependentColumn[0], independentColumns);
                    RidgeRegression ridge = new RidgeRegression(numericDataset.x(), numericDataset.y(), lambda);
    			    double[][] ttest = ridge.ttest();
    			    DoubleColumn[] ttestColumns = new DoubleColumn[4];
    			    ttestColumns[0] = DoubleColumn.create("Coefficients");
    			    ttestColumns[1] = DoubleColumn.create("Std. error");
    			    ttestColumns[2] = DoubleColumn.create("t value");
    			    ttestColumns[3] = DoubleColumn.create("Pr(>|t|");
    			    for (int colNum = 0; colNum < 4; colNum++) {
    			        for (int rowNum = 0; rowNum < independentColumns.length; rowNum++) {
    			            ttestColumns[colNum].append(ttest[rowNum][colNum]);
    			        }
    			    }
    			    derivedTables[0] = Table.create("Coefficients", ttestColumns);
    			    NumberColumn<?> values = table.numberColumn(dependentColumn[0]);
                    DoubleColumn residuals = DoubleColumn.create("Residuals", ridge.residuals());
                    DoubleColumn fitted = values.add(residuals);
                    fitted.setName("Fitted");
                    derivedTables[1] = Table.create("Values", values, fitted, residuals);
			    }
			}
		}
		super.updateTableControls();
		fireTableDataChanged();
	}
}

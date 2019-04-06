package etablesaw.ui.smile;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import etablesaw.ui.views.DerivedTableView;
import smile.data.AttributeDataset;
import smile.regression.LASSO;
import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.NumberColumn;
import tech.tablesaw.api.NumericColumn;
import tech.tablesaw.api.Table;

public class LassoRegressionView extends DerivedTableView {

    public LassoRegressionView() {
        super("Coefficients", "Values");
    }

    private Control dependentColumnsSelector;
	private Control independentColumnsSelector;
	private Control lambdaControl;
	private Control toleranceControl;
	private Control maxIterationsControl;

	@Override
	protected void createConfigControls(final Composite configParent) {
		super.createConfigControls(configParent);
		dependentColumnsSelector = createColumnControl(configParent, "Dependent column: ", null, NumericColumn.class);
		independentColumnsSelector = createColumnControl(configParent, "Independent columns: ", true, NumericColumn.class);
		lambdaControl = createNumbericParameterControl(configParent, "Lambda: ", Double.class, 0.05d);
		toleranceControl = createNumbericParameterControl(configParent, "Tolerance: ", Double.class, 0.001d);
		maxIterationsControl = createNumbericParameterControl(configParent, "Max. iterations: ", Integer.class, 5000);
	}

	@Override
	protected void updateConfigControls() {
		super.updateConfigControls();
		setColumnNames(dependentColumnsSelector, getViewTable(), NumericColumn.class);
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
			    double lambda = getNumbericParameter(lambdaControl, Double.class, Double.NaN);
			    double tolerance = getNumbericParameter(toleranceControl, Double.class, Double.NaN);
			    int maxIter = getNumbericParameter(maxIterationsControl, Integer.class, -1);
			    if ((! Double.isNaN(lambda) && ( Double.isNaN(tolerance)) && maxIter > 0)) {
    			    AttributeDataset numericDataset = table.smile().numericDataset(dependentColumn[0], independentColumns);
                    LASSO lasso = new LASSO(numericDataset.x(), numericDataset.y(), lambda, tolerance, maxIter);
    			    double[] coefficients = lasso.residuals();
    			    DoubleColumn[] testColumns = new DoubleColumn[1];
    			    testColumns[0] = DoubleColumn.create("Coefficients");
			        for (int rowNum = 0; rowNum < independentColumns.length; rowNum++) {
			            testColumns[0].append(coefficients[rowNum]);
			        }
    			    derivedTables[0] = Table.create("Coefficients", testColumns);
    			    NumberColumn<?> values = table.numberColumn(dependentColumn[0]);
                    DoubleColumn residuals = DoubleColumn.create("Residuals", lasso.residuals());
                    DoubleColumn fitted = values.add(residuals);
                    fitted.setName("Fitted");
                    derivedTables[1] = Table.create("Values", values, fitted, residuals);
			    }
			}
		}
		super.updateTableControls();
		fireTableDataChanged(true);
	}
}

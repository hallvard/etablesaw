package etablesaw.ui.smile;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import smile.data.DataFrame;
import smile.data.formula.Formula;
import smile.regression.LinearModel;
import smile.regression.RidgeRegression;
import tech.tablesaw.api.Table;

public class RidgeRegressionView extends RegressionView {

	private Control lambdaControl;

	@Override
	protected void createConfigControls(final Composite configParent) {
		super.createConfigControls(configParent);
		lambdaControl = createNumbericParameterControl(configParent, "Lambda: ", Double.class, 0.05d);
	}

    protected LinearModel getLinearModel(Table table, String dependentColumn, final String[] independentColumns) {
        double lambda = getNumbericParameter(lambdaControl, Double.class, Double.NaN);
        if (Double.isNaN(lambda)) {
            return null;
        }
        DataFrame dataFrame = SmileHelper.createDataFrame(table, dependentColumn, independentColumns);        
        return RidgeRegression.fit(Formula.of(dependentColumn, independentColumns), dataFrame, lambda);
    }
}

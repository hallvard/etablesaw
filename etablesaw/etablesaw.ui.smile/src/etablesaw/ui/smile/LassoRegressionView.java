package etablesaw.ui.smile;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import smile.data.DataFrame;
import smile.data.formula.Formula;
import smile.regression.LASSO;
import smile.regression.LinearModel;
import tech.tablesaw.api.Table;

public class LassoRegressionView extends RegressionView {

	private Control lambdaControl;
	private Control toleranceControl;
	private Control maxIterationsControl;

	@Override
	protected void createConfigControls(final Composite configParent) {
		super.createConfigControls(configParent);
		lambdaControl = createNumbericParameterControl(configParent, "Lambda: ", Double.class, 0.05d);
		toleranceControl = createNumbericParameterControl(configParent, "Tolerance: ", Double.class, 0.001d);
		maxIterationsControl = createNumbericParameterControl(configParent, "Max. iterations: ", Integer.class, 5000);
	}

    protected LinearModel getLinearModel(Table table, String dependentColumn, final String[] independentColumns) {
        double lambda = getNumbericParameter(lambdaControl, Double.class, Double.NaN);
        double tolerance = getNumbericParameter(toleranceControl, Double.class, Double.NaN);
        int maxIter = getNumbericParameter(maxIterationsControl, Integer.class, -1);
        if (Double.isNaN(lambda) || Double.isNaN(tolerance) || maxIter <= 0) {
            return null;
        }
        DataFrame dataFrame = SmileHelper.createDataFrame(table, dependentColumn, independentColumns);
        return LASSO.fit(Formula.of(dependentColumn, independentColumns), dataFrame, lambda, tolerance, maxIter);
    }
}

package etablesaw.ui.smile;

import smile.data.DataFrame;
import smile.data.formula.Formula;
import smile.regression.LinearModel;
import smile.regression.OLS;
import tech.tablesaw.api.Table;

public class OlsRegressionView extends RegressionView {

	@Override
	protected void configControlUpdated() {
		super.configControlUpdated();
		fireTableChanged();
	}

    protected LinearModel getLinearModel(Table table, String dependentColumn, final String[] independentColumns) {
        DataFrame dataFrame = SmileHelper.createDataFrame(table, dependentColumn, independentColumns);
        return OLS.fit(Formula.of(dependentColumn, independentColumns),  dataFrame);
    }
}

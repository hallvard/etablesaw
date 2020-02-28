package etablesaw.ui.smile;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import smile.classification.KNN;
import smile.data.DataFrame;
import tech.tablesaw.api.IntColumn;
import tech.tablesaw.api.Table;

public class KnnClassificationView extends ClassificationView {

    public KnnClassificationView() {
        super();
    }

	private Control kControl;

	@Override
	protected void createConfigControls(final Composite configParent) {
		super.createConfigControls(configParent);
		kControl = createNumbericParameterControl(configParent, "K: ", Integer.class, 1);
	}

	@Override
    protected void updateDerivedTables(final Table table, final String categoryColumnName, final String[] independentColumns) {
        Table classTable = Table.create(table.name(), table.columnArray());
        CategoryMap<?> categoryMap = new CategoryMap<>(table.categoricalColumn(categoryColumnName), categoryColumnName + " classes");
	    IntColumn classColumn = categoryMap.getClassColumn();
	    classTable.addColumns(classColumn);
        DataFrame dataFrame = SmileHelper.createDataFrame(classTable, classColumn.name(), independentColumns);
        double[][] instances = dataFrame.select(independentColumns).toArray();

        int k = getNumbericParameter(kControl, Integer.class, -1);
        if (k > 0) {
            KNN<double[]> knn = KNN.fit(instances, dataFrame.apply(categoryColumnName).toIntArray(), k);
            setDerivedTables(knn, categoryMap, instances);
        }
	}
}

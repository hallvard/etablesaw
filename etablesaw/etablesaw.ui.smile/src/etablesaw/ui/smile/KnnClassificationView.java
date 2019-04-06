package etablesaw.ui.smile;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import smile.classification.KNN;
import smile.data.AttributeDataset;
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
	    int k = getNumbericParameter(kControl, Integer.class, 1);
	    AttributeDataset nominalDataset = classTable.smile().nominalDataset(classColumn.name(), independentColumns);
        double[][] instances = nominalDataset.x();
        KNN<double[]> knn = KNN.learn(instances, nominalDataset.labels(), k);
        setDerivedTables(knn, categoryMap, instances);
	}
}

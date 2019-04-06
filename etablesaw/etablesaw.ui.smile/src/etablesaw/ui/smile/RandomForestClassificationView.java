package etablesaw.ui.smile;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import smile.classification.RandomForest;
import smile.data.AttributeDataset;
import tech.tablesaw.api.IntColumn;
import tech.tablesaw.api.Table;

public class RandomForestClassificationView extends ClassificationView {

    public RandomForestClassificationView() {
        super();
    }

	private Control nTreesControl;

	@Override
	protected void createConfigControls(final Composite configParent) {
		super.createConfigControls(configParent);
		nTreesControl = createNumbericParameterControl(configParent, "Num trees: ", Integer.class, 100);
	}

	@Override
    protected void updateDerivedTables(final Table table, final String categoryColumnName, final String[] independentColumns) {
        Table classTable = Table.create(table.name(), table.columnArray());
        CategoryMap<?> categoryMap = new CategoryMap<>(table.categoricalColumn(categoryColumnName), categoryColumnName + " classes");
	    IntColumn classColumn = categoryMap.getClassColumn();
        classTable.addColumns(classColumn);
	    int nTrees = getNumbericParameter(nTreesControl, Integer.class, 1);
	    AttributeDataset nominalDataset = classTable.smile().nominalDataset(classColumn.name(), independentColumns);
        double[][] instances = nominalDataset.x();
        RandomForest rf = new RandomForest(nominalDataset, nTrees);
        setDerivedTables(rf, categoryMap, instances);
	}
}

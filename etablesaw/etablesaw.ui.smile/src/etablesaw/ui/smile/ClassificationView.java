package etablesaw.ui.smile;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import etablesaw.ui.views.DerivedTableView;
import smile.classification.SoftClassifier;
import tech.tablesaw.api.CategoricalColumn;
import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.NumericColumn;
import tech.tablesaw.api.Table;

public abstract class ClassificationView extends DerivedTableView {

    public ClassificationView() {
        super("Performance", "Values");
    }

    protected Control dependentColumnsSelector;
    protected Control independentColumnsSelector;

	@Override
	protected void createConfigControls(final Composite configParent) {
		super.createConfigControls(configParent);
		dependentColumnsSelector = createColumnControl(configParent, "Category column: ", null, CategoricalColumn.class);
		independentColumnsSelector = createColumnControl(configParent, "Independent columns: ", true, NumericColumn.class);
	}

	@Override
	protected void updateConfigControls() {
		super.updateConfigControls();
		setColumnNames(dependentColumnsSelector, getViewTable(), CategoricalColumn.class);
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
                updateDerivedTables(table, dependentColumn[0], independentColumns);
            }
            super.updateTableControls();
            fireTableDataChanged();
        }
    }

    protected abstract void updateDerivedTables(final Table table, final String dependentColumn, final String[] independentColumns);

    protected void setDerivedTables(SoftClassifier<double[]> knn, CategoryMap<?> categoryMap, double[][] instances) {
        int categoryCount = categoryMap.getCategoryCount();
        double[] posteriori = new double[categoryCount];
        DoubleColumn[] posterioriColumns = new DoubleColumn[posteriori.length];
        for (int j = 0; j < posterioriColumns.length; j++) {
            posterioriColumns[j] = DoubleColumn.create("posteriori " + categoryMap.getCategory(j), instances.length);
        }
        int[] actual = categoryMap.getClassArray();
        int[] prediction = new int[instances.length];
        int[] counts = new int[categoryCount], tps = new int[categoryCount], fps = new int[categoryCount];
        for (int i = 0; i < instances.length; i++) {
            int c = knn.predict(instances[i], posteriori);
            prediction[i] = c;
            counts[c]++;
            if (c == actual[i]) {
                tps[c]++;
            } else {
                fps[c]++;
            }
            for (int j = 0; j < posteriori.length; j++) {
                posterioriColumns[j].set(i, posteriori[j]);
            }
        }
        double[] precisions = new double[categoryCount], recalls = new double[categoryCount];
        for (int c = 0; c < categoryCount; c++) {
            precisions[c] = (double) tps[c] / (tps[c] + fps[c]);
            recalls[c] = (double) tps[c] / counts[c];
        }
        derivedTables[0] = Table.create("Performance", DoubleColumn.create("Precision", precisions), DoubleColumn.create("Recall", recalls));
        derivedTables[1] = Table.create("Values",
                categoryMap.getCategoricalColumn(),
                categoryMap.createCategoryColumn(prediction, categoryMap.getCategoricalColumn().name()+ " prediction")
//                        categoryMap.getClassColumn(),
//                        IntColumn.create(categoryColumnName + " prediction classes", prediction)
                );
        derivedTables[1].addColumns(posterioriColumns);
    }
}

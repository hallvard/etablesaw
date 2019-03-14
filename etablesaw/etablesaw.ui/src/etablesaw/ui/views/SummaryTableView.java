package etablesaw.ui.views;

import java.util.Arrays;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import tech.tablesaw.aggregate.AggregateFunction;
import tech.tablesaw.aggregate.Summarizer;
import tech.tablesaw.api.CategoricalColumn;
import tech.tablesaw.api.NumberColumn;
import tech.tablesaw.api.Table;

public class SummaryTableView extends DerivedTableView {

    public SummaryTableView() {
        super("Summary");
    }
    
	private Control numericsSelector;
	private Control categorySelector;
	private Control aggregateFunctionSelector;

	@Override
	protected void createConfigControls(final Composite configParent) {
		super.createConfigControls(configParent);
		numericsSelector = createColumnControl(configParent, "Numbers: ", true, NumberColumn.class);
		categorySelector = createColumnControl(configParent, "[Category]: ", true, CategoricalColumn.class);
		aggregateFunctionSelector = createAggregateFunctionSelector(configParent, "Aggregate with: ", true);
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
			final String[] numerics = getSelectedStrings(numericsSelector);
			final String[] categories = getSelectedStrings(categorySelector);
			final AggregateFunction<?, ?>[] aggregateFunctions = getAggregateFunctions(aggregateFunctionSelector, table, numerics);
			if (numerics != null && numerics.length > 0 && aggregateFunctions != null && aggregateFunctions.length > 0) {
				final Summarizer summarizer = table.summarize(Arrays.asList(numerics), aggregateFunctions);
				derivedTables[0] = ((categories == null || categories.length == 0) ? summarizer.apply() : summarizer.by(categories));
			}
		}
		super.updateTableControls();
		fireTableDataChanged(true);
	}
}

package etablesaw.ui.plots;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import tech.tablesaw.aggregate.AggregateFunction;
import tech.tablesaw.aggregate.Summarizer;
import tech.tablesaw.api.CategoricalColumn;
import tech.tablesaw.api.NumberColumn;
import tech.tablesaw.api.NumericColumn;
import tech.tablesaw.api.Table;
import tech.tablesaw.plotly.components.Figure;
import tech.tablesaw.plotly.components.Layout;
import tech.tablesaw.plotly.components.Page;
import tech.tablesaw.plotly.traces.BarTrace;
import tech.tablesaw.plotly.traces.Trace;

public class BarPlotView extends AbstractPlotView {

	private Control categorySelector;
	private Control numericsSelector;
	private Control aggregateFunctionSelector;

	@Override
	public void createConfigControls(final Composite parent) {
		super.createConfigControls(parent);
		categorySelector = createColumnControl(parent, "Category (x-axis): ", null, CategoricalColumn.class);
		numericsSelector = createColumnControl(parent, "Numbers: ", true, NumberColumn.class);
		aggregateFunctionSelector = createAggregateFunctionSelector(parent, "[Aggregate with]: ", true);
	}

	@Override
	protected void updateConfigControls() {
		super.updateConfigControls();
		setColumnNames(categorySelector, getViewTable(), CategoricalColumn.class);
		String[] categoryColumnNames = getColumnNames(getViewTable(), CategoricalColumn.class, 1);
		selectStrings(categorySelector, categoryColumnNames);
		setColumnNames(numericsSelector, getViewTable(), NumberColumn.class);
		String[] numberColumnNames = getColumnNames(getViewTable(), NumberColumn.class, -1);
		selectStrings(numericsSelector, removeColumnNames(numberColumnNames, categoryColumnNames));
	}

	@Override
	protected String computeBrowserContents(final Point size) {
		String[] numerics = getSelectedStrings(numericsSelector);
		final String[] categories = getSelectedStrings(categorySelector);
		if (hasElement(categories) && hasElement(numerics)) {
			Table table = getViewTable();
			final Collection<Trace> traces = new ArrayList<>();
			final AggregateFunction<?, ?>[] aggregateFunctions = getAggregateFunctions(aggregateFunctionSelector, table, numerics);
			if (aggregateFunctions != null && aggregateFunctions.length > 0) {
				final Summarizer summarizer = table.summarize(Arrays.asList(numerics), aggregateFunctions);
				table = summarizer.by(categories);
				final CategoricalColumn<?> categoricalColumn = table.categoricalColumn(categories[0]);
				for (NumericColumn<?> numberColumn : table.numberColumns()) {
					if (numberColumn != categoricalColumn) {
						final BarTrace trace = BarTrace.builder(
								categoricalColumn,
								numberColumn)
								.orientation(BarTrace.Orientation.VERTICAL)
								.showLegend(table.columnCount() - 1 > 1)
								.name(numberColumn.name())
								.build();
						traces.add(trace);
					}					
				}
			} else {
				final CategoricalColumn<?> categoricalColumn = table.categoricalColumn(categories[0]);
				for (int i = 0; i < numerics.length; i++) {
					final String name = String.valueOf(numerics[i]);
					final BarTrace trace = BarTrace.builder(
							categoricalColumn,
							table.numberColumn(name))
							.orientation(BarTrace.Orientation.VERTICAL)
							.showLegend(true)
							.name(name)
							.build();
					traces.add(trace);
				}
			}
			final Layout layout = getPlotLayout(size);
			final Figure figure = new Figure(layout, traces.toArray(new Trace[traces.size()]));
			final Page page = Page.pageBuilder(figure, "plot").build();
			final String html = page.asJavascript();
			return html;
		}
		return null;
	}
}

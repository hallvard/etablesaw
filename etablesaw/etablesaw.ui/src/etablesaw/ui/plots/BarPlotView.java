package etablesaw.ui.plots;

import java.util.Arrays;
import java.util.List;

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import tech.tablesaw.aggregate.AggregateFunction;
import tech.tablesaw.aggregate.Summarizer;
import tech.tablesaw.api.CategoricalColumn;
import tech.tablesaw.api.NumberColumn;
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
		categorySelector = createColumnControl(parent, "Category: ", null, CategoricalColumn.class);
		numericsSelector = createColumnControl(parent, "Numbers: ", true, NumberColumn.class);
		aggregateFunctionSelector = createAggregateFunctionSelector(parent, "[Aggregate with]: ", true);
	}

	@Override
	protected void updateConfigControls() {
		super.updateConfigControls();
		setColumnNames(categorySelector, getViewTable());
		setColumnNames(numericsSelector, getViewTable());
	}

	@Override
	protected String computeBrowserContents(final Point size) {
		String[] numerics = getSelectedStrings(numericsSelector);
		final String[] categories = getSelectedStrings(categorySelector);
		if (categories != null && categories.length > 0 && numerics != null && numerics.length > 0) {
			Table table = getViewTable();
			final AggregateFunction<?, ?>[] aggregateFunctions = getAggregateFunctions(aggregateFunctionSelector, table, numerics);
			final boolean summarize = aggregateFunctions != null && aggregateFunctions.length > 0;
			if (summarize) {
				final Summarizer summarizer = table.summarize(Arrays.asList(numerics), aggregateFunctions);
				table = ((categories == null || categories.length == 0) ? summarizer.apply() : summarizer.by(categories));
				final List<String> columnNames = table.columnNames();
				columnNames.removeAll(Arrays.asList(categories));
				numerics = columnNames.toArray(new String[columnNames.size()]);
			}
			final Layout layout = getPlotLayout(size);
			final Trace[] traces = new Trace[numerics.length];
			for (int i = 0; i < numerics.length; i++) {
				final String name = String.valueOf(numerics[i]);
				final CategoricalColumn<?> categoricalColumn = table.categoricalColumn(categories[0]);
				final NumberColumn<?> numberColumn = table.numberColumn(name);
				final BarTrace trace = BarTrace.builder(
						categoricalColumn,
						numberColumn)
						.orientation(BarTrace.Orientation.VERTICAL)
						.showLegend(numerics.length > 1)
						.name(name)
						.build();
				traces[i] = trace;
			}
			final Figure figure = new Figure(layout, traces);
			final Page page = Page.pageBuilder(figure, "plot").build();
			final String html = page.asJavascript();
			return html;
		}
		return null;
	}
}

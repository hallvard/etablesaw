package etablesaw.ui.plots;

import tech.tablesaw.api.CategoricalColumn;
import tech.tablesaw.api.NumericColumn;
import tech.tablesaw.plotly.api.ScatterPlot;
import tech.tablesaw.plotly.components.Figure;
import tech.tablesaw.plotly.components.Page;

public class ScatterPlotView extends XYPlotView {

	@Override
	protected String computeBrowserContents(final NumericColumn<?> x, final NumericColumn<?> y, final CategoricalColumn<?> category) {
		final Figure figure = (category == null
				? ScatterPlot.create(getViewTable().name(), getViewTable(), x.name(), y.name())
						: ScatterPlot.create(getViewTable().name(), getViewTable(), x.name(), y.name(), category.name()));
		final Page page = Page.pageBuilder(figure, "plot").build();
		return page.asJavascript();
	}
}

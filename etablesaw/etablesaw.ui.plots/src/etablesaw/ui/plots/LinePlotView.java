package etablesaw.ui.plots;

import tech.tablesaw.api.CategoricalColumn;
import tech.tablesaw.api.NumericColumn;
import tech.tablesaw.plotly.api.LinePlot;
import tech.tablesaw.plotly.components.Figure;
import tech.tablesaw.plotly.components.Page;

public class LinePlotView extends XYPlotView {

	@Override
	protected String computeBrowserContents(final NumericColumn<?> x, final NumericColumn<?> y, final CategoricalColumn<?> category) {
		final Figure figure = (category == null
				? LinePlot.create(getViewTable().name(), getViewTable(), x.name(), y.name())
						: LinePlot.create(getViewTable().name(), getViewTable(), x.name(), y.name(), category.name()));
		final Page page = Page.pageBuilder(figure, "plot").build();
		return page.asJavascript();
	}
}

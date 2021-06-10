package etablesaw.ui.plots;

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import tech.tablesaw.api.NumberColumn;
import tech.tablesaw.api.Table;
import tech.tablesaw.plotly.components.Figure;
import tech.tablesaw.plotly.components.Layout;
import tech.tablesaw.plotly.components.Page;
import tech.tablesaw.plotly.traces.HistogramTrace;

public class HistogramView extends AbstractPlotView {

	private Control numericsSelector;

	@Override
	public void createConfigControls(final Composite parent) {
		super.createConfigControls(parent);
		numericsSelector = createColumnControl(parent, "Numbers: ", null, NumberColumn.class);
	}

	@Override
	protected void updateConfigControls() {
		super.updateConfigControls();
		setColumnNames(numericsSelector, getViewTable(), NumberColumn.class);
	}

	@Override
	protected String computeBrowserContents(final Point size) {
		String[] numerics = getSelectedStrings(numericsSelector);
		if (numerics != null && numerics.length > 0) {
			Table table = getViewTable();
			final Layout layout = getPlotLayout(size);
	        HistogramTrace trace = HistogramTrace.builder(table.numberColumn(numerics[0])).build();
			final Figure figure = new Figure(layout, trace);
			final Page page = Page.pageBuilder(figure, "plot").build();
			final String html = page.asJavascript();
			return html;
		}
		return null;
	}
}

package etablesaw.ui.plots;

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import tech.tablesaw.api.CategoricalColumn;
import tech.tablesaw.api.NumericColumn;
import tech.tablesaw.api.Table;
import tech.tablesaw.plotly.api.BubblePlot;
import tech.tablesaw.plotly.components.Figure;
import tech.tablesaw.plotly.components.Page;

public class BubblePlotView extends AbstractPlotView {

	private Control xSelector;
	private Control ySelector;
	private Control sSelector;
	private Control categorySelector;

	@Override
	public void createConfigControls(final Composite parent) {
		super.createConfigControls(parent);
		xSelector = createColumnControl(parent, "X-axis: ", null, NumericColumn.class);
		ySelector = createColumnControl(parent, "Y-axis: ", null, NumericColumn.class);
		ySelector = createColumnControl(parent, "Size: ", null, NumericColumn.class);
		categorySelector = createColumnControl(parent, "[Category]: ", false, CategoricalColumn.class);
	}

	@Override
	protected void updateConfigControls() {
		super.updateConfigControls();
		setColumnNames(xSelector, getViewTable(), NumericColumn.class);
		setColumnNames(ySelector, getViewTable(), NumericColumn.class);
		setColumnNames(sSelector, getViewTable(), NumericColumn.class);
		setColumnNames(categorySelector, getViewTable(), CategoricalColumn.class);
	}

	@Override
	protected String computeBrowserContents(final Point size) {
		final String[] xs = getSelectedStrings(xSelector);
		final String[] ys = getSelectedStrings(ySelector);
		final String[] ss = getSelectedStrings(ySelector);
		final String[] categories = getSelectedStrings(categorySelector);
		if (hasElement(xs) && hasElement(ys) && hasElement(ss)) {
			final Table table = getViewTable();
			return computeBrowserContents(table.numberColumn(xs[0]), table.numberColumn(ys[0]), table.numberColumn(ss[0]),
					(categories != null && categories.length == 1 ? table.categoricalColumn(categories[0]) : null));
		}
		return null;
	}

	protected String computeBrowserContents(NumericColumn<?> x, NumericColumn<?> y, NumericColumn<?> s, CategoricalColumn<?> category) {
		final Figure figure = (category == null
				? BubblePlot.create(getViewTable().name(), getViewTable(), x.name(), y.name(), s.name())
						: BubblePlot.create(getViewTable().name(), getViewTable(), x.name(), y.name(), s.name(), category.name()));
		final Page page = Page.pageBuilder(figure, "plot").build();
		return page.asJavascript();
	}
}

package etablesaw.ui.plots;

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import tech.tablesaw.api.CategoricalColumn;
import tech.tablesaw.api.NumericColumn;
import tech.tablesaw.api.Table;

public abstract class XYPlotView extends AbstractPlotView {

	private Control xSelector;
	private Control ySelector;
	private Control categorySelector;

	@Override
	public void createConfigControls(final Composite parent) {
		super.createConfigControls(parent);
		xSelector = createColumnControl("x: ", parent, null, NumericColumn.class);
		ySelector = createColumnControl("y: ", parent, null, NumericColumn.class);
		categorySelector = createColumnControl("Category: ", parent, false, CategoricalColumn.class);
	}

	@Override
	protected void updateConfigControls() {
		super.updateConfigControls();
		setColumnNames(xSelector, getViewTable());
		setColumnNames(ySelector, getViewTable());
		setColumnNames(categorySelector, getViewTable());
	}

	@Override
	protected String computeBrowserContents(final Point size) {
		final String[] xs = getSelectedStrings(xSelector), ys = getSelectedStrings(ySelector);
		final String[] categories = getSelectedStrings(categorySelector);
		if (xs != null && xs.length == 1 && ys != null && ys.length == 1) {
			final Table table = getViewTable();
			return computeBrowserContents(table.numberColumn(xs[0]), table.numberColumn(ys[0]),
					(categories != null && categories.length == 1 ? table.categoricalColumn(categories[0]) : null));
		}
		return null;
	}

	protected abstract String computeBrowserContents(NumericColumn<?> x, NumericColumn<?> y, CategoricalColumn<?> category);
}

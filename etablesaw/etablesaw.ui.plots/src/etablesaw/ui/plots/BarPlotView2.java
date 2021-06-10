package etablesaw.ui.plots;

import java.util.List;

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import tech.tablesaw.api.CategoricalColumn;
import tech.tablesaw.api.NumberColumn;
import tech.tablesaw.api.NumericColumn;
import tech.tablesaw.api.Table;
import tech.tablesaw.plotly.components.Figure;
import tech.tablesaw.plotly.components.Layout;
import tech.tablesaw.plotly.components.Page;
import tech.tablesaw.plotly.traces.BarTrace;
import tech.tablesaw.plotly.traces.Trace;
import tech.tablesaw.table.TableSliceGroup;

public class BarPlotView2 extends AbstractPlotView {

	private Control categorySelector;
	private Control numericsSelector;
	private Control seriesCategorySelector;

	@Override
	public void createConfigControls(final Composite parent) {
		super.createConfigControls(parent);
		categorySelector = createColumnControl(parent, "Category (x-axis): ", null, CategoricalColumn.class);
		numericsSelector = createColumnControl(parent, "Numbers: ", null, NumberColumn.class);
		seriesCategorySelector = createColumnControl(parent, "Category (series): ", true, CategoricalColumn.class);
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
		setColumnNames(seriesCategorySelector, getViewTable(), CategoricalColumn.class);
		String[] seriesCategoryColumnNames = getColumnNames(getViewTable(), CategoricalColumn.class, -1);
		selectStrings(seriesCategorySelector, removeColumnNames(seriesCategoryColumnNames, categoryColumnNames, numberColumnNames));
	}

	@Override
	protected String computeBrowserContents(final Point size) {
		String[] numerics = getSelectedStrings(numericsSelector);
		final String[] categories = getSelectedStrings(categorySelector);
		if (hasElement(categories) && hasElement(numerics)) {
			Table[] tables = new Table[] { getViewTable() };
			final String[] seriesCategories = getSelectedStrings(seriesCategorySelector);
			if (seriesCategories != null && seriesCategories.length > 0) {
				TableSliceGroup slices = getViewTable().splitOn(seriesCategories);
				List<Table> sliceTables = slices.asTableList();
				tables = sliceTables.toArray(new Table[sliceTables.size()]);
			}
			final Layout layout = getPlotLayout(size);
			final Trace[] traces = new Trace[tables.length];
			for (int i = 0; i < tables.length; i++) {
				Table table = tables[i];
				final String name = numerics[0];
				final CategoricalColumn<?> categoricalColumn = table.categoricalColumn(categories[0]);
				NumericColumn<?> numberColumn = table.numberColumn(name);
				final BarTrace trace = BarTrace.builder(
						categoricalColumn,
						numberColumn)
						.orientation(BarTrace.Orientation.VERTICAL)
						.showLegend(tables.length > 1)
						.name(table.name())
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

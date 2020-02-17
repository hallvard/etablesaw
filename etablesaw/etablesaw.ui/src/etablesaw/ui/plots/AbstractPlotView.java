package etablesaw.ui.plots;

import org.eclipse.swt.graphics.Point;

import tech.tablesaw.plotly.components.Layout;
import tech.tablesaw.plotly.components.Margin;

public abstract class AbstractPlotView extends AbstractBrowserTableView {

	public AbstractPlotView() {
		super();
	}

	protected Layout getPlotLayout(final Point size) {
		final Layout layout = Layout.builder()
				.height(size.y)
				.width(size.x)
				.margin(getPlotMargin())
				.build();
		return layout;
	}

	protected Margin getPlotMargin() {
		return Margin.builder().left(40).right(20).top(10).bottom(25).build();
	}

	@Override
	public void setFocus() {
	}
}

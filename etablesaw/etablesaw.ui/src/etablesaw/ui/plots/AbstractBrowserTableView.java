package etablesaw.ui.plots;

import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;

import etablesaw.ui.Activator;
import etablesaw.ui.preferences.TablesawPreferenceInitializer;
import etablesaw.ui.views.AbstractTablesawView;
import tech.tablesaw.plotly.components.TemplateUtils;

public abstract class AbstractBrowserTableView extends AbstractTablesawView {

	public AbstractBrowserTableView() {
		super(false);
	}

	private Browser browser;

	@Override
	public void createTableDataControls(final Composite parent) {
	    super.createTableDataControls(parent);
		browser = new Browser(parent, SWT.NONE);
		browser.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		browser.setJavascriptEnabled(true);
	}

	protected abstract String computeBrowserContents(Point size);

	@Override
	protected void updateTableControls() {
		if (browser != null && (! browser.isDisposed())) {
			String html = null;
			if (getViewTable() != null) {
			    String templatesLocation = Activator.getInstance().getPreferenceStore().getString(TablesawPreferenceInitializer.TEMPLATES_LOCATION_PREFERENCE);
				try {
				    if (templatesLocation != null && templatesLocation.trim().length() > 0) {
				        TemplateUtils.setTemplateLocations(templatesLocation);
				    }
                    html = computeBrowserContents(browser.getSize());
                } finally {
                    TemplateUtils.setTemplateLocations();
                }
			}
			if (html == null) {
				html = "<h2>no plot data</h2>";
			}
			browser.setText(html);
		}
	}

	@Override
	public void setFocus() {
	}
}

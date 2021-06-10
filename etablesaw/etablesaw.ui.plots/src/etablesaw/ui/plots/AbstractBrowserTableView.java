package etablesaw.ui.plots;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;

import etablesaw.ui.Activator;
import etablesaw.ui.preferences.TablesawPreferenceInitializer;
import etablesaw.ui.util.Util;
import etablesaw.ui.views.AbstractTablesawView;
import tech.tablesaw.plotly.components.TemplateUtils;

public abstract class AbstractBrowserTableView extends AbstractTablesawView {

	public AbstractBrowserTableView() {
		super(false);
	}

	private Browser browser;
	private IAction openInternalBrowserAction, openExternalBrowserAction;

	@Override
	public void createTableDataControls(final Composite parent) {
	    super.createTableDataControls(parent);
		browser = new Browser(parent, SWT.NONE);
		browser.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		browser.setJavascriptEnabled(true);

		openInternalBrowserAction = new Action("Open internal browser", Util.imageFromPlugin("org.eclipse.ui.browser", "/icons/obj16/internal_browser.png")) {
			@Override
			public void run() {
				openBrowser(false);
			}
		};
		openExternalBrowserAction = new Action("Open internal browser", Util.imageFromPlugin("org.eclipse.ui.browser", "/icons/obj16/external_browser.png")) {
			@Override
			public void run() {
				openBrowser(true);
			}
		};
	}

	protected abstract String computeBrowserContents(Point size);
	
	protected void addActions() {
		super.addActions();
	    IToolBarManager toolBarManager = getViewSite().getActionBars().getToolBarManager();
        toolBarManager.add(openInternalBrowserAction);
        toolBarManager.add(openExternalBrowserAction);
    }

	@Override
	protected void updateTableControls() {
		if (browser != null && (! browser.isDisposed())) {
			browser.setText(computeHtml());
		}
	}

	private int browserStyle = IWorkbenchBrowserSupport.LOCATION_BAR | IWorkbenchBrowserSupport.NAVIGATION_BAR | IWorkbenchBrowserSupport.STATUS;
	
	protected void openBrowser(boolean external) {
		String html = computeHtml();
		try {
			IWorkbenchBrowserSupport browserSupport = PlatformUI.getWorkbench().getBrowserSupport();
			IWebBrowser extBrowser = (external ? browserSupport.getExternalBrowser() : browserSupport.createBrowser(browserStyle, toString(), getPartName(), getTitle()));
			Path tempFile = Files.createTempFile(getClass().getSimpleName(), ".html");
			Files.write(tempFile, html.getBytes());
			extBrowser.openURL(tempFile.toUri().toURL());
		} catch (PartInitException e) {
			// couldn't open browser
		} catch (IOException e) {
			// couldn't write to temp file
		}
	}

	private String computeHtml() {
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
		return html;
	}

	@Override
	public void setFocus() {
	}
}

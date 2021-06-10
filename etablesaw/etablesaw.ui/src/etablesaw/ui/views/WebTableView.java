package etablesaw.ui.views;

import java.io.BufferedInputStream;
import java.io.StringBufferInputStream;
import java.net.URL;

import etablesaw.io.FileFormatSupport;
import etablesaw.ui.Activator;
import etablesaw.ui.TableProvider;
import etablesaw.ui.editor.NatTablesawViewer;
import etablesaw.ui.util.Util;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.LocationEvent;
import org.eclipse.swt.browser.LocationListener;
import org.eclipse.swt.browser.ProgressEvent;
import org.eclipse.swt.browser.ProgressListener;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.part.ViewPart;
import tech.tablesaw.api.Table;
import tech.tablesaw.io.DataFrameReader;
import org.eclipse.ui.internal.browser.BrowserViewer;

public class WebTableView extends ViewPart implements LocationListener, ProgressListener, TableProvider {

    private StackLayout altLayout;
    private NatTablesawViewer natTablesawViewer;
    private Text exceptionText;
    private BrowserViewer browserViewer;
    private String sourceString;
    private Text sourceText;

    public void createPartControl(final Composite parent) {
        SashForm sashForm = new SashForm(parent, SWT.VERTICAL);
        Composite tableParent = new Composite(sashForm, SWT.NONE);
        altLayout = new StackLayout();
        tableParent.setLayout(altLayout);
        natTablesawViewer = new NatTablesawViewer();
        natTablesawViewer.createPartControl(tableParent);
        altLayout.topControl = natTablesawViewer.getControl();
        exceptionText = new Text(tableParent, SWT.MULTI | SWT.WRAP | SWT.V_SCROLL);

        TabFolder tabFolder = new TabFolder(sashForm, SWT.HORIZONTAL);
        
        browserViewer = new BrowserViewer(tabFolder, BrowserViewer.BUTTON_BAR | BrowserViewer.LOCATION_BAR);
        TabItem browserTab = new TabItem(tabFolder, SWT.NONE);
        browserTab.setText("Web");
        browserTab.setControl(browserViewer);
        
        sourceText = new Text(tabFolder, SWT.MULTI | SWT.WRAP | SWT.V_SCROLL);
        sourceText.setEditable(true);
        TabItem sourceTab = new TabItem(tabFolder, SWT.NONE);
        sourceTab.setText("Source");
        sourceTab.setControl(sourceText);

        tabFolder.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                switch (tabFolder.getSelectionIndex()) {
                case 0: {
                    updateSourceText();
                    break;
                }
                case 1: {
                    updateBrowserSource();
                    break;
                }
                }
            }
        });
        sashForm.setWeights(new int[] { 20, 80 });
        addActions();
        Activator.getInstance().getTableProviderRegistry().registerTableProvider(getPartName(), natTablesawViewer);
        browserViewer.getBrowser().addProgressListener(this);
    }

    //

    @Override
    public void dispose() {
        super.dispose();
        if (browserViewer != null && (! browserViewer.isDisposed())) {
            browserViewer.getBrowser().removeProgressListener(this);
            browserViewer.dispose();
            browserViewer = null;
        }
    }

    @Override
    public void setFocus() {
        browserViewer.setFocus();
    }

    protected void addActions() {
        IActionBars actionBars = getViewSite().getActionBars();
        IToolBarManager toolBarManager = actionBars.getToolBarManager();
        Action refreshAction = new Action("Refresh",
                Util.imageFromPlugin("org.eclipse.ui.ide", "/icons/full/elcl16/refresh_nav.png")) {
            @Override
            public void run() {
                updateTable();
            }
        };
        toolBarManager.add(refreshAction);
    }

    // location listener, notified when URL changes

    @Override
    public void changed(LocationEvent locationEvent) {
    }

    @Override
    public void changing(LocationEvent locationEvent) {
    }

    // progress listener, notified during/after loading page

    @Override
    public void changed(ProgressEvent progressEvent) {
//		updateEmfsResource(true);
    }

    @Override
    public void completed(ProgressEvent progressEvent) {
        updateSourceText();
        updateTable();
    }

    private final static String blankUrl = "about:blank";

    protected void updateSourceText() {
        if (! browserViewer.getURL().equals(blankUrl)) {
            sourceString = getBrowserSourceText();
            sourceText.setText(sourceString);
        }
    }

    protected String getBrowserSourceText() {
        return browserViewer.getBrowser().getText();
    }

    protected void updateBrowserSource() {
        String source = sourceText.getText();
        if (! source.equals(sourceString)) {
            browserViewer.setURL(blankUrl);
            browserViewer.getBrowser().setText(source);
        }
    }

    public void updateTable() {
        try {
            extractTable();
            altLayout.topControl = natTablesawViewer.getControl();
            altLayout.topControl.getParent().layout();
        } catch (Exception e) {
            exceptionText.setText(e.toString());
            altLayout.topControl = exceptionText;
            altLayout.topControl.getParent().layout();
        }
    }

    protected void extractTable() throws Exception {
        Table table = null;
        URL url = null;
        try {
            url = new URL(browserViewer.getURL());
        } catch (Exception e1) {
            return;
        }
        String path = url.getPath();
        int pos = path.lastIndexOf('.');
        String fileFormat = null;
        if (pos > 0) {
            fileFormat = path.substring(pos + 1);
        }
        try {
            if (fileFormat != null) {
                table = extractTable(path, getBrowserSourceText(), fileFormat);
            } else {
                table = new DataFrameReader(Table.defaultReaderRegistry).url(url);
            }
        } catch (Exception e) {
            try {
                table = extractTable(path, getBrowserSourceText(), "html");
            } catch (Exception e2) {
                throw e2;
            }
        }
        if (table != null) {
            natTablesawViewer.setInput(table);
        }
    }

    protected Table extractTable(String name, String source, String format) throws Exception {
        FileFormatSupport ffs = Activator.getInstance().getFileFormatSupport(format);
        Table table = null;
        if (ffs != null && (! Boolean.FALSE.equals(ffs.supportsFormat(format)))) {
            try {
                Table[] tables = ffs.read(name, () -> new BufferedInputStream(new StringBufferInputStream(source)));
                if (tables != null && tables.length > 0) {
                    table = tables[0];
                }
            } catch (Exception e) {
                try {
                    table = new DataFrameReader(Table.defaultReaderRegistry).string(source, format);
                } catch (Exception e2) {
                    throw e;
                }
            }
        }
        return table;
    }

    @Override
    public Table getTable() {
        return natTablesawViewer.getTable();
    }

    @Override
    public void addTableDataProviderListener(TableProvider.Listener listener) {
        natTablesawViewer.getTableProviderHelper().addTableDataProviderListener(listener);
    }

    @Override
    public void removeTableDataProviderListener(TableProvider.Listener listener) {
        natTablesawViewer.getTableProviderHelper().removeTableDataProviderListener(listener);
    }
}

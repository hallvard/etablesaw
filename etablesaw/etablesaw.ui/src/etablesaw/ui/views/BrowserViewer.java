package etablesaw.ui.views;

import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;

public class BrowserViewer extends Composite {

    private Text locationText;
    private Button goButton;
    private Browser browser;
    
    public BrowserViewer(Composite parent, int style) {
        super(parent, style);
        
        setLayout(new GridLayout(2, false));
        locationText = new Text(this, SWT.NONE);
        locationText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
        goButton = new Button(this, SWT.NONE);
        goButton.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, false, false));
        browser = new Browser(this, SWT.NONE);
        browser.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1));
        locationText.addSelectionListener(selectionListener);
        goButton.addSelectionListener(selectionListener);
    }
    
    private SelectionAdapter selectionListener = new SelectionAdapter() {
        @Override
        public void widgetSelected(SelectionEvent e) {
            go();
        }
    };

    private void go() {
        browser.setUrl(locationText.getText());
    }

    public Browser getBrowser() {
        return browser;
    }
    
    public String getURL() {
        return browser.getUrl();
    }

    public void setURL(String url) {
        browser.setUrl(url);
    }
}

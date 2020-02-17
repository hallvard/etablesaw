package etablesaw.ui.plots;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.FilteredResourcesSelectionDialog;

public class HtmlTemplateTableView extends AbstractTemplateBrowserTableView {

    private Text templatePathText;
    private Text templatePropertiesText;
    
    @Override
    protected void createConfigControls(Composite configParent) {
        super.createConfigControls(configParent);
        Label templatePathLabel = new Label(configParent, SWT.NONE);
        templatePathLabel.setText("Template: ");
        templatePathLabel.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
        templatePathText = new Text(configParent, SWT.BORDER);
        templatePathText.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.keyCode == '\r') {
                    e.doit = true;
                    if (templatePathText.getText().length() == 0) {
                        selectTemplate(true);                        
                    } else {
                        updateTableControls();
                    }
                }
            }
        });
        templatePathText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

        Label templatePropertiesLabel = new Label(configParent, SWT.NONE);
        templatePropertiesLabel.setText("Properties: ");
        templatePropertiesLabel.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
        templatePropertiesText = new Text(configParent, SWT.BORDER);
        templatePropertiesText.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.keyCode == '\r') {
                    e.doit = true;
                    updateTableControls();
                }
            }
        });
        templatePropertiesText.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
    }

    protected IPath selectTemplate(boolean updateTable) {
        FilteredResourcesSelectionDialog dialog = new FilteredResourcesSelectionDialog(getTableViewerParent().getShell(), false, ResourcesPlugin.getWorkspace().getRoot(), IResource.FILE);
        if (dialog.open() == Window.OK) {
            Object[] selection = dialog.getResult();
            if (selection != null && selection.length >= 1 && selection[0] instanceof IResource) {
                IPath path = ((IResource) selection[0]).getFullPath();
                if (updateTable) {
                    templatePathText.setText(path.toString());
                    updateTableControls();
                }
                return path;
            }
        }
        return null;
    }

    @Override
    protected String getTemplateResource() {
        return templatePathText.getText();
    }

    @Override
    protected Map<String, Object> getTemplateProperties() {
        Map<String, Object> templateProperties = new HashMap<>();
        String[] xtraProperties = templatePropertiesText.getText().split("[,;]\\s*");
        for (int i = 0; i < xtraProperties.length; i++) {
            String xtraProperty = xtraProperties[i].trim();
            if (xtraProperty.length() > 0) {
                int pos = xtraProperty.indexOf(':');
                if (pos < 0) {
                    pos = xtraProperty.indexOf('=');
                }
                if (pos < 0) {
                    templateProperties.put(xtraProperty, true);
                } else {
                    templateProperties.put(xtraProperty.substring(0, pos), xtraProperty.substring(pos + 1).trim());
                }
            }
        }
        return templateProperties;
    }
}

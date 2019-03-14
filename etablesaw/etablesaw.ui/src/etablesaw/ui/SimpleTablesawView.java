package etablesaw.ui;

import org.eclipse.nebula.widgets.nattable.NatTable;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;

import etablesaw.ui.editor.NatTablesawViewer;
import tech.tablesaw.api.Table;

public class SimpleTablesawView extends AbstractTablesawView {

    private final String[] tableNames;
    
	public SimpleTablesawView(boolean autoSelectTableDataProvider, String... tableNames) {
        super(autoSelectTableDataProvider);
        this.tableNames = tableNames;
    }


	public SimpleTablesawView(String... tableNames) {
		this(false, tableNames);
	}

	@Override
	protected void createConfigControls(final Composite configParent) {
		createTableRegistrySelector("Source: ", configParent, null);
	}
	
	private TabFolder tabFolder = null; 
	protected NatTablesawViewer[] natTablesawViewers;

	@Override
	protected void createTableDataControls(final Composite parent) {
	    natTablesawViewers = new NatTablesawViewer[tableNames.length];
	    GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        if (tableNames.length > 1) {
	        tabFolder = new TabFolder(parent, SWT.HORIZONTAL);
	        super.createTableDataControls(tabFolder);
            tabFolder.setLayoutData(layoutData);
            tabFolder.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    selectedTableViewerChanged();
                }
            });
	    } else {
	        super.createTableDataControls(parent);
		}
        for (int i = 0; i < natTablesawViewers.length; i++) {
            NatTablesawViewer viewer = new NatTablesawViewer();
            natTablesawViewers[i] = viewer;
            viewer.createPartControl(getTableViewerParent());            
            NatTable control = natTablesawViewers[i].getControl();
            if (tabFolder != null) {
                TabItem item = new TabItem(tabFolder, SWT.NONE);
                item.setText(tableNames[i]);
                item.setControl(control);
            } else {
                control.setLayoutData(layoutData);                
            }
        }
	}

    protected int getSelectedTableViewer() {
	    return (tabFolder != null ? tabFolder.getSelectionIndex() : 0);
	}
    protected void selectedTableViewerChanged() {
    }
	
	protected Table getTableViewerInput(int n) {
		return getViewTable();
	}

	@Override
	protected void updateTableControls() {
        for (int i = 0; i < natTablesawViewers.length; i++) {
            natTablesawViewers[i].setInput(getTableViewerInput(i));
        }
	}
}

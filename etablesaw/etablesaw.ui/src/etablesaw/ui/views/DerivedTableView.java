package etablesaw.ui.views;

import etablesaw.ui.TableProvider;
import tech.tablesaw.api.Table;

public abstract class DerivedTableView extends SimpleTablesawView implements TableProvider {

    protected Table[] derivedTables;
    
	public DerivedTableView(String... tableNames) {
        super(tableNames);
        derivedTables = new Table[tableNames.length];
    }

	@Override
	protected Table getTableViewerInput(int n) {
		return derivedTables[n];
	}

//	@Override
//	protected void createConfigControls(final Composite configParent) {
//		createTableRegistrySelector("Source: ", configParent, this);
//	}

	//

	protected Table getTable(int n) {
		return natTablesawViewers[n].getTable();
	}
	@Override
	public Table getTable() {
	    return getTable(getSelectedTableViewer());
	}

    protected void selectedTableViewerChanged() {
        fireTableChanged();
    }

	@Override
	public void addTableDataProviderListener(final TableProvider.Listener listener) {
	    for (int i = 0; i < natTablesawViewers.length; i++) {
	        natTablesawViewers[i].addTableDataProviderListener(listener);
	    }
	}

	@Override
	public void removeTableDataProviderListener(final TableProvider.Listener listener) {
	    for (int i = 0; i < natTablesawViewers.length; i++) {
	        natTablesawViewers[i].removeTableDataProviderListener(listener);
	    }
	}

	protected void fireTableChanged(boolean onlyTableDataChanged) {
        if (onlyTableDataChanged) {
            fireTableDataChanged();
        } else {
            fireTableChanged();         
        }
	}

	protected void fireTableChanged() {
	    for (int i = 0; i < natTablesawViewers.length; i++) {
	        natTablesawViewers[i].getTableProviderHelper().fireTableChanged(DerivedTableView.this);
	    }
	}

	protected void fireTableDataChanged() {
	    for (int i = 0; i < natTablesawViewers.length; i++) {
	        natTablesawViewers[i].getTableProviderHelper().fireTableDataChanged(DerivedTableView.this);
	    }
	}
}

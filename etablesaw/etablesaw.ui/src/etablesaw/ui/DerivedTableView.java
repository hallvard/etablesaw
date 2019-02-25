package etablesaw.ui;

import org.eclipse.swt.widgets.Composite;

import tech.tablesaw.api.Table;

public abstract class DerivedTableView extends SimpleTablesawView implements TableProvider {

	protected Table derivedTable = null;

	@Override
	protected Table getTableViewerInput() {
		return derivedTable;
	}

	@Override
	protected void createConfigControls(final Composite configParent) {
		createTableRegistrySelector("Source: ", configParent, this);
	}

	//

	@Override
	public Table getTable() {
		return natTablesawViewer.getTable();
	}

	@Override
	public void addTableDataProviderListener(final TableProvider.Listener listener) {
		natTablesawViewer.addTableDataProviderListener(listener);
	}

	@Override
	public void removeTableDataProviderListener(final TableProvider.Listener listener) {
		natTablesawViewer.removeTableDataProviderListener(listener);
	}

	protected void fireTableChanged(final boolean async) {
		if (async) {
			getTableViewerParent().getDisplay().asyncExec(new Runnable() {
				@Override
				public void run() {
					natTablesawViewer.getTableProviderHelper().fireTableChanged(DerivedTableView.this);
				}
			});
		} else {
			natTablesawViewer.getTableProviderHelper().fireTableChanged(DerivedTableView.this);
		}
	}

	protected void fireTableDataChanged(final boolean async) {
		if (async) {
			getTableViewerParent().getDisplay().asyncExec(new Runnable() {
				@Override
				public void run() {
					natTablesawViewer.getTableProviderHelper().fireTableDataChanged(DerivedTableView.this);
				}
			});
		} else {
			natTablesawViewer.getTableProviderHelper().fireTableDataChanged(DerivedTableView.this);
		}
	}
}

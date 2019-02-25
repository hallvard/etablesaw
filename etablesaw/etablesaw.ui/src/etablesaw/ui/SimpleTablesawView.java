package etablesaw.ui;

import org.eclipse.swt.widgets.Composite;

import etablesaw.ui.nattable.NatTablesawViewer;
import tech.tablesaw.api.Table;

public class SimpleTablesawView extends AbstractTablesawView {

	//	private TableViewer tablesawViewer;
	protected NatTablesawViewer natTablesawViewer;

	public SimpleTablesawView() {
		super(false);
	}

	@Override
	protected void createConfigControls(final Composite configParent) {
		createTableRegistrySelector("Source: ", configParent, null);
		//		createWorkbenchTableProvideSelector("Source: ", configParent);
	}

	@Override
	protected void createTableDataControls(final Composite parent) {
		super.createTableDataControls(parent);
		natTablesawViewer = new NatTablesawViewer();
		natTablesawViewer.createPartControl(parent);
		//		natTablesawViewer.setInput(getTableViewerInput());
		//		if (table != null) {
		//			tablesawViewer = new TableViewer(parent, SWT.VIRTUAL | SWT.V_SCROLL);
		//			tablesawViewer.setContentProvider(new AbstractTablesawContentProvider() {
		//				@Override
		//				protected boolean isContentTable(final Table table) {
		//					return true;
		//				}
		//			});
		//			tablesawViewer.getTable().setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		//			tablesawViewer.getTable().setHeaderVisible(true);
		//
		//			addTableColumn(null);
		//			for (int colNum = 0; colNum < table.columnCount(); colNum++) {
		//				final Column<?> col = table.column(colNum);
		//				addTableColumn(col);
		//			}
		//			tablesawViewer.setInput(table);
		//
		//			parent.getDisplay().asyncExec(new Runnable() {
		//				@Override
		//				public void run() {
		//					final org.eclipse.swt.widgets.Table swtTable = tablesawViewer.getTable();
		//					if (swtTable != null && (! swtTable.isDisposed())) {
		//						for (final TableColumn column : swtTable.getColumns()) {
		//							column.pack();
		//						}
		//					}
		//				}
		//			});
		//		}
	}

	protected Table getTableViewerInput() {
		return getViewTable();
	}

	@Override
	protected void updateTableControls() {
		//		if (tablesawViewer != null) {
		//			tablesawViewer.getTable().dispose();
		//			tablesawViewer = null;
		//		}
		//		createTableViewer(getTableViewerParent());
		//		getTableViewerParent().layout(true);
		natTablesawViewer.setInput(getTableViewerInput());
	}
	//
	//	protected void addTableColumn(final Column<?> column) {
	//		final TableViewerColumn viewerColumn = new TableViewerColumn(tablesawViewer, SWT.NONE);
	//		final AbstractTablesawColumnLabelProvider labelProvider = new AbstractTablesawColumnLabelProvider() {
	//			@Override
	//			protected String getColumnName() {
	//				return (column != null ? column.name() : null);
	//			}
	//			@Override
	//			protected String getColumnTitle() {
	//				final String name = getColumnName();
	//				return (name != null ? name : "#");
	//			}
	//		};
	//		viewerColumn.setLabelProvider(labelProvider);
	//		viewerColumn.getColumn().setText(labelProvider.getColumnTitle());
	//		viewerColumn.getColumn().setWidth(labelProvider.getColumnTitle().length() * 10);
	//	}
}

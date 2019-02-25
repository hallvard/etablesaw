package etablesaw.ui.nattable;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.nebula.widgets.nattable.data.IDataProvider;

import tech.tablesaw.api.ColumnType;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;
import tech.tablesaw.selection.Selection;

public class TablesawDataProvider implements IDataProvider, ColumnTypeProvider {

	private Table table;
	private Table filtered = null;
	// null = normal, TRUE = column header, FALSE = row header
	private final Boolean mode;

	public TablesawDataProvider(final Table table) {
		this(table, null);
	}

	public TablesawDataProvider(final Table table, final Boolean mode) {
		super();
		setTable(table);
		this.mode = mode;
	}

	public Table getTable() {
		return table;
	}

	public void setTable(final Table table) {
		this.table = table;
		filtered = null;
	}

	public boolean isFiltered() {
		return filtered != null;
	}

	public void applyFilter(final Selection selection) {
		filtered = (selection != null ? table.where(selection) : null);
		fireRowsChanged(-1, -1);
	}

	protected Table getDataTable() {
		return filtered != null ? filtered : table;
	}

	@Override
	public ColumnType getColumnType(final int columnIndex) {
		return (table != null ? getDataTable().column(columnIndex).type() : null);
	}

	@Override
	public Object getDataValue(final int columnIndex, final int rowIndex) {
		if (table != null) {
			final Column<?> column = getDataTable().column(columnIndex);
			return  (mode == null ? column.get(rowIndex) : (mode ? column.name() : rowIndex));
		}
		return null;
	}

	@Override
	public void setDataValue(final int columnIndex, final int rowIndex, final Object newValue) {
		if (table != null) {
			final Column<Object> column = (Column<Object>) getDataTable().column(columnIndex);
			if (mode == null) {
				final Object oldValue = column.get(rowIndex);
				column.set(rowIndex, newValue);
				if (oldValue != newValue && (oldValue == null || (! oldValue.equals(newValue)))) {
					fireCellChanged(rowIndex, columnIndex, oldValue, newValue);
				}
			} else if (mode) {
				column.setName(String.valueOf(newValue));
			}
		}
	}

	@Override
	public int getColumnCount() {
		return (table != null ? (Boolean.FALSE.equals(mode) ? 1 : getDataTable().columnCount()) : 0);
	}

	@Override
	public int getRowCount() {
		return (table != null ? (Boolean.TRUE.equals(mode) ? 1 : getDataTable().rowCount()) : 0);
	}

	//

	public static interface Listener {
		public void rowsChanged(int startRow, int endRow);
		public void cellChanged(int row, int column, Object oldValue, Object newValue);
	}

	private Collection<Listener> listeners = null;

	public void addTableChangeListener(final Listener listener) {
		if (listeners == null) {
			listeners = new ArrayList<TablesawDataProvider.Listener>();
		}
		listeners.add(listener);
	}

	public void removeTableChangeListener(final Listener listener) {
		if (listeners != null) {
			listeners.remove(listener);
		}
	}

	protected void fireRowsChanged(final int startRow, final int endRow) {
		for (final Listener listener : listeners) {
			listener.rowsChanged(startRow, endRow);
		}
	}

	protected void fireCellChanged(final int row, final int column, final Object oldValue, final Object newValue) {
		for (final Listener listener : listeners) {
			listener.cellChanged(row, column, oldValue, newValue);
		}
	}
}

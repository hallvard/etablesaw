package etablesaw.ui.editor;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.eclipse.nebula.widgets.nattable.data.IDataProvider;

import tech.tablesaw.api.ColumnType;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.AbstractColumnParser;
import tech.tablesaw.columns.Column;
import tech.tablesaw.columns.numbers.IntColumnType;
import tech.tablesaw.io.ReadOptions;
import tech.tablesaw.io.csv.CsvReadOptions;
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
        fireProviderRowsChanged(-1, -1);
    }

    protected Table getDataTable() {
        return filtered != null ? filtered : table;
    }

    // column filter
    
    private List<String> columnNames = null;

    public boolean hasColumn(String columnName) {
        return getColumnNames().contains(columnName);
    }

    public Collection<String> getColumnNames() {
        return columnNames != null ? columnNames : getDataTable().columnNames();
    }

    public void clearColumnNames() {
        columnNames = null;
        fireProviderRowsChanged(-1, -1);
    }
    
    public void setColumnNames(Collection<String> columnNames) {
        this.columnNames = new ArrayList<>(columnNames);
        fireProviderRowsChanged(-1, -1);
    }

    public void setColumnNames(String... columnNames) {
        this.columnNames = Arrays.asList(columnNames);
        fireProviderRowsChanged(-1, -1);
    }
    
    public Column<?> getColumn(int columnIndex) {
        if (columnNames != null) {
            return getDataTable().column(columnNames.get(columnIndex));
        }
        return getDataTable().column(columnIndex);
    }

    @Override
    public int getColumnCount() {
        return (table != null ? (Boolean.FALSE.equals(mode) ? 1 : (columnNames != null ? columnNames.size() : getDataTable().columnCount())) : 0);
    }

    //
    
    @Override
    public int getRowCount() {
        return (table != null ? (Boolean.TRUE.equals(mode) ? 1 : getDataTable().rowCount()) : 0);
    }

    @Override
    public ColumnType getColumnType(final int columnIndex) {
        return (table != null ? (Boolean.FALSE.equals(mode) ? IntColumnType.instance() : getColumn(columnIndex).type())
                : null);
    }

    @Override
    public Object getDataValue(final int columnIndex, final int rowIndex) {
        if (table != null) {
            final Column<?> column = getColumn(columnIndex);
            return (mode == null ? column.get(rowIndex) : (mode ? column.name() : rowIndex + 1));
        }
        return null;
    }
    
    private ReadOptions options = CsvReadOptions.builderFromString("").build();

    @Override
    public void setDataValue(final int columnIndex, final int rowIndex, Object newValue) {
        if (table != null) {
            final Column<Object> column = (Column<Object>) getColumn(columnIndex);
            if (mode == null && rowIndex >= 0) {
                final Object oldValue = (column.isMissing(rowIndex) ? null : column.get(rowIndex));
                try {
                    if (isMissingValue(column, newValue)) {
                        column.setMissing(rowIndex);
                        newValue = null;
                    } else {
                        column.set(rowIndex, newValue);
                    }
                } catch (Exception e) {
                    newValue = setDataValueUsingParser(column, rowIndex, String.valueOf(newValue));
                }
                if (oldValue != newValue && (oldValue == null || (! oldValue.equals(newValue)))) {
                    fireCellChanged(rowIndex, columnIndex, oldValue, newValue);
                }
            } else if (mode || rowIndex == -1) {
                // pretend row -1 is column name
                String colName = String.valueOf(newValue);
                for (Column<?> other : table.columns()) {
                    if (colName.equals(other.name())) {
                        return;
                    }
                }
                String oldName = column.name();
                column.setName(colName);
                fireCellChanged(-1, columnIndex, oldName, colName);
            }
        }
    }

    public boolean isMissingValue(Column<Object> column, Object newValue) {
        return newValue == null || newValue instanceof String && ((String) newValue).trim().length() == 0;
    }

    protected Object setDataValueUsingParser(final Column<Object> column, final int rowIndex, String s) {
        AbstractColumnParser<?> parser = column.type().customParser(options);
        if (parser.canParse(s)) {
            Object altValue = parser.parse(s);
            try {
                column.set(rowIndex, altValue);
                return altValue;
            } catch (Exception e1) {
                column.setMissing(rowIndex);
                return null;
            }
        } else {
            column.setMissing(rowIndex);
            return null;
        }
    }

    //

    public static interface Listener {
        public void providerRowsChanged(int startRow, int endRow);
        public void tableCellChanged(int row, int column, Object oldValue, Object newValue);
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

    protected void fireProviderRowsChanged(final int startRow, final int endRow) {
        if (listeners != null) {
            // avoid ConcurrentModificationException
            for (final Listener listener : new ArrayList<>(listeners)) {
                listener.providerRowsChanged(startRow, endRow);
            }
        }
    }

    protected void fireCellChanged(final int row, final int column, final Object oldValue, final Object newValue) {
        if (listeners != null) {
            for (final Listener listener : listeners) {
                listener.tableCellChanged(row, column, oldValue, newValue);
            }
        }
    }
}

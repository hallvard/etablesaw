package etablesaw.ui.editor;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.eclipse.nebula.widgets.nattable.data.IDataProvider;
import org.eclipse.nebula.widgets.nattable.sort.ISortModel;
import org.eclipse.nebula.widgets.nattable.sort.SortDirectionEnum;
import tech.tablesaw.api.ColumnType;
import tech.tablesaw.api.Row;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.AbstractColumnParser;
import tech.tablesaw.columns.Column;
import tech.tablesaw.columns.numbers.IntColumnType;
import tech.tablesaw.io.ReadOptions;
import tech.tablesaw.io.csv.CsvReadOptions;
import tech.tablesaw.selection.Selection;

public class TablesawDataProvider extends AbstractTablesawDataProvider implements IDataProvider, ColumnTypeProvider, ISortModel, Comparator<Row> {

    private Table table;
    private Selection selection = null;
    private Table tableView = null;
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
        tableView = null;
    }

    public boolean isSorted() {
        return ! sortedColumnIndexesWithOrder.isEmpty();
    }

    public boolean isRowFiltered() {
        return selection != null;
    }

    public void applyRowFilter(final Selection selection) {
        this.selection = selection;
        updateTableView();
    }

    private void updateTableView() {
        if (isSorted() || isRowFiltered()) {
            Selection viewSelection = isRowFiltered() ? selection : Selection.withRange(0, getRowCount());
            tableView = table.where(viewSelection);
            if (isSorted()) {
                System.out.println(tableView);
                tableView = tableView.sortOn(this);
                System.out.println(sortedColumnIndexesWithOrder);
                System.out.println(tableView);
            }
        } else if (tableView != null) {
            tableView.clear();
            tableView = null;
        }
        fireProviderRowsChanged(-1, -1);
    }

    protected Table getDataTable() {
        return tableView != null ? tableView : table;
    }

    // column filter
    
    private List<String> columnFilter = null;

    public boolean hasColumn(String columnName) {
        return getColumnNames().contains(columnName);
    }

    public Collection<String> getColumnNames() {
        return columnFilter != null ? columnFilter : getDataTable().columnNames();
    }

    public Collection<String> getColumnNames(Iterable<Integer> indices) {
        List<String> allColumnNames = new ArrayList<String>(getColumnNames());
        List<String> columnNames = new ArrayList<String>();
        for (int index : indices) {
            columnNames.add(allColumnNames.get(index));
        }
        return columnNames;
    }

    public boolean isColumnFiltered() {
        return columnFilter != null;
    }

    public void clearColumnFilter() {
        columnFilter = null;
        fireProviderRowsChanged(-1, -1);
    }
    
    public void setColumnNames(Collection<String> columnNames) {
        this.columnFilter = new ArrayList<>(columnNames);
        fireProviderRowsChanged(-1, -1);
    }

    public void setColumnNames(String... columnNames) {
        this.columnFilter = Arrays.asList(columnNames);
        fireProviderRowsChanged(-1, -1);
    }

    public void addColumnNames(String... columnNames) {
        List<String> newColumnNames = new ArrayList<>(getColumnNames());
        for (int i = 0; i < columnNames.length; i++) {
            if (! newColumnNames.contains(columnNames[i])) {
                newColumnNames.add(columnNames[i]);
            }
        }
        this.columnFilter = newColumnNames;
        fireProviderRowsChanged(-1, -1);
    }

    public void removeColumnNames(String... columnNames) {
        if (isColumnFiltered()) {
            List<String> newColumnNames = new ArrayList<>(getColumnNames());
            newColumnNames.removeAll(Arrays.asList(columnNames));
            this.columnFilter = newColumnNames;
            fireProviderRowsChanged(-1, -1);
        }
    }
    
    public Column<?> getColumn(int columnIndex) {
        if (columnFilter != null) {
            return getDataTable().column(columnFilter.get(columnIndex));
        }
        return getDataTable().column(columnIndex);
    }

    int getColumnCount(final Boolean mode) {
        return (table != null ? (Boolean.FALSE.equals(mode) ? 1 : (columnFilter != null ? columnFilter.size() : getDataTable().columnCount())) : 0);
    }
    @Override
    public int getColumnCount() {
        return getColumnCount(mode);
    }

    //
    
    int getRowCount(final Boolean mode) {
        return (table != null ? (Boolean.TRUE.equals(mode) ? 1 : getDataTable().rowCount()) : 0);
    }
    @Override
    public int getRowCount() {
        return getRowCount(mode);
    }

    ColumnType getColumnType(final Boolean mode, final int columnIndex) {
        return (table != null ? (Boolean.FALSE.equals(mode) ? IntColumnType.instance() : getColumn(columnIndex).type())
                : null);
    }
    @Override
    public ColumnType getColumnType(final int columnIndex) {
        return getColumnType(mode, columnIndex);
    }

    Object getDataValue(final Boolean mode, final int columnIndex, final int rowIndex) {
        if (table != null) {
            final Column<?> column = getColumn(columnIndex);
            return (mode == null ? getColumnDataValue(column, rowIndex) : (mode ? getColumnHeaderDataValue(column) : getRowHeaderDataValue(rowIndex)));
        }
        return null;
    }

    private String columnHeaderDataValueFormat = "%s (%s)";
    
	protected String getColumnHeaderDataValue(final Column<?> column) {
		return String.format(columnHeaderDataValueFormat, column.name(), column.type());
	}

	protected int getRowHeaderDataValue(final int rowIndex) {
		return rowIndex + 1;
	}
	
	protected Object getColumnDataValue(final Column<?> column, final int rowIndex) {
		return column.get(rowIndex);
	}

    @Override
    public Object getDataValue(final int columnIndex, final int rowIndex) {
        return getDataValue(mode, columnIndex, rowIndex);
    }
    
    private ReadOptions options = CsvReadOptions.builderFromString("").build();

    void setDataValue(final Boolean mode, final int columnIndex, final int rowIndex, Object newValue) {
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
                if (! oldName.equals(colName)) {
                    column.setName(colName);
                    if (isColumnFiltered()) {
                        removeColumnNames(oldName);
                        addColumnNames(colName);
                    }
                    fireCellChanged(-1, columnIndex, oldName, colName);
                }
            }
        }
    }
    @Override
    public void setDataValue(final int columnIndex, final int rowIndex, Object newValue) {
        setDataValue(mode, columnIndex, rowIndex, newValue);
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

    // listeners

    public static interface Listener {
        public void providerRowsChanged(int startRow, int endRow);
        public void tableCellChanged(int row, int column, Object oldValue, Object newValue);
    }

    private Collection<Listener> listeners = null;

    @Override
    public void addTableChangeListener(final Listener listener) {
        if (listeners == null) {
            listeners = new ArrayList<TablesawDataProvider.Listener>();
        }
        listeners.add(listener);
    }

    @Override
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

    // sorting
    
    private List<Integer> sortedColumnIndexesWithOrder = new ArrayList<>();
    private List<Integer> sortedColumnIndexes = new ArrayList<>();

    @Override
    public List<Integer> getSortedColumnIndexes() {
        return sortedColumnIndexes;
    }

    @Override
    public int compare(Row row1, Row row2) {
        for (int colNum : getSortedColumnIndexes()) {
            Object o1 = row1.getObject(colNum);
            Object o2 = row2.getObject(colNum);
            int diff = 0;
            if (o1 != null && o2 != null) {
                Comparator comparator = getColumnComparator(colNum);
                diff = comparator.compare(o1, o2);
            } else {
                diff = (o1 == null ? -1 : 0) + (o2 == null ? 1 : 0);
            }
            if (diff != 0) {
                return getSortDirection(colNum) == SortDirectionEnum.DESC ? -diff : diff;
            }
        }
        return 0;
    }

    private int columnIndexPos(int columnIndex) {
        int index = getRowHeaderDataValue(columnIndex);
        for (int i = 0; i < sortedColumnIndexesWithOrder.size(); i++) {
            int sortedColumnIndex = sortedColumnIndexesWithOrder.get(i);
            if (sortedColumnIndex == index || sortedColumnIndex == -index) {
                return i;
            }
        }
        return -1;
    }

    @Override
    public boolean isColumnIndexSorted(int columnIndex) {
        return columnIndexPos(columnIndex) >= 0;
    }

    @Override
    public SortDirectionEnum getSortDirection(int columnIndex) {
        int pos = columnIndexPos(columnIndex);
        if (pos < 0) {
            return SortDirectionEnum.NONE;
        } else if (sortedColumnIndexesWithOrder.get(pos) < 0) {
            return SortDirectionEnum.DESC;            
        } else {
            return SortDirectionEnum.ASC;            
        }
    }

    @Override
    public int getSortOrder(int columnIndex) {
        return columnIndexPos(columnIndex);
    }

    @Override
    public List<Comparator> getComparatorsForColumnIndex(int columnIndex) {
        return Collections.singletonList(getColumnComparator(columnIndex));
    }

    @Override
    public Comparator<?> getColumnComparator(int columnIndex) {
        return getColumn(columnIndex);
    }

    @Override
    public void sort(int columnIndex, SortDirectionEnum sortDirection, boolean accumulate) {
        if (! accumulate) {
            clear();
        }
        if (sortDirection == SortDirectionEnum.NONE) {
            int pos = columnIndexPos(columnIndex);
            if (pos >= 0) {
                sortedColumnIndexesWithOrder.remove(pos);
            } else {
                return;
            }
        } else {
            int index = getRowHeaderDataValue(columnIndex);
            sortedColumnIndexesWithOrder.add(sortDirection == SortDirectionEnum.DESC ? -index : index);
        }
        sortedColumnIndexes.clear();
        for (int index : sortedColumnIndexesWithOrder) {
            sortedColumnIndexes.add((index < 0 ? -index : index) - 1);
        }
        updateTableView();
    }

    @Override
    public void clear() {
        sortedColumnIndexesWithOrder.clear();
        sortedColumnIndexes.clear();
    }
}

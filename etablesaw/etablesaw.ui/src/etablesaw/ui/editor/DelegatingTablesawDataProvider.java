package etablesaw.ui.editor;

import org.eclipse.nebula.widgets.nattable.data.IDataProvider;

import tech.tablesaw.api.ColumnType;

public class DelegatingTablesawDataProvider extends AbstractTablesawDataProvider implements IDataProvider, ColumnTypeProvider {

    private final TablesawDataProvider delegate;
    // null = normal, TRUE = column header, FALSE = row header
    private final Boolean mode;

    public DelegatingTablesawDataProvider(final TablesawDataProvider delegate) {
        this(delegate, null);
    }

    public DelegatingTablesawDataProvider(final TablesawDataProvider delegate, final Boolean mode) {
        super();
        this.delegate = delegate;
        this.mode = mode;
    }

    @Override
    public int getColumnCount() {
        return delegate.getColumnCount(mode);
    }

    //
    
    @Override
    public int getRowCount() {
        return delegate.getRowCount(mode);
    }

    @Override
    public ColumnType getColumnType(final int columnIndex) {
        return delegate.getColumnType(mode, columnIndex);
    }

    @Override
    public Object getDataValue(final int columnIndex, final int rowIndex) {
        return delegate.getDataValue(mode, columnIndex, rowIndex);
    }

    @Override
    public void setDataValue(final int columnIndex, final int rowIndex, Object newValue) {
        delegate.setDataValue(mode, columnIndex, rowIndex, newValue);
    }

    //

    @Override
    public void addTableChangeListener(final TablesawDataProvider.Listener listener) {
        delegate.addTableChangeListener(listener);
    }

    @Override
    public void removeTableChangeListener(final TablesawDataProvider.Listener listener) {
        delegate.removeTableChangeListener(listener);
    }
}

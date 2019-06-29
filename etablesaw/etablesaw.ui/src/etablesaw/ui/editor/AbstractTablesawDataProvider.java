package etablesaw.ui.editor;

import org.eclipse.nebula.widgets.nattable.data.IDataProvider;

public abstract class AbstractTablesawDataProvider implements IDataProvider, ColumnTypeProvider {

    public abstract void addTableChangeListener(final TablesawDataProvider.Listener listener);
    public abstract void removeTableChangeListener(final TablesawDataProvider.Listener listener);
}

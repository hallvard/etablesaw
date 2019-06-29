package etablesaw.ui.editor.commands;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import etablesaw.ui.editor.NatTablesawEditor;
import etablesaw.ui.editor.TablesawDataProvider;
import tech.tablesaw.api.ColumnType;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;

public class AddColumnOperation extends AbstractNatTablesawEditorTableOperation {

    private final ColumnType colType;
    
    public AddColumnOperation(NatTablesawEditor natTablesawEditor, ColumnType colType) {
        super("Add " + colType.name() + " Column", natTablesawEditor);
        this.colType = colType;
    }

    private String columnName = null;
    
    public String getColumnName() {
        return columnName;
    }
    
    @Override
    public IStatus execute(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
        Table table = getNatTablesawEditor().getModelTable();
        Table newTable = Table.create(table.name(), table.columnArray());
        if (columnName == null) {
            String colName = colType.name();
            for (int i = 1; colNameExists(newTable, colName); i++) {
                colName = colType.name() + i;
            }
            columnName = colName;
        }
        Column<?> column = colType.create(columnName);
        for (int count = newTable.rowCount(); count > 0; count--) {
            column.appendMissing();
        }
        newTable.addColumns(column);
        replaceEditorTable(newTable);
        selectColumns(newTable.columnIndex(column));

        TablesawDataProvider tablesawDataProvider = getTablesawDataProvider();
        if (tablesawDataProvider.isColumnFiltered()) {
            tablesawDataProvider.addColumnNames(columnName);
        }
        return Status.OK_STATUS;
    }

    @Override
    public IStatus undo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
        TablesawDataProvider tablesawDataProvider = getTablesawDataProvider();
        if (tablesawDataProvider.isColumnFiltered()) {
            tablesawDataProvider.removeColumnNames(columnName);
        }
        return super.undo(monitor, info);        
    }
    
    @Override
    public IStatus redo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
        TablesawDataProvider tablesawDataProvider = getTablesawDataProvider();
        if (tablesawDataProvider.isColumnFiltered()) {
            tablesawDataProvider.addColumnNames(columnName);
        }
        return super.redo(monitor, info);
    }
    
    private boolean colNameExists(Table table, String colName) {
        for (Column<?> col : table.columns()) {
            if (colName.equals(col.name())) {
                return true;
            }
        }
        return false;
    }
}

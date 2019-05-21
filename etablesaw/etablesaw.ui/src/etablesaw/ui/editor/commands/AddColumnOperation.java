package etablesaw.ui.editor.commands;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import etablesaw.ui.editor.NatTablesawEditor;
import tech.tablesaw.api.ColumnType;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;

public class AddColumnOperation extends AbstractNatTablesawEditorTableOperation {

    private final ColumnType colType;
    
    public AddColumnOperation(NatTablesawEditor natTablesawEditor, ColumnType colType) {
        super("Add " + colType.name() + " Column", natTablesawEditor);
        this.colType = colType;
    }

    @Override
    public IStatus execute(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
        Table table = getNatTablesawEditor().getModelTable();
        Table newTable = Table.create(table.name(), table.columnArray());
        String colName = colType.name();
        for (int i = 1; colNameExists(newTable, colName); i++) {
            colName = colType.name() + i;
        }
        Column<?> column = colType.create(colName);
        for (int count = newTable.rowCount(); count > 0; count--) {
            column.appendMissing();
        }
        newTable.addColumns(column);
        replaceEditorTable(newTable);
        selectColumns(newTable.columnIndex(column));
        return Status.OK_STATUS;
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

package etablesaw.ui.editor.commands;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import etablesaw.ui.editor.NatTablesawEditor;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;

public class DeleteColumnsOperation extends AbstractNatTablesawEditorOperation {

    private final int[] columnNums;
    
    public DeleteColumnsOperation(NatTablesawEditor natTablesawEditor, int... columnNums) {
        super("Delete columns", natTablesawEditor);
        this.columnNums = columnNums;
    }

    @Override
    public IStatus execute(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
        Table table = getNatTablesawEditor().getModelTable();
        Collection<Column<?>> columns = new ArrayList<Column<?>>(table.columns());
        columns.removeAll(table.columns(columnNums));
        Table newTable = table.select(columns.toArray(new Column<?>[columns.size()]));
        replaceEditorTable(newTable);
        return Status.OK_STATUS;
    }
    
    @Override
    public IStatus undo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
        IStatus status = super.undo(monitor, info);
        if (status.isOK()) {
            selectColumns(columnNums);
        }
        return status;
    }
}

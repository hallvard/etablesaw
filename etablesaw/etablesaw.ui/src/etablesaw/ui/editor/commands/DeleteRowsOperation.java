package etablesaw.ui.editor.commands;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import etablesaw.ui.editor.NatTablesawEditor;
import tech.tablesaw.api.Table;

public class DeleteRowsOperation extends AbstractNatTablesawEditorOperation {

    private final int[] rowNums;
    
    public DeleteRowsOperation(NatTablesawEditor natTablesawEditor, int... rowNums) {
        super("Delete Rows", natTablesawEditor);
        this.rowNums = rowNums;
    }

    @Override
    public IStatus execute(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
        Table newTable = getNatTablesawEditor().getModelTable().dropRows(rowNums);
        replaceEditorTable(newTable);
        return Status.OK_STATUS;
    }
    
    @Override
    public IStatus undo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
        IStatus status = super.undo(monitor, info);
        if (status.isOK()) {
            selectRows(rowNums);
        }
        return status;
    }
}

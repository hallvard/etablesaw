package etablesaw.ui.editor.commands;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import etablesaw.ui.editor.NatTablesawEditor;

public class TableCellsChangesOperation extends AbstractNatTablesawEditorOperation {

    private boolean done = false;
    private final TableCellChangeRecorder tableCellChanges;
    
    public TableCellsChangesOperation(NatTablesawEditor natTablesawEditor, TableCellChangeRecorder tableCellChanges) {
        super("Change Table Cells", natTablesawEditor);
        this.tableCellChanges = tableCellChanges;
    }
    
    public void setDone(boolean done) {
        this.done = done;
    }

    protected void doChanges(IProgressMonitor monitor, IAdaptable info, boolean undo) throws ExecutionException {
        tableCellChanges.doTableCellChanges(undo);
        getNatTablesawViewer().refresh(false);
    }

    @Override
    public IStatus execute(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
        if (! done) {
            doChanges(monitor, info, false);
        }
        return Status.OK_STATUS;                
    }
    
    @Override
    public IStatus undo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
        doChanges(monitor, info, true);
        return Status.OK_STATUS;                
    }
    
    @Override
    public IStatus redo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
        doChanges(monitor, info, false);
        return Status.OK_STATUS;                
    }
}

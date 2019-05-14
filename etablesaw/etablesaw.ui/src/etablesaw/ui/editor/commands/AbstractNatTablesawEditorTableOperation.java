package etablesaw.ui.editor.commands;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import etablesaw.ui.editor.NatTablesawEditor;
import tech.tablesaw.api.Table;

public abstract class AbstractNatTablesawEditorTableOperation extends AbstractNatTablesawEditorOperation {

    public AbstractNatTablesawEditorTableOperation(String name, NatTablesawEditor natTablesawEditor) {
        super(name, natTablesawEditor);
    }
    
    private Table oldTable;

    protected void replaceEditorTable(Table table) {
        oldTable = getNatTablesawEditor().getModelTable();
        getNatTablesawEditor().setModelTable(table);
    }

    protected void swapEditorTable() throws ExecutionException {
        if (oldTable == null) {
            throw new ExecutionException("Cannot replace, there is no old table");
        }
        Table table = oldTable;
        oldTable = getNatTablesawEditor().getModelTable();
        getNatTablesawEditor().setModelTable(table);
    }

    @Override
    public IStatus redo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
        swapEditorTable();
        return Status.OK_STATUS;
    }

    @Override
    public IStatus undo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
        swapEditorTable();
        return Status.OK_STATUS;
    }
}

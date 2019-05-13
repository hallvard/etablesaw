package etablesaw.ui.editor.commands;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.operations.AbstractOperation;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.nebula.widgets.nattable.selection.ISelectionModel;
import org.eclipse.swt.graphics.Rectangle;

import etablesaw.ui.editor.NatTablesawEditor;
import etablesaw.ui.editor.NatTablesawViewer;
import etablesaw.ui.editor.TablesawDataProvider;
import tech.tablesaw.api.Table;

public abstract class AbstractNatTablesawEditorOperation extends AbstractOperation {

    private final NatTablesawEditor natTablesawEditor;
    
    public AbstractNatTablesawEditorOperation(String name, NatTablesawEditor natTablesawEditor) {
        super(name);
        this.natTablesawEditor = natTablesawEditor;
    }
    
    protected NatTablesawEditor getNatTablesawEditor() {
        return natTablesawEditor;
    }
    
    protected NatTablesawViewer getNatTablesawViewer() {
        return getNatTablesawEditor().getNatTablesawViewer();
    }
    
    protected TablesawDataProvider getTablesawDataProvider() {
        return getNatTablesawViewer().getTablesawDataProvider();
    }

    private Table oldTable;

    protected void replaceEditorTable(Table table) {
        oldTable = getNatTablesawEditor().getModelTable();
        getNatTablesawEditor().setModelTable(table);
    }

    protected void replaceEditorTable() throws ExecutionException {
        if (oldTable == null) {
            throw new ExecutionException("Cannot replace with empty table");
        }
        Table table = oldTable;
        oldTable = getNatTablesawEditor().getModelTable();
        getNatTablesawEditor().setModelTable(table);
    }

    @Override
    public IStatus redo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
        replaceEditorTable();
        return Status.OK_STATUS;
    }

    @Override
    public IStatus undo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
        replaceEditorTable();
        return Status.OK_STATUS;
    }

    //

    protected void selectRows(int... rowNums) {
        ISelectionModel selectionModel = getNatTablesawEditor().getNatTablesawViewer().getSelectionLayer().getSelectionModel();
        int columnCount = getNatTablesawEditor().getModelTable().columnCount();
        for (int i = 0; i < rowNums.length; i++) {                
            selectionModel.addSelection(new Rectangle(0, rowNums[i], columnCount, 1));
        }
    }

    protected void selectColumns(int... columnNums) {
        ISelectionModel selectionModel = getNatTablesawEditor().getNatTablesawViewer().getSelectionLayer().getSelectionModel();
        int rowCount = getNatTablesawEditor().getModelTable().rowCount();
        for (int i = 0; i < columnNums.length; i++) {                
            selectionModel.addSelection(new Rectangle(columnNums[i], 0, 1, rowCount));
        }
    }
}

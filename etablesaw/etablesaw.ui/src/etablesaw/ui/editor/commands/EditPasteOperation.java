package etablesaw.ui.editor.commands;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.nebula.widgets.nattable.selection.SelectionLayer;

import etablesaw.ui.editor.NatTablesawEditor;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;

public class EditPasteOperation extends AbstractNatTablesawEditorTableOperation {

    private final Table table;
    
    public EditPasteOperation(NatTablesawEditor natTablesawEditor, Table table) {
        super("Paste Table", natTablesawEditor);
        this.table = table;
    }

    @Override
    public IStatus execute(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
        Table tableCopy = getNatTablesawEditor().getModelTable().copy();
        SelectionLayer selectionLayer = getNatTablesawViewer().getSelectionLayer();
        // What TODO?
        replaceEditorTable(table);
        return Status.OK_STATUS;
    }
    
    private void appendMissing(Column<?> column, int count) {
        while (count-- > 0) {
            column.appendMissing();
        }
    }
}

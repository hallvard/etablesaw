package etablesaw.ui.editor.commands;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.operations.IUndoableOperation;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.nebula.widgets.nattable.selection.SelectionLayer;

import etablesaw.ui.editor.NatTablesawEditor;

public class EditDeleteCommandHandler extends AbstractNatTablesawEditorHandler {

    @Override
    public IStatus execute(NatTablesawEditor editor) throws ExecutionException {        
        SelectionLayer selectionLayer = editor.getNatTablesawViewer().getSelectionLayer();
        int[] rows = selectionLayer.getFullySelectedRowPositions();
        if (rows != null && rows.length > 0) {
            IUndoableOperation operation = new DeleteRowsOperation(editor, rows);
            return execute(editor, operation, null, null);
        }
        int[] columns = selectionLayer.getFullySelectedColumnPositions();
        if (columns != null && columns.length > 0) {
            IUndoableOperation operation = new DeleteColumnsOperation(editor, columns);
            return execute(editor, operation, null, null);
        }
        return null;
    }
}

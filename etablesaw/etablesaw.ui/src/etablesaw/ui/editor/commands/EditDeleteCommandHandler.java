package etablesaw.ui.editor.commands;

import org.eclipse.nebula.widgets.nattable.selection.SelectionLayer;

import etablesaw.ui.editor.NatTablesawEditor;

public class EditDeleteCommandHandler extends AbstractNatTablesawEditorHandler {

    @Override
    public Object execute(NatTablesawEditor editor) {
        SelectionLayer selectionLayer = editor.getNatTablesawViewer().getSelectionLayer();
        int[] rows = selectionLayer.getFullySelectedRowPositions();
        if (rows != null && rows.length > 0) {
            return selectionLayer.doCommand(new DeleteRowsCommand(rows));
        }
        int[] columns = selectionLayer.getFullySelectedColumnPositions();
        if (columns != null && columns.length > 0) {
            return selectionLayer.doCommand(new DeleteColumnsCommand(columns));
        }
        return null;
    }
}

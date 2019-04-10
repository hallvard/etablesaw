package etablesaw.ui.editor.commands;

import org.eclipse.nebula.widgets.nattable.layer.ILayer;

import etablesaw.ui.editor.NatTablesawEditor;
import tech.tablesaw.api.Table;

public class DeleteRowsCommandHandler extends AbstractNatTablesawEditorCommandHandler<DeleteRowsCommand> {

    public DeleteRowsCommandHandler(NatTablesawEditor natTablesawEditor) {
        super(natTablesawEditor);
    }

    @Override
    public Class<DeleteRowsCommand> getCommandClass() {
        return DeleteRowsCommand.class;
    }

    @Override
    public boolean doCommand(ILayer targetLayer, DeleteRowsCommand command) {
        Table newTable = getNatTablesawEditor().getModelTable().dropRows(command.getRowNums());
        getNatTablesawEditor().setModelTable(newTable);
        return true;
    }
}

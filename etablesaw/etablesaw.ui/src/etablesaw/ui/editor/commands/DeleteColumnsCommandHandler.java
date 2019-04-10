package etablesaw.ui.editor.commands;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.nebula.widgets.nattable.layer.ILayer;

import etablesaw.ui.editor.NatTablesawEditor;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;

public class DeleteColumnsCommandHandler extends AbstractNatTablesawEditorCommandHandler<DeleteColumnsCommand> {

    public DeleteColumnsCommandHandler(NatTablesawEditor natTablesawEditor) {
        super(natTablesawEditor);
    }

    @Override
    public Class<DeleteColumnsCommand> getCommandClass() {
        return DeleteColumnsCommand.class;
    }

    @Override
    public boolean doCommand(ILayer targetLayer, DeleteColumnsCommand command) {
        Table table = getNatTablesawEditor().getModelTable();
        Collection<Column<?>> columns = new ArrayList<Column<?>>(table.columns());
        columns.removeAll(table.columns(command.getColumnNums()));
        Table newTable = table.select(columns.toArray(new Column<?>[columns.size()]));
        getNatTablesawEditor().setModelTable(newTable);
        return true;
    }
}

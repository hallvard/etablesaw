package etablesaw.ui.editor.commands;

import org.eclipse.nebula.widgets.nattable.command.AbstractContextFreeCommand;

public class DeleteRowsCommand extends AbstractContextFreeCommand {

    private final int[] rowNums;
    
    public DeleteRowsCommand(int... rowNums) {
        this.rowNums = rowNums;
    }
    
    public int[] getRowNums() {
        return rowNums;
    }
}

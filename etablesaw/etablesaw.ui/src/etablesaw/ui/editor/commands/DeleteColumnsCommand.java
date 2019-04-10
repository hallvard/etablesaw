package etablesaw.ui.editor.commands;

import org.eclipse.nebula.widgets.nattable.command.AbstractContextFreeCommand;

public class DeleteColumnsCommand extends AbstractContextFreeCommand {

    private final int[] colNums;
    
    public DeleteColumnsCommand(int... colNums) {
        this.colNums = colNums;
    }
    
    public int[] getColumnNums() {
        return colNums;
    }
}

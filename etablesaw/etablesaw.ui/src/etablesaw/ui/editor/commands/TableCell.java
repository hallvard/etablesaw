package etablesaw.ui.editor.commands;

public class TableCell {

    public final int row, column;
    public final Object value;

    public TableCell(int row, int column, Object value) {
        super();
        this.row = row;
        this.column = column;
        this.value = value;
    }
}

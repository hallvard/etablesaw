package etablesaw.ui.editor.commands;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.nebula.widgets.nattable.copy.action.CopyDataAction;
import org.eclipse.nebula.widgets.nattable.layer.cell.ILayerCell;
import org.eclipse.nebula.widgets.nattable.selection.SelectionLayer;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;

import etablesaw.ui.editor.NatTablesawEditor;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;
import tech.tablesaw.io.csv.CsvWriteOptions;
import tech.tablesaw.selection.BitmapBackedSelection;
import tech.tablesaw.selection.Selection;

public class EditCopyCommandHandler extends AbstractNatTablesawEditorHandler {

    private CopyDataAction copyAction = new CopyDataAction();
    
    @Override
    public IStatus execute(NatTablesawEditor editor) throws ExecutionException {
        if (editor.getNatTablesawViewer().hasActiveCellEditor()) {
            Control control = editor.getNatTablesawViewer().getControl().getActiveCellEditor().getEditorControl();
            if (control instanceof Text) {
                ((Text) control).copy();
            } else {
                copyAction.run(editor.getNatTablesawViewer().getControl(), null);
            }
            return Status.OK_STATUS;
        }
        Table tableCopy = createTableCopy(editor);
        if (tableCopy != null) {
            ByteArrayOutputStream output = new ByteArrayOutputStream();
            CsvWriteOptions writeOptions = getWriteOptions(output);
            tableCopy.write().csv(writeOptions);
            try {
                output.close();
            } catch (IOException e) {
            }
            copyToClipboard(editor, output.toString());
        }
        return Status.OK_STATUS;
    }

    protected Table createTableCopy(NatTablesawEditor editor) {
        Table table = editor.getTable();
        SelectionLayer selectionLayer = editor.getNatTablesawViewer().getSelectionLayer();
        List<TableCell> cells = new ArrayList<TableCell>();
        for (ILayerCell cell : selectionLayer.getSelectedCells()) {
            for (int colNum = cell.getColumnIndex(); colNum < cell.getColumnIndex() + cell.getColumnSpan(); colNum++) {
                Column<?> column = table.column(colNum);
                for (int rowNum = cell.getRowIndex(); rowNum < cell.getRowIndex() + cell.getRowSpan(); rowNum++) {
                    Object value = (column.isMissing(rowNum) ? null : column.get(rowNum));
                    cells.add(new TableCell(rowNum, colNum, value));
                }
            }
        }
        if (cells.isEmpty()) {
            return null;
        }
        Collections.sort(cells, (cell1, cell2) -> {
            int diff = cell1.column - cell2.column;
            if (diff == 0) {
                diff = cell1.row - cell2.row;
            }
            return diff;
        });
        Map<String, Column<?>> columns = new LinkedHashMap<>(cells.size());
        int minRow = cells.get(0).row, maxRow = cells.get(cells.size() - 1).row;
        Selection selection = new BitmapBackedSelection();
        for (TableCell cell : cells) {
            Column<?> column = table.column(cell.column);
            Column<?> columnCopy = columns.get(column.name());
            if (columnCopy == null) {
                columnCopy = column.emptyCopy();
                columns.put(column.name(), columnCopy);
            }
            int rowNum = cell.row - minRow;
            appendMissing(columnCopy, rowNum - columnCopy.size());
            if (cell.value == null) {
                columnCopy.appendMissing();
            } else {
                columnCopy.appendCell(String.valueOf(cell.value));
                // select only non-empty rows
                selection.addRange(rowNum, rowNum + 1);
            }
        }
        Collection<Column<?>> columns2 = columns.values();
        // make all columns same size
        for (Column<?> column : columns2) {
            while (column.size() < maxRow - minRow + 1) {
                column.appendMissing();
            }
        }
        Table tableCopy = Table.create("copy", columns2.toArray(new Column[0]));
        return tableCopy.where(selection);
    }
    
    private void appendMissing(Column<?> column, int count) {
        while (count-- > 0) {
            column.appendMissing();
        }
    }

    protected void copyToClipboard(NatTablesawEditor editor, String textData) {
        final Clipboard cb = new Clipboard(editor.getSite().getShell().getDisplay());
        TextTransfer textTransfer = TextTransfer.getInstance();
        try {
            cb.setContents(new Object[] { textData }, new Transfer[] { textTransfer });
        } finally {
            cb.dispose();
        }
    }

    protected CsvWriteOptions getWriteOptions(OutputStream output) {
        return CsvWriteOptions.builder(output).separator('\t').build();
    }
}

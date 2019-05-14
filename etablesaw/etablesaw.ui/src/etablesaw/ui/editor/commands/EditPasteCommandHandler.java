package etablesaw.ui.editor.commands;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.operations.IUndoableOperation;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.nebula.widgets.nattable.copy.action.PasteDataAction;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;

import etablesaw.ui.editor.NatTablesawEditor;
import tech.tablesaw.api.Table;
import tech.tablesaw.io.csv.CsvReadOptions;

public class EditPasteCommandHandler extends AbstractNatTablesawEditorHandler {

    private PasteDataAction pasteAction = new PasteDataAction();
    
    @Override
    public IStatus execute(NatTablesawEditor editor) throws ExecutionException {
        if (editor.getNatTablesawViewer().hasActiveCellEditor()) {
            Control control = editor.getNatTablesawViewer().getControl().getActiveCellEditor().getEditorControl();
            if (control instanceof Text) {
                ((Text) control).paste();
            } else {
                pasteAction.run(editor.getNatTablesawViewer().getControl(), null);
            }
            return Status.OK_STATUS;
        }
        String contents = null;
        final Clipboard cb = new Clipboard(editor.getSite().getShell().getDisplay());
        try {
            contents = (String) cb.getContents(TextTransfer.getInstance());
        } finally {
            cb.dispose();
        }
        if (contents != null) {
            // TODO: should be more intelligent, e.g. guess if there are headers
            ByteArrayInputStream input = new ByteArrayInputStream(contents.getBytes());
            CsvReadOptions readOptions = getReadOptions(input);
            Table table = null;
            try {
                table = Table.read().csv(readOptions);
            } catch (IOException e) {
            }
            if (table != null) {
                IUndoableOperation operation = new EditPasteOperation(editor, table);
                return execute(editor, operation, null, null);
            }
        }
        return Status.CANCEL_STATUS;
    }

    protected CsvReadOptions getReadOptions(InputStream input) {
        return CsvReadOptions.builder(input).separator('\t').build();
    }
}

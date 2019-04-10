package etablesaw.ui.editor.commands;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.handlers.HandlerUtil;

import etablesaw.ui.editor.NatTablesawEditor;

public abstract class AbstractNatTablesawEditorHandler extends AbstractHandler {

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        IEditorPart activeEditor = HandlerUtil.getActiveEditor(event);
        if (activeEditor instanceof NatTablesawEditor) {
            return execute((NatTablesawEditor) activeEditor);
        }
        return null;
    }

    protected abstract Object execute(NatTablesawEditor activeEditor);
}

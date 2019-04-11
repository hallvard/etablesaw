package etablesaw.ui.editor.commands;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.operations.IOperationHistory;
import org.eclipse.core.commands.operations.IUndoableOperation;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbench;
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

    protected IStatus execute(NatTablesawEditor editor, IUndoableOperation operation, IProgressMonitor monitor, IAdaptable adaptable) throws ExecutionException {
        IWorkbench workbench = editor.getSite().getWorkbenchWindow().getWorkbench();
        IOperationHistory operationHistory = workbench.getOperationSupport().getOperationHistory();
        operation.addContext(editor.getUndoContext());
        return operationHistory.execute(operation, monitor, adaptable);
    }

    protected abstract IStatus execute(NatTablesawEditor activeEditor) throws ExecutionException;
}

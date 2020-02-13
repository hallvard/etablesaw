package etablesaw.ui.editor;

import java.util.function.Consumer;

import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;

import etablesaw.ui.util.ResourceChangeHelper;

public class EditorResourceChangeHelper extends ResourceChangeHelper {

    private final IEditorPart editorPart;

    public EditorResourceChangeHelper(IEditorPart editorPart, Consumer<IPath> changeHandler) {
        super(changeHandler);
        this.editorPart = editorPart;
    }

    protected boolean isPath(IPath path) {
        return path.equals(((IFileEditorInput) editorPart.getEditorInput()).getFile().getFullPath());
    }

    @Override
    protected void handleRemovedResource() {
        editorPart.getSite().getShell().getDisplay().asyncExec(() -> {
            if (editorPart.isDirty()) {
                editorPart.getEditorSite().getPage().closeEditor(editorPart, false);
            }
        });
    }
    
    @Override
    protected void handleChangedResource(IPath path) {
        if (isPath(path)) {
            editorPart.getSite().getShell().getDisplay().asyncExec(() -> {
                // need this to avoid opening the dialog twice, don't know why
                boolean reload = true;
                if (editorPart.isDirty()) {
                    reload = (Dialog.OK != MessageDialog.open(MessageDialog.WARNING, editorPart.getSite().getShell(), "Reload data?", path.lastSegment() + " has changed outside editor, would you like to reload it?", SWT.SHEET,
                            "Keep editing", "Reload data"));
                }
                if (reload) {
                    super.handleChangedResource(path);
                }
            });
        } else {
            super.handleChangedResource(path);
        }
    }
}

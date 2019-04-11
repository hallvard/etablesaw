package etablesaw.ui.editor;

import java.util.function.Consumer;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;

public class ResourceChangeHelper implements IResourceChangeListener, IResourceDeltaVisitor {

    private final IEditorPart editorPart;
    private final Consumer<IPath> changeHandler;

    public ResourceChangeHelper(IEditorPart editorPart, Consumer<IPath> changeHandler) {
        this.editorPart = editorPart;
        this.changeHandler = changeHandler;
        ResourcesPlugin.getWorkspace().addResourceChangeListener(this, IResourceChangeEvent.POST_CHANGE);
    }

    public void dispose() {
        ResourcesPlugin.getWorkspace().removeResourceChangeListener(this);
    }

    private boolean removed = false;
    private IPath changed = null;

    protected boolean isEditorInput(IPath path) {
        return path.equals(((IFileEditorInput) editorPart.getEditorInput()).getFile().getFullPath());
    }

    public void resourceChanged(IResourceChangeEvent event) {
        IResourceDelta delta = event.getDelta();
        try {
            delta.accept(this);
            if (removed) {
                editorPart.getSite().getShell().getDisplay().asyncExec(new Runnable() {
                    @Override
                    public void run() {
                        if (editorPart.isDirty()) {
                            editorPart.getEditorSite().getPage().closeEditor(editorPart, false);
                        }
                    }
                });
            } else if (changed != null) {
                if (isEditorInput(changed)) {
                    editorPart.getSite().getShell().getDisplay().asyncExec(new Runnable() {
                        @Override
                        public void run() {
                            // need this to avoid opening the dialog twice, don't know why
                            if (changed != null) {
                                IPath file = changed;
                                changed = null;
                                boolean reload = true;
                                if (editorPart.isDirty()) {
                                    reload = (Dialog.OK != MessageDialog.open(MessageDialog.WARNING, editorPart.getSite().getShell(), "Reload data?", file.lastSegment() + " has changed outside editor, would you like to reload it?", SWT.SHEET,
                                            "Keep editing", "Reload data"));
                                }
                                if (reload) {
                                    changeHandler.accept(file);
                                }
                            }
                        };
                    });
                } else {
                    changeHandler.accept(changed);
                }
            }
        } catch (CoreException e) {
        }
    }

    public boolean visit(final IResourceDelta delta) {
        if (delta.getResource().getType() == IResource.FILE) {
            if (delta.getKind() == IResourceDelta.REMOVED || delta.getKind() == IResourceDelta.CHANGED) {
                if (isEditorInput(delta.getFullPath())) {
                    int flags = delta.getFlags();
                    if (delta.getKind() == IResourceDelta.REMOVED) {
                        if ((flags & IResourceDelta.MOVED_TO) != 0) {
                            changed = delta.getMovedToPath();
                        } else {
                            removed = true;
                        }
                    } else if ((flags & IResourceDelta.CONTENT) != 0) {
                        changed = delta.getFullPath();
                    }
                    return false;
                }
            }
        }
        return true;
    }
}

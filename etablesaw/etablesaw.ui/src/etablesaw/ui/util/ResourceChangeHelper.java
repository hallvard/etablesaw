package etablesaw.ui.util;

import java.util.function.Consumer;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;

public abstract class ResourceChangeHelper implements IResourceChangeListener, IResourceDeltaVisitor {

    private final Consumer<IPath> changeHandler;

    public ResourceChangeHelper(Consumer<IPath> changeHandler) {
        this.changeHandler = changeHandler;
        ResourcesPlugin.getWorkspace().addResourceChangeListener(this, IResourceChangeEvent.POST_CHANGE);
    }

    public void dispose() {
        ResourcesPlugin.getWorkspace().removeResourceChangeListener(this);
    }

    private boolean removed = false;
    private IPath changed = null;

    protected abstract boolean isPath(IPath path);

    public void resourceChanged(IResourceChangeEvent event) {
        IResourceDelta delta = event.getDelta();
        try {
            delta.accept(this);
            if (removed) {
                handleRemovedResource();
            } else if (changed != null) {
                IPath path = changed;
                changed = null;
                handleChangedResource(path);
            }
        } catch (CoreException e) {
        }
    }

    protected void handleChangedResource(IPath path) {
        changeHandler.accept(path);
    }

    protected void handleRemovedResource() {
        changeHandler.accept(null);
    }

    public boolean visit(final IResourceDelta delta) {
        if (delta.getResource().getType() == IResource.FILE) {
            if (delta.getKind() == IResourceDelta.REMOVED || delta.getKind() == IResourceDelta.CHANGED) {
                if (isPath(delta.getFullPath())) {
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

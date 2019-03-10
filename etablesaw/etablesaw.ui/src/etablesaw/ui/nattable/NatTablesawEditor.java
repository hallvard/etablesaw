package etablesaw.ui.nattable;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.EditorPart;

import etablesaw.io.FileFormatSupport;
import etablesaw.ui.Activator;
import etablesaw.ui.TableProvider;
import tech.tablesaw.api.Table;

public class NatTablesawEditor extends EditorPart implements TableProvider, ISelectionProvider {

	private Table modelTable;

	@Override
	public void init(final IEditorSite site, final IEditorInput input) throws PartInitException {
		setSite(site);
		setInput(input);
		if (input instanceof IFileEditorInput) {
			final IFile file = ((IFileEditorInput) input).getFile();
			setPartName(file.getName());
			load(file, null);
		}
	}

    protected void load(final IFile file, final IProgressMonitor monitor) throws PartInitException {
        try {
            FileFormatSupport ffs = Activator.getInstance().getFileFormatSupport(file.getFileExtension());
            if (ffs == null) {
                throw new PartInitException("Unsupported file format: " + file.getName());
            }
            Table[] tables = ffs.read(file.getName(), () -> {
                try {
                    return file.getContents();
                } catch (CoreException e) {
                }
                return null;
            });
            if (tables == null || tables.length == 0) {
                throw new PartInitException("Couldn't read table from: " + file.getName());
            }
            modelTable = tables[0];
        } catch (final Exception e) {
            throw new PartInitException("Couldn't read table from: " + file.getName(), e);
        }
    }

	@Override
	public void doSave(final IProgressMonitor monitor) {
		if (modelTable != null) {
			save(((IFileEditorInput) getEditorInput()).getFile(), monitor);
		}
	}

	@Override
	public void doSaveAs() {
	}

	protected void save(final IFile file, final IProgressMonitor monitor) {
        try {
            FileFormatSupport ffs = Activator.getInstance().getFileFormatSupport(file.getFileExtension());
			final ByteArrayOutputStream output = new ByteArrayOutputStream();
			ffs.write(new Table[]{ modelTable }, file.getName(), output);
			file.setContents(new ByteArrayInputStream(output.toByteArray()), 0, monitor);
			setDirty(false);
		} catch (final CoreException | IOException e) {
		}
	}

	private boolean dirty = false;

	protected void setDirty(final boolean dirty) {
		final boolean changed = this.dirty != dirty;
		this.dirty = dirty;
		if (changed) {
			firePropertyChange(IEditorPart.PROP_DIRTY);
		}
	}

	@Override
	public boolean isDirty() {
		return dirty;
	}

	@Override
	public boolean isSaveAsAllowed() {
		return false;
	}

	private NatTablesawViewer natTablesawViewer;

	@Override
	public void createPartControl(final Composite parent) {
		natTablesawViewer = new NatTablesawViewer();
		natTablesawViewer.setEditable(true);
		natTablesawViewer.createPartControl(parent);
		natTablesawViewer.setInput(modelTable);
		natTablesawViewer.addTableChangeListener(new TablesawDataProvider.Listener() {
			@Override
			public void rowsChanged(final int startRange, final int endRange) {
				// used for filters
			}
			@Override
			public void cellChanged(final int row, final int column, final Object oldValue, final Object newValue) {
				setDirty(true);
			}
		});
		if (getEditorInput() instanceof IFileEditorInput) {
			final IFile file = ((IFileEditorInput) getEditorInput()).getFile();
			Activator.getInstance().getTableProviderRegistry().registerTableProvider(file.getName(), this);
		}
		//		tablesawViewer.addSelectionChangedListener(selectionChangeListener);
	}

	@Override
	public void setFocus() {
	}

	// TableProvider

	@Override
	public Table getTable() {
		return natTablesawViewer.getTable();
	}

	@Override
	public void addTableDataProviderListener(final TableProvider.Listener listener) {
		natTablesawViewer.addTableDataProviderListener(listener);
	}

	@Override
	public void removeTableDataProviderListener(final TableProvider.Listener listener) {
		natTablesawViewer.removeTableDataProviderListener(listener);
	}

	// ISelectionProvider

	@Override
	public void setSelection(final ISelection selection) {
		// not sure this makes much sense
	}

	@Override
	public ISelection getSelection() {
		return StructuredSelection.EMPTY;
	}

	// forward selection changes
	private final ISelectionChangedListener selectionChangeListener = new ISelectionChangedListener() {
		@Override
		public void selectionChanged(final SelectionChangedEvent event) {
			if (selectionListeners != null) {
				for (final ISelectionChangedListener selectionChangedListener : selectionListeners) {
					selectionChangedListener.selectionChanged(event);
				}
			}
		}
	};

	private Collection<ISelectionChangedListener> selectionListeners;

	@Override
	public void addSelectionChangedListener(final ISelectionChangedListener listener) {
		if (selectionListeners == null) {
			selectionListeners = new ArrayList<ISelectionChangedListener>();
		}
		selectionListeners.add(listener);
	}

	@Override
	public void removeSelectionChangedListener(final ISelectionChangedListener listener) {
		if (selectionListeners != null) {
			selectionListeners.remove(listener);
		}
	}

	// IAdaptable

	@Override
	public <T> T getAdapter(final Class<T> adapter) {
		return super.getAdapter(adapter);
	}
}

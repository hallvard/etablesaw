package etablesaw.ui.editor;

import etablesaw.io.FileFormatSupport;
import etablesaw.ui.Activator;
import etablesaw.ui.TableProvider;
import etablesaw.ui.editor.commands.AbstractNatTablesawEditorHandler;
import etablesaw.ui.editor.commands.AddColumnOperation;
import etablesaw.ui.editor.commands.TableCellChangeRecorder;
import etablesaw.ui.editor.commands.TableCellsChangedOperation;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.Consumer;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.operations.IOperationHistory;
import org.eclipse.core.commands.operations.IUndoContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.action.IStatusLineManager;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.SaveAsDialog;
import org.eclipse.ui.operations.UndoRedoActionGroup;
import org.eclipse.ui.part.EditorPart;
import org.eclipse.ui.part.FileEditorInput;
import tech.tablesaw.api.ColumnType;
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
            try {
                load(file, null);
                resourceChangeHelper = new EditorResourceChangeHelper(this, this::handleResourceChange);
            } catch (Exception e) {
                throw new PartInitException(e.getMessage(), e);
            }
        }
    }

    private EditorResourceChangeHelper resourceChangeHelper;

    @Override
    public void dispose() {
        if (resourceChangeHelper != null) {
            resourceChangeHelper.dispose();
        }
        registerTableProvider(null);
        super.dispose();
    }

    protected void load(final IFile file, final IProgressMonitor monitor) throws RuntimeException {
        try {
            String fileFormat = file.getFileExtension();
            FileFormatSupport ffs = Activator.getInstance().getFileFormatSupport(fileFormat);
            if (ffs == null || Boolean.FALSE.equals(ffs.supportsFormat(fileFormat))) {
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
                throw new RuntimeException("Couldn't read table from: " + file.getName());
            }
            modelTable = tables[0];
            setDirty(false);
        } catch (final Exception e) {
            throw new RuntimeException("Couldn't read table from: " + file.getName(), e);
        }
    }

    protected void handleResourceChange(IPath path) {
        IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(path);
        if (path.equals(((IFileEditorInput) getEditorInput()).getFile().getFullPath())) {
            // same file changed outside editor
            load(file, null);
            getNatTablesawViewer().setInput(getModelTable());
        } else {
            // rename
            setEditorInput(file);
        }
    }

    protected void setEditorInput(IFile newFile) {
        setInput(new FileEditorInput(newFile));
        setPartName(newFile.getName());
        firePropertyChange(IEditorPart.PROP_INPUT);
    }

    public Table getModelTable() {
        return modelTable;
    }

    public void setModelTable(Table table) {
        this.modelTable = table;
        setDirty();
        natTablesawViewer.setInput(modelTable);
    }

    @Override
    public void doSave(final IProgressMonitor monitor) {
        if (modelTable != null) {
            save(((IFileEditorInput) getEditorInput()).getFile(), monitor);
        }
    }

    @Override
    public void doSaveAs() {
        IProgressMonitor progressMonitor = getProgressMonitor();
        final IEditorInput input = getEditorInput();
        SaveAsDialog dialog = new SaveAsDialog(PlatformUI.getWorkbench().getModalDialogShellProvider().getShell());
        IFile original = (input instanceof IFileEditorInput) ? ((IFileEditorInput) input).getFile() : null;
        if (original != null) {
            dialog.setOriginalFile(original);
        } else {
            dialog.setOriginalName(input.getName());
        }
        dialog.create();
        if (dialog.open() == Window.CANCEL || dialog.getResult() == null) {
            if (progressMonitor != null) {
                progressMonitor.setCanceled(true);
            }
            return;
        }
        IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(dialog.getResult());
        try {
            save(file, progressMonitor);
            setEditorInput(file);
        } catch (Exception e) {
            if (progressMonitor != null) {
                progressMonitor.setCanceled(true);
            }
        }
    }

    public IProgressMonitor getProgressMonitor() {
        IStatusLineManager statusLineManager = getEditorSite().getActionBars().getStatusLineManager();
        IProgressMonitor progressMonitor = statusLineManager != null ? statusLineManager.getProgressMonitor()
                : new NullProgressMonitor();
        return progressMonitor;
    }

    public static void save(final Table table, final IFile file, final IProgressMonitor monitor) throws Exception {
        String fileFormat = file.getFileExtension();
        FileFormatSupport ffs = Activator.getInstance().getFileFormatSupport(fileFormat);
        if (ffs == null) {
            throw new RuntimeException("Unsupported file format: " + file.getName());
        } else if (!Boolean.TRUE.equals(ffs.supportsFormat(fileFormat))) {
            throw new RuntimeException("Write of file format not supported: " + file.getName());
        }
        final ByteArrayOutputStream output = new ByteArrayOutputStream();
        ffs.write(new Table[] { table }, file.getName(), output);
        ByteArrayInputStream source = new ByteArrayInputStream(output.toByteArray());
        if (file.exists()) {
            file.setContents(source, 0, monitor);
        } else {
            file.create(source, 0, monitor);
        }
    }

    protected void save(final IFile file, final IProgressMonitor monitor) {
        try {
            save(modelTable, file, monitor);
            setDirty(false);
        } catch (final Exception e) {
            System.err.println(e);
        }
    }

    private NatTablesawEditorUndoContext undoContext = new NatTablesawEditorUndoContext();

    public IUndoContext getUndoContext() {
        return undoContext;
    }

    private boolean dirty = false;

    protected void setDirty(final boolean dirty) {
        final boolean changed = this.dirty != dirty;
        this.dirty = dirty;
        if (changed) {
            firePropertyChange(IEditorPart.PROP_DIRTY);
        }
    }

    public void setDirty() {
        setDirty(true);
    }

    @Override
    public boolean isDirty() {
        return dirty;
    }

    @Override
    public boolean isSaveAsAllowed() {
        return getEditorInput() instanceof IFileEditorInput;
    }

    private NatTablesawViewer natTablesawViewer;

    @Override
    public void createPartControl(final Composite parent) {
        natTablesawViewer = new NatTablesawViewer() {
            @Override
            protected TableProvider getTableProvider() {
                return NatTablesawEditor.this;
            }
            @Override
            protected void createExtraPopupControls(Composite parent) {
                createColumnTypeSelector(parent, colType -> {
                    AddColumnOperation addColumnOperation = new AddColumnOperation(NatTablesawEditor.this, colType);
                    addColumnOperation.addContext(getUndoContext());
                    try {
                        AbstractNatTablesawEditorHandler.execute(NatTablesawEditor.this, addColumnOperation, null, null);
                        TablesawDataProvider tablesawDataProvider = NatTablesawEditor.this.natTablesawViewer.getTablesawDataProvider();
                        tablesawDataProvider.addColumnNames(addColumnOperation.getColumnName());
                    } catch (ExecutionException e) {
                    }
                });
            }
            private Control createColumnTypeSelector(Composite parent, Consumer<ColumnType> callback) {
                List<String> comboItems = new ArrayList<>();
                comboItems.add("Add column of type...");
                comboItems.addAll(ColumnType.values.keySet());
                final Combo colTypeSelector = new Combo(parent, SWT.READ_ONLY);
                colTypeSelector.setItems(comboItems.toArray(new String[comboItems.size()]));
                colTypeSelector.select(0);
                colTypeSelector.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        int selectionIndex = colTypeSelector.getSelectionIndex();
                        parent.dispose();
                        if (selectionIndex > 0) {
                            ColumnType colType = ColumnType.values.get(comboItems.get(selectionIndex));
                            callback.accept(colType);
                        }
                    }
                });
                return colTypeSelector;
            }
        };
        natTablesawViewer.setEditable(true);
        natTablesawViewer.setOnTableCellChanges(tableCellChanges -> executeTableCellChangesOperation(tableCellChanges, true));
        natTablesawViewer.createPartControl(parent);
        natTablesawViewer.setInput(modelTable);
        natTablesawViewer.addTableChangeListener(new TablesawDataProvider.Listener() {
            @Override
            public void providerRowsChanged(final int startRange, final int endRange) {
                // used for filters
            }

            @Override
            public void tableCellChanged(final int row, final int column, final Object oldValue, final Object newValue) {
                setDirty();
            }
        });
        registerTableProvider(this);
        natTablesawViewer.addSelectionChangedListener(selectionChangeListener);
        // support undo and redo
        new UndoRedoActionGroup(getEditorSite(), getUndoContext(), true)
                .fillActionBars(getEditorSite().getActionBars());
    }

    private void registerTableProvider(TableProvider tableProvider) {
        if (getEditorInput() instanceof IFileEditorInput) {
            final IFile file = ((IFileEditorInput) getEditorInput()).getFile();
            Activator.getInstance().getTableProviderRegistry().registerTableProvider(file.getName(), tableProvider);
        }
    }

    @Override
    public void setFocus() {
    }

    public NatTablesawViewer getNatTablesawViewer() {
        return natTablesawViewer;
    }
    
    protected void executeTableCellChangesOperation(TableCellChangeRecorder tableCellChanges, boolean done) {
        IWorkbench workbench = getSite().getWorkbenchWindow().getWorkbench();
        IOperationHistory operationHistory = workbench.getOperationSupport().getOperationHistory();
        TableCellsChangedOperation operation = new TableCellsChangedOperation(this, tableCellChanges);
        operation.setDone(done);
        operation.addContext(getUndoContext());
        try {
            operationHistory.execute(operation, null, null);
        } catch (ExecutionException e) {
        }
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

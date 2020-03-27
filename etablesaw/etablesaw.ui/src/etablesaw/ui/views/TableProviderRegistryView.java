package etablesaw.ui.views;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import etablesaw.ui.Activator;
import etablesaw.ui.TableProvider;
import etablesaw.ui.TableProviderHelper;
import etablesaw.ui.TableProviderRegistry;
import etablesaw.ui.util.ResourceTableProvider;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.viewers.ViewerDropAdapter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ResourceTransfer;
import org.eclipse.ui.part.ViewPart;
import tech.tablesaw.api.Table;

public class TableProviderRegistryView extends ViewPart implements TableProvider {

    private TableViewer viewer;
    
    private List<String> keyOrder = new ArrayList<String>();
    
    private Collection<ResourceTableProvider> resourceTableProviders;
    
    @Override
    public void createPartControl(Composite parent) {
        final GridLayout layout = new GridLayout(1, false);
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        layout.horizontalSpacing = 0;
        layout.verticalSpacing = 0;
        parent.setLayout(layout);
        
        viewer = new TableViewer(parent, SWT.H_SCROLL);
        viewer.getControl().setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        viewer.getTable().setHeaderVisible(true);

        TableViewerColumn keyColumn = new TableViewerColumn(viewer, SWT.NONE);
        keyColumn.getColumn().setText("Key");
        keyColumn.getColumn().setWidth(400);
        keyColumn.setLabelProvider(new ColumnLabelProvider());

        final TableProviderRegistry tableProviderRegistry = Activator.getInstance().getTableProviderRegistry();
        viewer.setContentProvider(new ArrayContentProvider() {
            @Override
            public Object[] getElements(Object inputElement) {
                if (inputElement instanceof TableProviderRegistry) {
                    Collection<String> tableProviderKeys = ((TableProviderRegistry) inputElement).getTableProviderKeys();
                    // don't show itself
                    tableProviderKeys.remove(getTableProviderKey());
                    return super.getElements(tableProviderKeys);
                }
                return super.getElements(inputElement);
            }
        });
        viewer.addSelectionChangedListener(new ISelectionChangedListener() {
            @Override
            public void selectionChanged(SelectionChangedEvent event) {
                if (tableProviderHelper != null) {
                    tableProviderHelper.fireTableChanged(TableProviderRegistryView.this);
                }
            }
        });
        tableProviderRegistry.registerTableProvider(getTableProviderKey(), TableProviderRegistryView.this);

        viewer.setComparator(new ViewerComparator() {
            public int compare(org.eclipse.jface.viewers.Viewer viewer, Object key1, Object key2) {
                int pos1 = keyOrder.indexOf(key1);
                if (pos1 < 0) {
                    pos1 = keyOrder.size();
                }
                int pos2 = keyOrder.indexOf(key2);
                if (pos2 < 0) {
                    pos2 = keyOrder.size();
                }
                if (pos1 == pos2) {
                    return super.compare(viewer, key1, key2);
                } else {
                    return pos1 - pos2;
                }
            }
        });
        
        viewer.setInput(tableProviderRegistry);
        tableProviderRegistry.addTableRegistryChangedListener(new TableProviderRegistry.Listener() {
            @Override
            public void tableProviderRegistryChanged(String key, TableProvider tableProvider) {
                refreshViewer(key);
            }
        });
        tableProviderRegistry.addTableDataProviderListener(new TableProvider.Listener() {
            @Override
            public void tableChanged(TableProvider tableProvider) {
                refreshViewer(tableProviderRegistry.getTableProviderKey(tableProvider));
            }
            @Override
            public void tableDataChanged(TableProvider tableProvider) {
                refreshViewer(tableProviderRegistry.getTableProviderKey(tableProvider));
            }
        });
        
        Transfer[] transfers = new Transfer[] { ResourceTransfer.getInstance() };
        ViewerDropAdapter viewerDropAdapter = new ViewerDropAdapter(viewer) {
            
            @Override
            public boolean validateDrop(Object target, int operation, TransferData transferType) {
                for (int i = 0; i < transfers.length; i++) {
                    if (transfers[i].isSupportedType(transferType)) {
                        overrideOperation(DND.DROP_COPY);
                        return true;
                    }
                }
                return false;
            }
            
            @Override
            public boolean performDrop(Object data) {
                Collection<IFile> files = new ArrayList<>();
                Collection<?> elements = null;
                if (data instanceof Object[]) {
                    elements = Arrays.asList((Object[]) data);
                } else if (data instanceof Collection<?>) {
                    elements = (Collection<?>) data;
                }
                for (Object element : elements) {
                    if (element instanceof IFile) {
                        files.add((IFile) element);
                    }
                }
                addResourceTableProviders(files);
                return true;
            }
        };
        viewerDropAdapter.setFeedbackEnabled(false);
        viewerDropAdapter.setSelectionFeedbackEnabled(false);
        viewer.addDropSupport(DND.DROP_MOVE | DND.DROP_COPY, transfers, viewerDropAdapter);
    }

    private String getTableProviderKey() {
        return getPartName();
    }

    @Override
    public void dispose() {
        Activator.getInstance().getTableProviderRegistry().registerTableProvider(getTableProviderKey(), null);
        if (resourceTableProviders != null) {
            for (ResourceTableProvider resourceTableProvider : resourceTableProviders) {
                resourceTableProvider.dispose();
            }
            resourceTableProviders = null;
        }
        super.dispose();
    }

    protected void addResourceTableProviders(Collection<IFile> files) {
        final TableProviderRegistry tableProviderRegistry = Activator.getInstance().getTableProviderRegistry();
        for (IFile file : files) {
            if (ResourceTableProvider.supportsFile(file)) {
                ResourceTableProvider resourceTableProvider = new ResourceTableProvider(file);
                tableProviderRegistry.registerTableProvider(file.getName(), resourceTableProvider);
                if (resourceTableProviders == null) {
                    resourceTableProviders = new ArrayList<>();
                }
                resourceTableProviders.add(resourceTableProvider);
            }
        }
    }
    
    private void refreshViewer(String newFirstKey) {
        if (newFirstKey != null) {
            if (! keyOrder.remove(newFirstKey)) {
                return;
            }
            keyOrder.add(0, newFirstKey);
        }
        if (! viewer.getControl().isDisposed()) {
            viewer.refresh();
        }
    }

    @Override
    public void setFocus() {
        viewer.getControl().setFocus();
    }
    
    @Override
    public Table getTable() {
        IStructuredSelection selection = viewer.getStructuredSelection();
        if (! selection.isEmpty()) {
            TableProviderRegistry providerRegistry = Activator.getInstance().getTableProviderRegistry();
            String key = String.valueOf(selection.getFirstElement());
            TableProvider tableProvider = providerRegistry.getTableProvider(key);
            if (tableProvider != null) {
                return tableProvider.getTable();
            }
        }
        return null;
    }
    
    private TableProviderHelper tableProviderHelper = null;

    @Override
    public void addTableDataProviderListener(Listener listener) {
        if (tableProviderHelper == null) {
            tableProviderHelper = new TableProviderHelper();
        }
        tableProviderHelper.addTableDataProviderListener(listener);
    }

    @Override
    public void removeTableDataProviderListener(Listener listener) {
        if (tableProviderHelper != null) {
            tableProviderHelper.removeTableDataProviderListener(listener);
        }
    }
}

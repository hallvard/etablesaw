package etablesaw.ui.editor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.nebula.widgets.nattable.NatTable;
import org.eclipse.nebula.widgets.nattable.command.ILayerCommandHandler;
import org.eclipse.nebula.widgets.nattable.config.AbstractRegistryConfiguration;
import org.eclipse.nebula.widgets.nattable.config.AbstractUiBindingConfiguration;
import org.eclipse.nebula.widgets.nattable.config.CellConfigAttributes;
import org.eclipse.nebula.widgets.nattable.config.DefaultNatTableStyleConfiguration;
import org.eclipse.nebula.widgets.nattable.config.IConfigRegistry;
import org.eclipse.nebula.widgets.nattable.config.IEditableRule;
import org.eclipse.nebula.widgets.nattable.data.IDataProvider;
import org.eclipse.nebula.widgets.nattable.data.convert.DefaultDisplayConverter;
import org.eclipse.nebula.widgets.nattable.edit.EditConfigAttributes;
import org.eclipse.nebula.widgets.nattable.edit.action.MouseEditAction;
import org.eclipse.nebula.widgets.nattable.edit.command.UpdateDataCommand;
import org.eclipse.nebula.widgets.nattable.edit.command.UpdateDataCommandHandler;
import org.eclipse.nebula.widgets.nattable.edit.config.DefaultEditBindings;
import org.eclipse.nebula.widgets.nattable.edit.config.DefaultEditConfiguration;
import org.eclipse.nebula.widgets.nattable.filterrow.FilterRowHeaderComposite;
import org.eclipse.nebula.widgets.nattable.filterrow.IFilterStrategy;
import org.eclipse.nebula.widgets.nattable.grid.GridRegion;
import org.eclipse.nebula.widgets.nattable.grid.data.DefaultCornerDataProvider;
import org.eclipse.nebula.widgets.nattable.grid.layer.ColumnHeaderLayer;
import org.eclipse.nebula.widgets.nattable.grid.layer.CornerLayer;
import org.eclipse.nebula.widgets.nattable.grid.layer.GridLayer;
import org.eclipse.nebula.widgets.nattable.grid.layer.RowHeaderLayer;
import org.eclipse.nebula.widgets.nattable.hideshow.ColumnHideShowLayer;
import org.eclipse.nebula.widgets.nattable.layer.AbstractLayer;
import org.eclipse.nebula.widgets.nattable.layer.DataLayer;
import org.eclipse.nebula.widgets.nattable.layer.ILayer;
import org.eclipse.nebula.widgets.nattable.layer.ILayerListener;
import org.eclipse.nebula.widgets.nattable.layer.cell.ILayerCell;
import org.eclipse.nebula.widgets.nattable.layer.event.ILayerEvent;
import org.eclipse.nebula.widgets.nattable.reorder.ColumnReorderLayer;
import org.eclipse.nebula.widgets.nattable.selection.SelectionLayer;
import org.eclipse.nebula.widgets.nattable.selection.config.DefaultSelectionBindings;
import org.eclipse.nebula.widgets.nattable.selection.config.DefaultSelectionLayerConfiguration;
import org.eclipse.nebula.widgets.nattable.selection.event.ISelectionEvent;
import org.eclipse.nebula.widgets.nattable.style.DisplayMode;
import org.eclipse.nebula.widgets.nattable.ui.binding.UiBindingRegistry;
import org.eclipse.nebula.widgets.nattable.ui.matcher.CellEditorMouseEventMatcher;
import org.eclipse.nebula.widgets.nattable.ui.matcher.KeyEventMatcher;
import org.eclipse.nebula.widgets.nattable.ui.matcher.MouseEventMatcher;
import org.eclipse.nebula.widgets.nattable.viewport.ViewportLayer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;

import etablesaw.ui.Activator;
import etablesaw.ui.SimpleTableProvider;
import etablesaw.ui.TableProvider;
import etablesaw.ui.TableProviderHelper;
import etablesaw.ui.editor.commands.TableCellChangeRecorder;
import etablesaw.ui.editor.commands.TableCellChangeRecorderCommandHandler;
import etablesaw.ui.expr.ExprSupport;
import etablesaw.ui.util.MultiCheckSelectionShell;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;

public class NatTablesawViewer implements TableProvider, ISelectionProvider {

    private Table input;

    public void setInput(final Table input) {
        this.input = input;
        boolean onlyTableDataChanged = false;
        if (natTable != null) {
            Table oldTable = bodyDataProvider.getTable();
            if (oldTable != null && (input == oldTable || SimpleTableProvider.onlyTableDataChanged(oldTable, input))) {
                onlyTableDataChanged = true;
            }
            bodyDataProvider.setTable(input);
            final DefaultTablesawColumnLabelAccumulator columnLabelAccumulator = new DefaultTablesawColumnLabelAccumulator(
                    bodyDataLayer, input);
            bodyDataLayer.setConfigLabelAccumulator(columnLabelAccumulator);
            displayConverter.setColumnTypeProvider(bodyDataProvider);
        }
        if (input != null) {
            clearFilter();
        }
        refresh(! onlyTableDataChanged);
    }

    private NatTable natTable;
    private TablesawDataProvider bodyDataProvider;
    private DelegatingTablesawDataProvider columnHeaderDataProvider, rowHeaderDataProvider;
    private DataLayer bodyDataLayer, columnDataLayer;
    private ColumnHideShowLayer columnHideShowLayer;
    private SelectionLayer selectionLayer;
    private AbstractTablesawDisplayConverter displayConverter;
    private CornerLayer cornerLayer;
    private GridLayer gridLayer;

    public SelectionLayer getSelectionLayer() {
        return selectionLayer;
    }
    
    private final int defaultColumnWidth = 60, defaultRowHeight = 20;

    private boolean editable = false;

    public void setEditable(final boolean editable) {
        this.editable = editable;
    }

    private boolean includeFilterRow = true;

    public void setIncludeFilterRow(final boolean includeFilterRow) {
        this.includeFilterRow = includeFilterRow;
    }

    private FilterRowHeaderComposite<Object> filterRowHeaderComposite;
    private IFilterStrategy<Object> filterStrategy;

    private ExprSupport exprSupport = Activator.getInstance().getExprSupport("xaw");

    public void createPartControl(final Composite parent) {
        bodyDataProvider = new TablesawDataProvider(input);
        bodyDataLayer = new TableCellChangeRecorderDataLayer(bodyDataProvider, defaultColumnWidth, defaultRowHeight);
        final ColumnReorderLayer columnReorderLayer = new ColumnReorderLayer(bodyDataLayer);
        columnHideShowLayer = new ColumnHideShowLayer(columnReorderLayer);

        selectionLayer = new SelectionLayer(columnHideShowLayer, false);
        // remove copy action key binding, since we implement it ourselves
        selectionLayer.addConfiguration(new DefaultSelectionLayerConfiguration() {
            @Override
            protected void addSelectionUIBindings() {
                addConfiguration(new DefaultSelectionBindings() {
                    @Override
                    public void configureUiBindings(UiBindingRegistry uiBindingRegistry) {
                        super.configureUiBindings(uiBindingRegistry);
                        // remove copy, since we implement it ourselves
                        uiBindingRegistry.unregisterKeyBinding(new KeyEventMatcher(SWT.MOD1, 'c'));
                    }
                });
            }
        });
        
        final ViewportLayer viewportLayer = new ViewportLayer(selectionLayer);

        columnHeaderDataProvider = new DelegatingTablesawDataProvider(bodyDataProvider, true);
        columnDataLayer = new TableCellChangeRecorderDataLayer(columnHeaderDataProvider, defaultColumnWidth, defaultRowHeight);
        AbstractLayer columnHeaderLayer = new ColumnHeaderLayer(columnDataLayer, viewportLayer, selectionLayer);
        // allow editing column header with double-click
        columnHeaderLayer.addConfiguration(new DefaultEditBindings() {
            @Override
            public void configureUiBindings(final UiBindingRegistry uiBindingRegistry) {
                uiBindingRegistry.registerDoubleClickBinding(
                    new CellEditorMouseEventMatcher(GridRegion.COLUMN_HEADER), new MouseEditAction());
            }
        });

        if (includeFilterRow) {
            // adds a UpdateDataCommandHandler (for the filter string)
            final FilterRowHeaderComposite<Object> filterRowHeaderLayer = filterRowHeaderComposite = new FilterRowHeaderComposite<Object>(
                    filterStrategy = new ExprSupportFilterStrategy<Object>(bodyDataProvider, exprSupport),
                    columnHeaderLayer, columnHeaderDataProvider, null);
            filterRowHeaderLayer.addConfiguration(new DefaultEditConfiguration());
            columnHeaderLayer = filterRowHeaderLayer;
            addTableChangeListener(new TablesawDataProvider.Listener() {
                @Override
                public void providerRowsChanged(final int startRange, final int endRange) {
                    refresh(false);
                }
                @Override
                public void tableCellChanged(final int row, final int column, final Object oldValue, final Object newValue) {
                }
            });
        }
        rowHeaderDataProvider = new DelegatingTablesawDataProvider(bodyDataProvider, false);
        final ILayer rowHeaderLayer = new RowHeaderLayer(
                new DataLayer(rowHeaderDataProvider, defaultColumnWidth, defaultRowHeight), viewportLayer,
                selectionLayer);

        final IDataProvider cornerDataProvider = new DefaultCornerDataProvider(columnHeaderDataProvider,
                rowHeaderDataProvider);
        // adds a UpdateDataCommandHandler (for the row header)
        final DataLayer cornerDataLayer = new TableCellChangeRecorderDataLayer(cornerDataProvider);
        cornerLayer = new CornerLayer(cornerDataLayer, rowHeaderLayer, columnHeaderLayer);
        gridLayer = new GridLayer(viewportLayer, columnHeaderLayer, rowHeaderLayer, cornerLayer);
        natTable = new NatTable(parent, NatTable.DEFAULT_STYLE_OPTIONS | SWT.BORDER, gridLayer, false);
        natTable.addConfiguration(new DefaultNatTableStyleConfiguration());
        configure(natTable);
        natTable.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        
        selectionLayer.addLayerListener(new ILayerListener() {
            @Override
            public void handleLayerEvent(ILayerEvent event) {
                if (event instanceof ISelectionEvent) {
                    fireSelectionChangedEvent();
                }
            }
        });
    }
    
    private class TableCellChangeRecorderDataLayer extends DataLayer {
        public TableCellChangeRecorderDataLayer(IDataProvider dataProvider) {
            super(dataProvider);
        }
        public TableCellChangeRecorderDataLayer(IDataProvider dataProvider, int defaultColumnWidth, int defaultRowHeight) {
            super(dataProvider, defaultColumnWidth, defaultRowHeight);
        }
        public void registerCommandHandler(ILayerCommandHandler<?> commandHandler) {
            if (commandHandler instanceof UpdateDataCommandHandler) {
                TableCellChangeRecorderCommandHandler<UpdateDataCommand> recordingCommandHandler = new TableCellChangeRecorderCommandHandler<UpdateDataCommand>(UpdateDataCommand.class, (UpdateDataCommandHandler) commandHandler);
                recordingCommandHandler.setDataProvider(() -> (AbstractTablesawDataProvider) getDataProvider());
                recordingCommandHandler.setRecorderConsumer(onTableCellChanges);
                commandHandler = recordingCommandHandler;
            }
            super.registerCommandHandler(commandHandler);
        }
    }
    
    public boolean hasActiveCellEditor() {
        return natTable != null && natTable.getActiveCellEditor() != null;
    }

    private Consumer<TableCellChangeRecorder> onTableCellChanges;
    
    public void setOnTableCellChanges(Consumer<TableCellChangeRecorder> onTableCellChanges) {
        this.onTableCellChanges = onTableCellChanges;
    }

    public NatTable getControl() {
        return natTable;
    }

    public TablesawDataProvider getTablesawDataProvider() {
        return bodyDataProvider;
    }
    
    public void clearFilter() {
        if (filterRowHeaderComposite != null) {
            filterRowHeaderComposite.getFilterRowDataLayer().getFilterRowDataProvider().clearAllFilters();
        }
    }

    public void applyFilter() {
        if (filterRowHeaderComposite != null) {
            final Map<Integer, Object> filterIndexToObjectMap = filterRowHeaderComposite.getFilterRowDataLayer()
                    .getFilterRowDataProvider().getFilterIndexToObjectMap();
            filterStrategy.applyFilter(filterIndexToObjectMap);
        }
    }

    protected IEditableRule editableRule = new IEditableRule() {
        @Override
        public boolean isEditable(final int columnIndex, final int rowIndex) {
            return editable;
        }
        @Override
        public boolean isEditable(final ILayerCell cell, final IConfigRegistry configRegistry) {
            return editable;
        }
    };

    protected void configure(final NatTable natTable) {
        final IConfigRegistry configRegistry = natTable.getConfigRegistry();
        configRegistry.registerConfigAttribute(CellConfigAttributes.DISPLAY_CONVERTER,
                new DefaultTablesawDisplayConverter(rowHeaderDataProvider), DisplayMode.NORMAL, GridRegion.ROW_HEADER);
        displayConverter = new DefaultTablesawDisplayConverter(bodyDataProvider);
        configRegistry.registerConfigAttribute(CellConfigAttributes.DISPLAY_CONVERTER,
                displayConverter, DisplayMode.NORMAL, GridRegion.BODY);
        if (includeFilterRow) {
            configRegistry.registerConfigAttribute(CellConfigAttributes.DISPLAY_CONVERTER,
                    new DefaultDisplayConverter(), DisplayMode.NORMAL, "FILTER_ROW");
        }

        MultiCheckSelectionShell columnSelector = new MultiCheckSelectionShell(getControl()) {
            protected void createExtraControls(Composite parent) {
                NatTablesawViewer.this.createExtraPopupControls(parent);
            }
        };
        columnSelector.setTitle("Show/hide columns");
        columnSelector.setLocationFactors(0.0f,  0.0f);
        columnSelector.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                bodyDataProvider.setColumnNames(columnSelector.getSelections());
                // clear, since otherwise the filters may end up in the wrong column, since they're index-based
                clearFilter();
                refresh(true);
            }
        });

        gridLayer.addConfiguration(new AbstractUiBindingConfiguration() {
            @Override
            public void configureUiBindings(UiBindingRegistry uiBindingRegistry) {
                uiBindingRegistry.registerFirstMouseDownBinding(new MouseEventMatcher(SWT.NONE, GridRegion.CORNER, 1), (natTable, event) -> {
                    List<String> columnNames = input.columnNames();
                    columnSelector.setItems(columnNames.toArray(new String[columnNames.size()]));
                    for (int i = 0; i < columnNames.size(); i++) {
                        if (bodyDataProvider.hasColumn(columnNames.get(i))) {
                            columnSelector.select(i);
                        }
                    }
                    columnSelector.openShell();
                });
            }
        });
        gridLayer.addConfiguration(new AbstractRegistryConfiguration() {            
            @Override
            public void configureRegistry(IConfigRegistry configRegistry) {
              configRegistry.registerConfigAttribute(EditConfigAttributes.CELL_EDITABLE_RULE, editableRule);
            }
        });

        if (editable && includeFilterRow) {
            UpdateDataExprCommandHandler commandHandler = new UpdateDataExprCommandHandler(exprSupport, bodyDataProvider, tableCellChangeRecorder -> onTableCellChanges.accept(tableCellChangeRecorder)) {
                @Override
                protected int getColumnNum(ILayer targetLayer, int columnPos) {
                    return gridLayer.getColumnIndexByPosition(columnPos);
                }
            };
            // will override the one registered in the layer below
            filterRowHeaderComposite.registerCommandHandler(commandHandler);
        }
        natTable.configure();
    }

    protected void createExtraPopupControls(Composite parent) {
    }
    
    public void refresh(Boolean fireTableChanged) {
        if (natTable != null) {
            natTable.refresh();
            if (fireTableChanged == null);
            else if (fireTableChanged) {
                getTableProviderHelper().fireTableChanged(getTableProvider());
            } else {
                getTableProviderHelper().fireTableDataChanged(getTableProvider());
            }
        }
    }

    protected TableProvider getTableProvider() {
        return this;
    }

    //

    public void addTableChangeListener(final TablesawDataProvider.Listener listener) {
        bodyDataProvider.addTableChangeListener(listener);
    }

    public void removeTableChangeListener(final TablesawDataProvider.Listener listener) {
        bodyDataProvider.removeTableChangeListener(listener);
    }

    // TableProvider

    @Override
    public Table getTable() {
        Table table = null;
        final Table dataTable = bodyDataProvider.getDataTable();
        if (dataTable != null) {
            table = Table.create(dataTable != null ? dataTable.name() : "<no data>");
            final Collection<Column<?>> columns = new ArrayList<Column<?>>();
            final Collection<Integer> rowNums = getRows();
            Collection<String> columnNames = bodyDataProvider.getColumnNames();
            for (int colNum = 0; colNum < columnNames.size(); colNum++) {
                final Column<?> modelColumn = bodyDataProvider.getColumn(colNum);
                final Column<?> column = modelColumn.emptyCopy();
                for (final int rowNum : rowNums) {
                    final Object element = modelColumn.get(rowNum);
                    ((Column<Object>) column).append(element);
                }
                columns.add(column);
            }
            table.addColumns(columns.toArray(new Column<?>[columns.size()]));
        }
        return table;
    }

    protected final Collection<Integer> getRows() {
        final Table dataTable = bodyDataProvider.getDataTable();
        final Collection<Integer> rows = new ArrayList<>();
        for (int i = 0; i < dataTable.rowCount(); i++) {
            rows.add(i);
        }
        return rows;
    }

    private final TableProviderHelper tableProviderHelper = new TableProviderHelper();

    @Override
    public void addTableDataProviderListener(final TableProvider.Listener listener) {
        tableProviderHelper.addTableDataProviderListener(listener);
    }

    @Override
    public void removeTableDataProviderListener(final TableProvider.Listener listener) {
        tableProviderHelper.removeTableDataProviderListener(listener);
    }

    public TableProviderHelper getTableProviderHelper() {
        return tableProviderHelper;
    }

    // ISelectionProvider

    @Override
    public void setSelection(final ISelection selection) {
        Collection<Rectangle> rectangles = new ArrayList<>();
        if (selection instanceof IStructuredSelection) {
            Iterator<?> it = ((IStructuredSelection) selection).iterator();
            while (it.hasNext()) {
                Object next = it.next();
                if (next instanceof Rectangle) {
                    rectangles.add((Rectangle) next);
                }
            }
        }
        selectionLayer.getSelectionModel().clearSelection();
        for (Rectangle rectangle : rectangles) {
            selectionLayer.getSelectionModel().addSelection(rectangle);
        }
    }

    @Override
    public ISelection getSelection() {
        return new StructuredSelection(selectionLayer.getSelectionModel().getSelections());
    }

    protected void fireSelectionChangedEvent() {
        if (selectionListeners != null && (! selectionListeners.isEmpty())) {
            SelectionChangedEvent event = new SelectionChangedEvent(this, getSelection());
            for (final ISelectionChangedListener selectionChangedListener : selectionListeners) {
                selectionChangedListener.selectionChanged(event);
            }
        }
    }

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
}

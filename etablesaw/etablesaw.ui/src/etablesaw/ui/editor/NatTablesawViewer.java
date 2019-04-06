package etablesaw.ui.editor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.nebula.widgets.nattable.NatTable;
import org.eclipse.nebula.widgets.nattable.config.AbstractUiBindingConfiguration;
import org.eclipse.nebula.widgets.nattable.config.CellConfigAttributes;
import org.eclipse.nebula.widgets.nattable.config.DefaultNatTableStyleConfiguration;
import org.eclipse.nebula.widgets.nattable.config.IConfigRegistry;
import org.eclipse.nebula.widgets.nattable.config.IEditableRule;
import org.eclipse.nebula.widgets.nattable.data.IDataProvider;
import org.eclipse.nebula.widgets.nattable.data.convert.DefaultDisplayConverter;
import org.eclipse.nebula.widgets.nattable.edit.EditConfigAttributes;
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
import org.eclipse.nebula.widgets.nattable.layer.DataLayer;
import org.eclipse.nebula.widgets.nattable.layer.ILayer;
import org.eclipse.nebula.widgets.nattable.layer.LabelStack;
import org.eclipse.nebula.widgets.nattable.layer.cell.ILayerCell;
import org.eclipse.nebula.widgets.nattable.reorder.ColumnReorderLayer;
import org.eclipse.nebula.widgets.nattable.selection.SelectionLayer;
import org.eclipse.nebula.widgets.nattable.style.DisplayMode;
import org.eclipse.nebula.widgets.nattable.ui.binding.UiBindingRegistry;
import org.eclipse.nebula.widgets.nattable.ui.matcher.MouseEventMatcher;
import org.eclipse.nebula.widgets.nattable.viewport.ViewportLayer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;

import etablesaw.ui.Activator;
import etablesaw.ui.TableProvider;
import etablesaw.ui.TableProviderHelper;
import etablesaw.ui.util.MultiCheckSelectionShell;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;

public class NatTablesawViewer implements TableProvider, ISelectionProvider {

    private Table input;

    public void setInput(final Table input) {
        this.input = input;
        if (natTable != null) {
            bodyDataProvider.setTable(input);
            columnHeaderDataProvider.setTable(input);
            rowHeaderDataProvider.setTable(input);
            final DefaultTablesawColumnLabelAccumulator columnLabelAccumulator = new DefaultTablesawColumnLabelAccumulator(
                    bodyDataLayer, input);
            bodyDataLayer.setConfigLabelAccumulator(columnLabelAccumulator);
            displayConverter.setColumnTypeProvider(bodyDataProvider);
        }
        if (input != null) {
            applyFilter();
        }
        refresh();
    }

    private NatTable natTable;
    private TablesawDataProvider bodyDataProvider, columnHeaderDataProvider, rowHeaderDataProvider;
    private DataLayer bodyDataLayer, columnDataLayer;
    private ColumnHideShowLayer columnHideShowLayer;
    private SelectionLayer selectionLayer;
    private AbstractTablesawDisplayConverter displayConverter;
    private CornerLayer cornerLayer;
    private GridLayer gridLayer;

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

    public void createPartControl(final Composite parent) {
        bodyDataProvider = new TablesawDataProvider(input);
        bodyDataLayer = new DataLayer(bodyDataProvider, defaultColumnWidth, defaultRowHeight);
        rowHeaderDataProvider = new TablesawDataProvider(input, false);
        final ColumnReorderLayer columnReorderLayer = new ColumnReorderLayer(bodyDataLayer);
        columnHideShowLayer = new ColumnHideShowLayer(columnReorderLayer);
        selectionLayer = new SelectionLayer(columnHideShowLayer);
        final ViewportLayer viewportLayer = new ViewportLayer(selectionLayer);

        columnHeaderDataProvider = new TablesawDataProvider(input, true);
        columnDataLayer = new DataLayer(columnHeaderDataProvider, defaultColumnWidth, defaultRowHeight);
        ILayer columnHeaderLayer = new ColumnHeaderLayer(columnDataLayer, viewportLayer, selectionLayer);

        if (includeFilterRow) {
            final FilterRowHeaderComposite<Object> filterRowHeaderLayer = filterRowHeaderComposite = new FilterRowHeaderComposite<Object>(
                    filterStrategy = new ExprSupportFilterStrategy<Object>(bodyDataProvider, Activator.getInstance().getExprSupports("xaw")),
                    columnHeaderLayer, columnHeaderDataProvider, null);
            filterRowHeaderLayer.addConfiguration(new DefaultEditConfiguration());
            columnHeaderLayer = filterRowHeaderLayer;
            addTableChangeListener(new TablesawDataProvider.Listener() {
                @Override
                public void rowsChanged(final int startRange, final int endRange) {
                    natTable.refresh();
                    getTableProviderHelper().fireTableDataChanged();
                }
                @Override
                public void cellChanged(final int row, final int column, final Object oldValue, final Object newValue) {
                }
            });
        }
        final ILayer rowHeaderLayer = new RowHeaderLayer(
                new DataLayer(rowHeaderDataProvider, defaultColumnWidth, defaultRowHeight), viewportLayer,
                selectionLayer);

        final IDataProvider cornerDataProvider = new DefaultCornerDataProvider(columnHeaderDataProvider,
                rowHeaderDataProvider);
        final DataLayer cornerDataLayer = new DataLayer(cornerDataProvider);
        cornerLayer = new CornerLayer(cornerDataLayer, rowHeaderLayer, columnHeaderLayer);
        gridLayer = new GridLayer(viewportLayer, columnHeaderLayer, rowHeaderLayer, cornerLayer);
        natTable = new NatTable(parent, NatTable.DEFAULT_STYLE_OPTIONS | SWT.BORDER, gridLayer, false);
        natTable.addConfiguration(new DefaultNatTableStyleConfiguration());
        configure(natTable);
        natTable.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
    }

    public NatTable getControl() {
        return natTable;
    }

    public void applyFilter() {
        if (filterRowHeaderComposite != null) {
            final Map<Integer, Object> filterIndexToObjectMap = filterRowHeaderComposite.getFilterRowDataLayer()
                    .getFilterRowDataProvider().getFilterIndexToObjectMap();
            filterStrategy.applyFilter(filterIndexToObjectMap);
        }
    }

    protected void configure(final NatTable natTable) {
        final IConfigRegistry configRegistry = natTable.getConfigRegistry();
        configRegistry.registerConfigAttribute(CellConfigAttributes.DISPLAY_CONVERTER,
                new DefaultTablesawDisplayConverter(rowHeaderDataProvider), DisplayMode.NORMAL, GridRegion.ROW_HEADER);
        displayConverter = new DefaultTablesawDisplayConverter(bodyDataProvider);
        configRegistry.registerConfigAttribute(CellConfigAttributes.DISPLAY_CONVERTER,
                displayConverter, DisplayMode.NORMAL, GridRegion.BODY);
        configRegistry.registerConfigAttribute(EditConfigAttributes.CELL_EDITABLE_RULE, new IEditableRule() {
            @Override
            public boolean isEditable(final int columnIndex, final int rowIndex) {
                return editable;
            }
            @Override
            public boolean isEditable(final ILayerCell cell, final IConfigRegistry configRegistry) {
                return editable;
            }
        });
        if (includeFilterRow) {
            configRegistry.registerConfigAttribute(CellConfigAttributes.DISPLAY_CONVERTER,
                    new DefaultDisplayConverter(), DisplayMode.NORMAL, "FILTER_ROW");
        }

        MultiCheckSelectionShell columnSelector = new MultiCheckSelectionShell(getControl());
        columnSelector.setTitle("Show/hide columns");
        columnSelector.setLocationFactors(0.0f,  0.0f);
        columnSelector.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                bodyDataProvider.setColumnNames(columnSelector.getSelections());
                columnHeaderDataProvider.setColumnNames(columnSelector.getSelections());
                refresh();
//                Collection<Integer> columnPositions = new ArrayList<>();
//                int[] indices = columnSelector.getSelectionIndices();
//                int pos = 0, itemCount = columnSelector.getItemCount();
//                for (int i = 0; i < itemCount; i++) {
//                    if (pos < indices.length && indices[pos] == i) {
//                        pos++;
//                    } else {
//                        columnPositions.add(i);
//                    }
//                }
//                columnHideShowLayer.showAllColumns();
//                columnHideShowLayer.hideColumnPositions(columnPositions);
            }
        });

        gridLayer.addConfiguration(new AbstractUiBindingConfiguration() {
            @Override
            public void configureUiBindings(UiBindingRegistry uiBindingRegistry) {
                uiBindingRegistry.registerFirstMouseDownBinding(new MouseEventMatcher(SWT.NONE, GridRegion.CORNER, 1) {
                    @Override
                    public boolean matches(NatTable natTable, MouseEvent event, LabelStack regionLabels) {
                        return super.matches(natTable, event, regionLabels);
                    }
                }, (natTable, event) -> {
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
        natTable.configure();
    }

    public void refresh() {
        natTable.refresh();
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
        // not sure this makes much sense
    }

    @Override
    public ISelection getSelection() {
        return new StructuredSelection(selectionLayer.getSelectedCells());
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
}

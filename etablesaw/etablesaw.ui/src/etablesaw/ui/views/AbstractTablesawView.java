package etablesaw.ui.views;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.Supplier;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;

import etablesaw.ui.Activator;
import etablesaw.ui.TableProvider;
import etablesaw.ui.TableProviderRegistry;
import etablesaw.ui.util.MultiCheckSelectionCombo;
import tech.tablesaw.aggregate.AggregateFunction;
import tech.tablesaw.aggregate.AggregateFunctions;
import tech.tablesaw.api.ColumnType;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;

public abstract class AbstractTablesawView extends ViewPart implements TableProvider.Listener {


	protected AbstractTablesawView(final boolean autoSelectTableDataProvider) {
		this.autoSelectTableDataProvider = autoSelectTableDataProvider;
	}

	@Override
	public void init(final IViewSite site) throws PartInitException {
		super.init(site);
		setAutoSelectTableDataProvider(autoSelectTableDataProvider);
	}

	private Composite tableViewerParent;

	protected void createTableDataControls(final Composite parent) {
		tableViewerParent = parent;
	}

	protected Composite getTableViewerParent() {
		return tableViewerParent;
	}

	@Override
	public void dispose() {
		setAutoSelectTableDataProvider(false);
		super.dispose();
	}

	private boolean autoSelectTableDataProvider = true;

	public void setAutoSelectTableDataProvider(final boolean autoSelectTableDataProvider) {
		if (this.autoSelectTableDataProvider) {
			getSite().getPage().removePartListener(partListener);
		}
		this.autoSelectTableDataProvider = autoSelectTableDataProvider;
		if (this.autoSelectTableDataProvider) {
			getSite().getPage().addPartListener(partListener);
			setTableDataProvider(getSite().getWorkbenchWindow().getActivePage().getActiveEditor());
		}
	}

	private TableProvider tableProvider;
	private Table viewTable;

	public Table getViewTable() {
		if (viewTable == null && tableProvider != null) {
			viewTable = tableProvider.getTable();
		}
		return viewTable;
	}

	@Override
	public void tableDataChanged(final TableProvider tableProvider) {
		// clear cache
		this.viewTable = null;
		updateTableControls();
	}

	@Override
	public void tableChanged(final TableProvider tableProvider) {
		this.viewTable = null;
		updateConfigControls();
		updateTableControls();
	}

	protected void setTableDataProvider(final IWorkbenchPart part) {
		if (part instanceof TableProvider) {
			setTableProvider((TableProvider) part);
		}
	}

	protected void setTableProvider(final TableProvider tableProvider) {
		if (this.tableProvider == tableProvider) {
			return;
		}
		if (this.tableProvider != null) {
			this.tableProvider.removeTableDataProviderListener(this);
		}
		this.tableProvider = tableProvider;
		this.viewTable = null;
		updateView();
		if (this.tableProvider != null) {
			this.tableProvider.addTableDataProviderListener(this);
		}
	}

	@Override
	public void createPartControl(final Composite parent) {
		final GridLayout layout = new GridLayout(1, false);
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		layout.verticalSpacing = 0;
		layout.horizontalSpacing = 0;
		parent.setLayout(layout);

		final Composite configParent = new Composite(parent, SWT.NONE);
		final GridLayout configLayout = new GridLayout(2, false);
		configParent.setLayout(configLayout);
		setDistinctPartName();
		createConfigControls(configParent);
		createTableDataControls(parent);
		updateView();
		if (this instanceof TableProvider) {
			Activator.getInstance().getTableProviderRegistry().registerTableProvider(getPartName(), (TableProvider) this);
		}
		try {
			updateViewAction = new Action("Refresh", ImageDescriptor.createFromURL(new URL("platform:/plugin/org.eclipse.search/icons/full/elcl16/refresh.png"))) {
				@Override
				public void run() {
					updateTableControls();
				}
			};
			spawnViewAction = new Action("Spawn", ImageDescriptor.createFromURL(new URL("platform:/plugin/org.eclipse.ui.views/icons/full/elcl16/new.png"))) {
				@Override
				public void run() {
					final IWorkbenchPage page = getSite().getPage();
					final String id = getSite().getId();
					try {
						page.showView(id, String.valueOf(page.getViewReferences().length), IWorkbenchPage.VIEW_ACTIVATE);
					} catch (final PartInitException e) {
						System.out.println(e);
					}
				}
			};
			getViewSite().getActionBars().getToolBarManager().add(updateViewAction);
			getViewSite().getActionBars().getToolBarManager().add(spawnViewAction);
		} catch (final MalformedURLException e) {
		}
		//		parent.addControlListener(new ControlAdapter() {
		//			@Override
		//			public void controlResized(final ControlEvent e) {
		//				parent.layout();
		//			}
		//		});
	}

	private void setDistinctPartName() {
		final String partName = getPartName();
		if (partName.indexOf('#') < 0) {
			int pos = 1;
			for (final IViewReference viewRef : getSite().getPage().getViewReferences()) {
				if (viewRef.getPart(false) == this) {
					break;
				}
				pos++;
			}
			setPartName(partName + " # " + pos);
		}
	}

	protected void createConfigControls(final Composite configParent) {
		createTableRegistrySelector("Source: ", configParent, null);
	}

	private final IAction selectTableProviderAction = new Action("Source table", IAction.AS_DROP_DOWN_MENU) {
	};

	protected void addSelectTableProviderAction() {
		final IMenuManager menuManager = getViewSite().getActionBars().getMenuManager();
		final ActionContributionItem contributionItem = new ActionContributionItem(selectTableProviderAction);
		menuManager.add(contributionItem);
	}

	@Override
	public void setFocus() {
	}

	protected ComboViewer createTableProviderSelector(final String label, final Composite parent) {
		if (label != null) {
			createControlLabel(parent, label);
		}
		final ComboViewer viewer = new ComboViewer(parent);
		viewer.getControl().getDisplay().asyncExec(new Runnable() {
			@Override
			public void run() {
				final Object[] elements = ((IStructuredContentProvider) viewer.getContentProvider()).getElements(viewer.getInput());
				if (elements.length > 0) {
					viewer.setSelection(new StructuredSelection(elements[0]));
				}
			}
		});
		return viewer;
	}

	protected void createWorkbenchTableProviderSelector(final String label, final Composite parent) {
		final ComboViewer viewer = createTableProviderSelector(label, parent);
		final IStructuredContentProvider contentProvider = new IStructuredContentProvider() {
			@Override
			public Object[] getElements(final Object inputElement) {
				final Collection<TableProvider> tableDataProviders = new ArrayList<TableProvider>();
				if (inputElement instanceof IWorkbenchPage) {
					for (final IEditorReference editorReference : ((IWorkbenchPage) inputElement).getEditorReferences()) {
						final IEditorPart editorPart = editorReference.getEditor(false);
						if (editorPart instanceof TableProvider) {
							tableDataProviders.add((TableProvider) editorPart);
						}
					}
					for (final IViewReference viewReference : ((IWorkbenchPage) inputElement).getViewReferences()) {
						final IViewPart viewPart = viewReference.getView(false);
						if (viewPart instanceof TableProvider) {
							tableDataProviders.add((TableProvider) viewPart);
						}
					}
				}
				return tableDataProviders.toArray();
			}
		};
		viewer.setContentProvider(contentProvider);
		viewer.setLabelProvider(new LabelProvider() {
			@Override
			public String getText(final Object element) {
				if (element instanceof IWorkbenchPart) {
					return ((IWorkbenchPart) element).getTitle();
				}
				return null;
			}
		});
		viewer.addSelectionChangedListener(new ISelectionChangedListener() {
			@Override
			public void selectionChanged(final SelectionChangedEvent event) {
				tableProviderChanged((TableProvider) viewer.getStructuredSelection().getFirstElement());
			}
		});
		final IPartListener partListener = new IPartListener() {
			private void refreshViewer(final Viewer viewer) {
				if (! viewer.getControl().isDisposed()) {
					viewer.refresh();
				}
			}
			@Override
			public void partOpened(final IWorkbenchPart part) {
				refreshViewer(viewer);
			}
			@Override
			public void partDeactivated(final IWorkbenchPart part) {
			}
			@Override
			public void partClosed(final IWorkbenchPart part) {
				refreshViewer(viewer);
			}
			@Override
			public void partBroughtToTop(final IWorkbenchPart part) {
			}
			@Override
			public void partActivated(final IWorkbenchPart part) {
			}
		};
		final IWorkbenchPage page = getSite().getPage();
		page.addPartListener(partListener);
		viewer.getControl().addDisposeListener(new DisposeListener() {
			@Override
			public void widgetDisposed(final DisposeEvent e) {
				page.removePartListener(partListener);
			}
		});
		viewer.setInput(page);
	}

	protected void createTableRegistrySelector(final String label, final Composite parent, final TableProvider except) {
		final ComboViewer viewer = createTableProviderSelector(label, parent);
		final IStructuredContentProvider contentProvider = new IStructuredContentProvider() {
			@Override
			public Object[] getElements(final Object inputElement) {
				final Collection<String> tableDataProviders = new ArrayList<String>();
				if (inputElement instanceof TableProviderRegistry) {
					final TableProviderRegistry tableProviderRegistry = (TableProviderRegistry) inputElement;
					final Collection<String> tableProviderKeys = tableProviderRegistry.getTableProviderKeys();
					tableDataProviders.addAll(tableProviderKeys);
					tableDataProviders.remove(tableProviderRegistry.getTableProviderKey(except));
				}
				return tableDataProviders.toArray();
			}
		};
		viewer.setContentProvider(contentProvider);
		viewer.setLabelProvider(new LabelProvider() {
			@Override
			public String getText(final Object element) {
				return String.valueOf(element);
			}
		});
		viewer.addSelectionChangedListener(new ISelectionChangedListener() {
			@Override
			public void selectionChanged(final SelectionChangedEvent event) {
				final Object firstElement = viewer.getStructuredSelection().getFirstElement();
				tableProviderChanged(Activator.getInstance().getTableProviderRegistry().getTableProvider(String.valueOf(firstElement)));
				updateTableControls();
			}
		});
		final TableProviderRegistry.Listener registryListener = new TableProviderRegistry.Listener() {
			@Override
			public void tableProviderRegistryChanged(final String key, final TableProvider tableProvider) {
				if (! viewer.getControl().isDisposed()) {
					viewer.refresh();
				}
			}
		};
		Activator.getInstance().getTableProviderRegistry().addTableRegistryChangedListener(registryListener);
		viewer.setInput(Activator.getInstance().getTableProviderRegistry());
	}

	protected class TableRegistryMenuCreator extends SelectionAdapter implements IMenuCreator, SelectionListener {

		private Menu menu = null;

		@Override
		public void widgetSelected(final SelectionEvent e) {
			final String key = ((MenuItem) e.widget).getText();
			final TableProviderRegistry tableProviderRegistry = Activator.getInstance().getTableProviderRegistry();
			final TableProvider tableProvider = tableProviderRegistry.getTableProvider(key);
			if (tableProvider != null) {
				System.out.println("Selecting " + key);
				tableProviderChanged(tableProvider);
			}
		}

		@Override
		public Menu getMenu(final Menu parent) {
			System.out.println(Activator.getInstance().getTableProviderRegistry().getTableProviderKeys());
			if (menu == null) {
				menu = createMenu(parent);
			}
			return menu;
		}

		public Menu createMenu(final Menu parent) {
			final Menu menu = new Menu(parent);
			addMenuItems(menu);
			return menu;
		}

		public void addMenuItems(final Menu menu) {
			for (final String key : Activator.getInstance().getTableProviderRegistry().getTableProviderKeys()) {
				final MenuItem menuItem = new MenuItem(menu, SWT.RADIO);
				menuItem.setText(key);
				menuItem.addSelectionListener(this);
			}
		}

		@Override
		public Menu getMenu(final Control parent) {
			return null;
		}

		@Override
		public void dispose() {
			if (menu != null) {
				menu.dispose();
			}
			menu = null;
		}
	}

	protected Action updateViewAction;
	protected Action spawnViewAction;

	protected void tableProviderChanged(final TableProvider tableDataProvider) {
		setTableProvider(tableDataProvider);
	}

	private final IPartListener partListener = new IPartListener() {

		@Override
		public void partOpened(final IWorkbenchPart part) {
		}
		@Override
		public void partClosed(final IWorkbenchPart part) {
			if (part == tableProvider) {
				setTableProvider((TableProvider) null);
			}
		}

		@Override
		public void partBroughtToTop(final IWorkbenchPart part) {
			if (autoSelectTableDataProvider) {
				setTableDataProvider(part);
			}
		}
		@Override
		public void partActivated(final IWorkbenchPart part) {
		}
		@Override
		public void partDeactivated(final IWorkbenchPart part) {
			if (part == tableProvider) {
				setTableProvider((TableProvider) null);
			}
		}
	};

	//

	protected void updateView() {
		viewTable = (tableProvider != null ? tableProvider.getTable() : null);
		updateConfigControls();
		updateTableControls();
	}

	protected final String noColumn = "<none>";

	protected StructuredViewer createColumnSelector(final String label, final Composite parent, final Boolean mode, final Class<?> columnClass) {
		createControlLabel(parent, label);
		final boolean multi = Boolean.TRUE.equals(mode);
		final StructuredViewer selector = (multi ? new ListViewer(parent) : new ComboViewer(parent));
		selector.setContentProvider(new IStructuredContentProvider() {
			@Override
			public Object[] getElements(final Object inputElement) {
				if (inputElement instanceof Table) {
					final Table table = (Table) inputElement;
					return getColumnNames(table, columnClass);
				}
				return null;
			}
		});
		selector.addSelectionChangedListener(new ISelectionChangedListener() {
			@Override
			public void selectionChanged(final SelectionChangedEvent event) {
				configControlUpdated();
			}
		});
		selector.setInput(getViewTable());
		final GridData gridData = new GridData(SWT.FILL, SWT.CENTER, true, false);
		if (multi) {
			gridData.heightHint = 60;
		}
		selector.getControl().setLayoutData(gridData);
		return selector;
	}

	protected void setColumnNames(final StructuredViewer columnSelector, final Table table) {
		columnSelector.setInput(table);
	}

	protected String getSelectedColumnName(final StructuredViewer columnSelector) {
		return (String) columnSelector.getStructuredSelection().getFirstElement();
	}

	final static String[] noStrings = new String[0];

	protected void configControlUpdated() {
		updateTableControls();
	}

	protected Control createColumnControl(final Composite parent, final String label, final Boolean mode, final Class<?> columnClass) {
		createControlLabel(parent, label);
		final boolean multi = Boolean.TRUE.equals(mode);
		final Supplier<String[]> itemsProvider = () -> {
			final Table table = getViewTable();
			if (table == null) {
				return noStrings;
			}
			return getColumnNames(table, columnClass);
		};
		final SelectionListener selectionListener = new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
			    configControlUpdated();
			}
		};
		Control control;
		if (multi) {
			final MultiCheckSelectionCombo combo = new MultiCheckSelectionCombo(parent, SWT.NONE);
			combo.setItemsProvider(itemsProvider);
			combo.addSelectionListener(selectionListener);
			control = combo;
		} else {
			final Combo combo = new Combo(parent, SWT.READ_ONLY);
			combo.setItems(itemsProvider.get());
			combo.addSelectionListener(selectionListener);
			control = combo;
		}
		final GridData gridData = new GridData(SWT.FILL, SWT.CENTER, true, false);
		control.setLayoutData(gridData);
		return control;
	}

    protected void createControlLabel(final Composite parent, final String label) {
        final Label swtLabel = new Label(parent, SWT.NONE);
		swtLabel.setText(label);
    }

	public String[] getColumnNames(final Table table, final Class<?> columnClass) {
		final Collection<String> columnNames = new ArrayList<>();
		for (final Column<?> column : table.columns()) {
			if (columnClass == null || columnClass.isInstance(column)) {
				columnNames.add(column.name());
			}
		}
		return columnNames.toArray(new String[columnNames.size()]);
	}

	protected void setColumnNames(final Control columnCombo, final Table table) {
		final String[] columnNames = (table != null ? table.columnNames().toArray(noStrings) : noStrings);
		if (columnCombo instanceof MultiCheckSelectionCombo) {
			final MultiCheckSelectionCombo multiCheckSelectionCombo = (MultiCheckSelectionCombo) columnCombo;
			multiCheckSelectionCombo.setItems(columnNames, true);
		} else if (columnCombo instanceof Combo) {
			((Combo) columnCombo).setItems(columnNames);
		}
	}
	protected String[] getSelectedStrings(final Control selector) {
		if (selector instanceof MultiCheckSelectionCombo) {
			return ((MultiCheckSelectionCombo) selector).getSelections();
		} else if (selector instanceof Combo) {
			final Combo combo = (Combo) selector;
			final int selectionIndex = combo.getSelectionIndex();
			return (selectionIndex >= 0 && selectionIndex < combo.getItemCount() ? new String[]{combo.getItem(selectionIndex)} : new String[0]);
		}
		return null;
	}

	protected int[] getSelectedIndices(final Control columnCombo) {
		if (columnCombo instanceof MultiCheckSelectionCombo) {
			return ((MultiCheckSelectionCombo) columnCombo).getSelectionIndices();
		} else if (columnCombo instanceof Combo) {
			final Combo combo = (Combo) columnCombo;
			return new int[]{combo.getSelectionIndex()};
		}
		return null;
	}

	private final List<AggregateFunction<?, ?>> aggregateFunctions = new ArrayList<AggregateFunction<?,?>>();
	{
		for (final Field field : AggregateFunctions.class.getFields()) {
			final int modifiers = field.getModifiers();
			if (Modifier.isPublic(modifiers) && Modifier.isStatic(modifiers) && AggregateFunction.class.isAssignableFrom(field.getType())) {
				try {
					aggregateFunctions.add((AggregateFunction<?, ?>) field.get(null));
				} catch (final IllegalArgumentException e) {
				} catch (final IllegalAccessException e) {
				}
			}
		}
	}

	protected Control createAggregateFunctionSelector(final Composite parent, final String label, final boolean multi) {
		createControlLabel(parent, label);
		final Collection<String> items = new ArrayList<>();
		for (final AggregateFunction<?, ?> aggregateFunction : aggregateFunctions) {
			items.add(aggregateFunction.functionName());
		}
		final SelectionListener selectionListener = new SelectionListener() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				configControlUpdated();
			}
			@Override
			public void widgetDefaultSelected(final SelectionEvent e) {
			}
		};
		Control control;
		final String[] itemsArray = items.toArray(new String[items.size()]);
		if (multi) {
			final MultiCheckSelectionCombo combo = new MultiCheckSelectionCombo(parent, SWT.NONE);
			combo.setItems(itemsArray);
			combo.addSelectionListener(selectionListener);
			control = combo;
		} else {
			final Combo combo = new Combo(parent, SWT.NONE);
			combo.setItems(itemsArray);
			combo.addSelectionListener(selectionListener);
			control = combo;
		}
		final GridData gridData = new GridData(SWT.FILL, SWT.CENTER, true, false);
		control.setLayoutData(gridData);
		return control;
	}

	protected AggregateFunction<?,?>[] getAggregateFunctions(final Control aggregateFunctionSelector, final ColumnType... columnTypes) {
		final int[] aggregateFunctionIndices = getSelectedIndices(aggregateFunctionSelector);
		final Collection<AggregateFunction<?, ?>> funs = new ArrayList<>();
		outer: for (int i = 0; i < aggregateFunctionIndices.length; i++) {
			final AggregateFunction<?, ?> fun = aggregateFunctions.get(aggregateFunctionIndices[i]);
			if (columnTypes != null) {
				for (int j = 0; j < columnTypes.length; j++) {
					if (! fun.isCompatibleColumn(columnTypes[j])) {
						continue outer;
					}
				}
			}
			funs.add(fun);
		}
		return funs.toArray(new AggregateFunction<?, ?>[funs.size()]);
	}
	protected AggregateFunction<?,?>[] getAggregateFunctions(final Control aggregateFunctionSelector, final Table table, final String... columnNames) {
        final ColumnType[] columnTypes = new ColumnType[columnNames != null ? columnNames.length : 0];
        for (int i = 0; i < columnTypes.length; i++) {
            columnTypes[i] = table.column(columnNames[i]).type();
        }
	    return getAggregateFunctions(aggregateFunctionSelector, columnTypes);
	}

	protected <T extends Number> Control createNumbericParameterControl(Composite parent, String label, Class<T> numClass, T def) {
        createControlLabel(parent, label);
        Text paramText = new Text(parent, SWT.BORDER);
        paramText.setText(String.valueOf(def));
        paramText.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(ModifyEvent e) {
                try {
                    Method valueOfMethod = numClass.getMethod("valueOf", String.class);
                    valueOfMethod.invoke(null, paramText.getText());
                    paramText.setForeground(null);
                } catch (Exception e1) {
                    paramText.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_RED));
                }
            }
        });
        paramText.addFocusListener(new FocusAdapter() {
            @Override
            public void focusLost(FocusEvent e) {
                updateTableControls();
            }
        });
        return paramText;
	}
	
	@SuppressWarnings("unchecked")
    protected <T extends Number> T getNumbericParameter(Control control, Class<T> numClass, T def) {
        try {
            Method getTextMethod = control.getClass().getMethod("getText");
            Method valueOfMethod = numClass.getMethod("valueOf", String.class);
            return (T) valueOfMethod.invoke(null, getTextMethod.invoke(control));
        } catch (Exception e1) {
            return def;
        }
	}
	
	protected abstract void updateTableControls();

	protected void updateConfigControls() {
	}
}

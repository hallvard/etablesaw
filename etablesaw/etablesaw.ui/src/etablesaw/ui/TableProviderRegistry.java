package etablesaw.ui;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

public class TableProviderRegistry extends TableProviderHelper {

	void clear() {
		for (final TableProvider tableProvider : tableProviderMap.values()) {
			tableProvider.removeTableDataProviderListener(tableProviderListener);
		}
		tableProviderMap.clear();
		registryChangedListeners.clear();
	}

	private final TableProvider.Listener tableProviderListener = new TableProvider.Listener() {

		@Override
		public void tableDataChanged(final TableProvider tableProvider) {
			fireTableDataChanged(tableProvider);
		}

		@Override
		public void tableChanged(final TableProvider tableProvider) {
			fireTableChanged(tableProvider);
		}
	};

	private final Map<String, TableProvider> tableProviderMap = new HashMap<String, TableProvider>();

	public void registerTableProvider(final TableProvider tableProvider) {
		registerTableProvider(tableProvider.getClass().getName(), tableProvider);
	}

	public void registerTableProvider(final String key, final TableProvider tableProvider) {
		if (tableProviderMap.containsKey(key)) {
			tableProviderMap.get(key).removeTableDataProviderListener(tableProviderListener);
		}
		if (tableProvider == null) {
			tableProviderMap.remove(key);
			fireTableProviderRegistryChanged(key, null);
		} else {
			tableProviderMap.put(key, tableProvider);
			tableProvider.addTableDataProviderListener(tableProviderListener);
			fireTableProviderRegistryChanged(key, tableProvider);
		}
	}

	public Collection<String> getTableProviderKeys() {
		return new ArrayList<>(tableProviderMap.keySet());
	}

	public TableProvider getTableProvider(final String key) {
		return tableProviderMap.get(key);
	}

	public String getTableProviderKey(final TableProvider tableProvider) {
		for (final Map.Entry<String, TableProvider> entry : tableProviderMap.entrySet()) {
			if (entry.getValue() == tableProvider) {
				return entry.getKey();
			}
		}
		return null;
	}

	public static interface Listener {

		public void tableProviderRegistryChanged(String key, final TableProvider tableProvider);
	}

	private final Collection<Listener> registryChangedListeners = new ArrayList<TableProviderRegistry.Listener>();

	protected void fireTableProviderRegistryChanged(final String key, final TableProvider tableProvider) {
		for (final Listener listener : registryChangedListeners) {
			listener.tableProviderRegistryChanged(key, tableProvider);
		}
	}

	public void addTableRegistryChangedListener(final TableProviderRegistry.Listener listener) {
		registryChangedListeners.add(listener);
	}

	public void removeTableRegistryChangedListener(final TableProviderRegistry.Listener listener) {
		registryChangedListeners.remove(listener);
	}
}

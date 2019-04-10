package etablesaw.ui;

import java.util.ArrayList;
import java.util.Collection;

public class TableProviderHelper {

    private Collection<TableProvider.Listener> tableProviderListeners;

    public Collection<TableProvider.Listener> getTableProviderListeners() {
        return new ArrayList<>(tableProviderListeners);
    }
    
    public void addTableDataProviderListener(final TableProvider.Listener listener) {
        if (tableProviderListeners == null) {
            tableProviderListeners = new ArrayList<TableProvider.Listener>();
        }
        if (! tableProviderListeners.contains(listener)) {
            tableProviderListeners.add(listener);
        }
    }

    public void removeTableDataProviderListener(final TableProvider.Listener listener) {
        if (tableProviderListeners != null) {
            tableProviderListeners.remove(listener);
        }
    }

    public void fireTableDataChanged(final TableProvider tableProvider) {
        if (tableProviderListeners != null) {
            for (final TableProvider.Listener tableProviderListener : tableProviderListeners) {
                if (tableProviderListener != tableProvider) {
                    tableProviderListener.tableDataChanged(tableProvider);
                }
            }
        }
    }

    public void fireTableChanged(final TableProvider tableProvider) {
        if (tableProviderListeners != null) {
            for (final TableProvider.Listener tableProviderListener : tableProviderListeners) {
                if (tableProviderListener != tableProvider) {
                    tableProviderListener.tableChanged(tableProvider);
                }
            }
        }
    }
}

package etablesaw.xtext.ui.commands;

import etablesaw.ui.Activator;
import etablesaw.ui.TableProviderRegistry;
import etablesaw.xtext.lib.XawBase.Exporter;
import etablesaw.xtext.lib.XawBase.Importer;
import tech.tablesaw.api.Table;

public class TableProviderRegistryPorter implements Importer, Exporter {

    private final TableProviderRegistry tableProviderRegistry = Activator.getInstance().getTableProviderRegistry();

    @Override
    public void exportTable(Table table, String name) {
        tableProviderRegistry.registerTable(name, table);
    }

    @Override
    public Table importTable(String name) {
        return tableProviderRegistry.getTableProvider(name).getTable();
    }        
}

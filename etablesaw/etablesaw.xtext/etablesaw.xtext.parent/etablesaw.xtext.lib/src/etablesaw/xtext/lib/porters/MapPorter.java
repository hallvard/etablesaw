package etablesaw.xtext.lib.porters;

import java.util.HashMap;
import java.util.Map;

import etablesaw.xtext.lib.XawBase;
import etablesaw.xtext.lib.XawBase.Exporter;
import etablesaw.xtext.lib.XawBase.Importer;
import tech.tablesaw.api.Table;

public class MapPorter implements Exporter, Importer {
    
    private final Map<String, Table> tables = new HashMap<>();

    @Override
    public void exportTable(Table table, String name) {
        tables.put(name, table);
    }

    @Override
    public Table importTable(String name) {
        return tables.get(name);
    }
    
    public Map<String, Table> getTables() {
        return tables;
    }
}

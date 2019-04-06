package etablesaw.xtext.lib;

import java.util.HashMap;
import java.util.Map;

import tech.tablesaw.api.Table;

public abstract class XawBase implements Runnable {
    
    private final Map<String, Table> exportedTables = new HashMap<>();

    public void exportTable(Table table, String key) {
        exportedTables.put(key, table);
    }

    public void exportTables(Table... tables) {
        for (Table table : tables) {
            exportTable(table, table.name());
        }
    }
    
    public Map<String, Table> getExportedTables() {
        return exportedTables;
    }
}

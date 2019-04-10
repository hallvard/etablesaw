package etablesaw.xtext.lib;

import java.util.ArrayList;
import java.util.Collection;

import etablesaw.xtext.lib.porters.DataFrameReaderImporter;
import etablesaw.xtext.lib.porters.MapPorter;
import tech.tablesaw.api.Table;

public abstract class XawBase implements Runnable {
    
    private Importer importer = new MapPorter();
    private Exporter exporter = new MapPorter();

    public Importer getImporter() {
        return importer;
    }

    public void setImporter(Importer importer) {
        this.importer = importer;
    }
    
    public Exporter getExporter() {
        return exporter;
    }
    
    public void setExporter(Exporter exporter) {
        this.exporter = exporter;
    }
    
    public Table importTable(String name) {
        if (importer != null) {
            return importer.importTable(name);
        }
        return null;
    }
    
    public Collection<Table> importTables(String... names) {
        Collection<Table> tables = new ArrayList<Table>();
        for (String name : names) {
            Table table = importTable(name);
            if (table != null) {
                tables.add(table);
            }
        }
        return tables;
    }
    
    public void exportTable(Table table, String key) {
        if (exporter != null) {
            exporter.exportTable(table, key);
        }
    }

    public void exportTables(Table... tables) {
        for (Table table : tables) {
            exportTable(table, table.name());
        }
    }
    
    public static interface Importer {
        public Table importTable(String name);
    }

    public static interface Exporter {
        public void exportTable(Table table, String name);
    }
    
    public static void main(XawBase xawBase) {
        xawBase.setImporter(new DataFrameReaderImporter(xawBase.getClass()));
        xawBase.run();
    }
}

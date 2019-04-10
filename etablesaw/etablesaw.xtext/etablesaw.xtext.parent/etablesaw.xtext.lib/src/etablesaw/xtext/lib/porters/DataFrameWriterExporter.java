package etablesaw.xtext.lib.porters;

import java.io.File;
import java.io.IOException;

import etablesaw.xtext.lib.XawBase;
import etablesaw.xtext.lib.XawBase.Exporter;
import tech.tablesaw.api.Table;

public class DataFrameWriterExporter implements Exporter {

    private final File folder;
    
    public DataFrameWriterExporter(File folder) {
        this.folder = folder;
    }

    @Override
    public void exportTable(Table table, String name) {
        try {
            table.write().csv(new File(folder, name));
        } catch (IOException e) {
            throw new RuntimeException("Exception when exporting " + name, e);
        }
    }
}

package etablesaw.xtext.lib.porters;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;

import etablesaw.xtext.lib.XawBase.Importer;
import tech.tablesaw.api.Table;

public class DataFrameReaderImporter implements Importer {

    private final URI parent;
    
    private static URI getParent(URL url) {
        String s = url.toString();
        return URI.create(s.substring(0, s.lastIndexOf('/') + 1));
    }
    
    public DataFrameReaderImporter(Class<?> clazz) {
        this(getParent(clazz.getResource(clazz.getSimpleName() + ".class")));
    }
    
    public DataFrameReaderImporter(File folder) {
        this(URI.create("file:" + folder.getPath()));
    }

    public DataFrameReaderImporter(String parent) {
        this(URI.create(parent));
    }

    public DataFrameReaderImporter(URL parent) throws URISyntaxException {
        this(parent.toURI());
    }

    public DataFrameReaderImporter(URI parent) {
        this.parent = parent;
    }

    @Override
    public Table importTable(String name) {
        try {
            URL url = parent.resolve(name).toURL();
            return Table.read().csv(url.openStream());
        } catch (IOException e) {
            throw new RuntimeException("Exception when importing " + name, e);
        }
    }
}

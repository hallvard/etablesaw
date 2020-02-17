package etablesaw.ui;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import etablesaw.io.FileFormatSupport;
import etablesaw.ui.expr.ExprSupport;
import tech.tablesaw.api.Table;

public class Activator extends AbstractUIPlugin {

	private static Activator instance = null;

	public static Activator getInstance() {
		return instance;
	}

	@Override
	public void start(final BundleContext context) throws Exception {
		instance = this;
	}

	@Override
	public void stop(final BundleContext context) throws Exception {
		instance = null;
		tableProviderRegistry.clear();
	}

	private final TableProviderRegistry tableProviderRegistry = new TableProviderRegistry();

	public TableProviderRegistry getTableProviderRegistry() {
		return tableProviderRegistry;
	}

	//
	
	private Collection<ExprSupport> exprSupports = null;

	public ExprSupport[] getExprSupports() {
		if (exprSupports == null) {
			exprSupports = new ArrayList<ExprSupport>();
			processExprSupportExtensions();
		}
		return exprSupports.toArray(new ExprSupport[exprSupports.size()]);
	}

	public ExprSupport getExprSupport(String... names) {
	    for (String name : names) {
	        ExprSupport exprSupport = getExprSupport(name);
	        if (exprSupport != null) {
	            return exprSupport;
	        }
	    }
	    return null;
	}

	public ExprSupport getExprSupport(String name) {
	    for (ExprSupport exprSupport : getExprSupports()) {
	        if ("*".equals(name) || exprSupport.getLang().equals(name)) {
	            return exprSupport;
	        }
	    }
	    return null;
	}

	private void processExprSupportExtensions() {
		final IExtensionPoint ep = Platform.getExtensionRegistry().getExtensionPoint("etablesaw.ui.exprSupport");
		for (final IExtension extension : ep.getExtensions()) {
			for (final IConfigurationElement ces : extension.getConfigurationElements()) {
				if ("exprSupport".equals(ces.getName())) {
					try {
						final ExprSupport es = (ExprSupport) ces.createExecutableExtension("supportClass");
						es.setLang(ces.getAttribute("langName"));
						exprSupports.add(es);
					} catch (final CoreException e) {
					}
				}
			}
		}
	}
	
	//

	private final static FileFormatSupport unsupportedFileFormat = new FileFormatSupport() {
	    @Override
	    public Boolean supportsFormat(String format) {
	        return false;
	    }
	    @Override
	    public Table[] read(String name, Supplier<InputStream> input) throws IOException {
	        throw new UnsupportedOperationException("Write is not supported");
	    }
	    @Override
	    public void write(Table[] tables, String name, OutputStream output) throws IOException {
	        throw new UnsupportedOperationException("Write is not supported");
	    }
	};

	private boolean readAllAtOnce = false;
	private Map<String, FileFormatSupport> fileFormatSupports = null;
	
	public FileFormatSupport getFileFormatSupport(String key) {
	    if (fileFormatSupports == null) {
	        fileFormatSupports = new HashMap<String, FileFormatSupport>();
	        if (readAllAtOnce) {
	            processFileFormatSupportExtensions(null);	            
	        }
	    }
	    if ((! readAllAtOnce) && (! fileFormatSupports.containsKey(key))) {
    	    if (! processFileFormatSupportExtensions(key)) {
    	        fileFormatSupports.put(key, unsupportedFileFormat);
    	    }
    	}
	    return fileFormatSupports.get(key);
	}

	private boolean processFileFormatSupportExtensions(String key) {
	    final IExtensionPoint ep = Platform.getExtensionRegistry().getExtensionPoint("etablesaw.core.fileFormatSupport");
	    for (final IExtension extension : ep.getExtensions()) {
	        for (final IConfigurationElement ces : extension.getConfigurationElements()) {
	            if ("fileFormatSupport".equals(ces.getName())) {
	                try {
	                    FileFormatSupport ffs = null;
	                    for (String fileFormat : ces.getAttribute("fileFormats").split(",\\s*")) {
	                        if (key == null || key.equals(fileFormat)) {
	                            if (ffs == null) {
	                                ffs = (FileFormatSupport) ces.createExecutableExtension("supportClass");
	                            }
	                            fileFormatSupports.put(fileFormat, ffs);
	                            if (key != null) {
	                                return true;
	                            }
	                        }
	                    }
	                } catch (final Exception e) {
	                }
	            }
	        }
	    }
	    return false;
	}
}

package etablesaw.ui;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import etablesaw.io.FileFormatSupport;
import etablesaw.ui.expr.ExprSupport;

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

	private Map<String, FileFormatSupport> fileFormatSupports = null;
	
	public FileFormatSupport getFileFormatSupport(String key) {
	    if (fileFormatSupports == null) {
	        fileFormatSupports = new HashMap<String, FileFormatSupport>();
	        processFileFormatSupportExtensions();
	    }
	    return fileFormatSupports.get(key);
	}

	private void processFileFormatSupportExtensions() {
	    final IExtensionPoint ep = Platform.getExtensionRegistry().getExtensionPoint("etablesaw.bridge.fileFormatSupport");
	    for (final IExtension extension : ep.getExtensions()) {
	        for (final IConfigurationElement ces : extension.getConfigurationElements()) {
	            if ("fileFormatSupport".equals(ces.getName())) {
	                try {
	                    final FileFormatSupport ffs = (FileFormatSupport) ces.createExecutableExtension("supportClass");
	                    for (String fileFormat : ces.getAttribute("fileFormats").split(",\\s*")) {
	                        fileFormatSupports.put(fileFormat, ffs);
	                    }
	                } catch (final CoreException e) {
	                }
	            }
	        }
	    }
	}
}

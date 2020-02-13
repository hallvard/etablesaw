package etablesaw.ui.util;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.ui.PartInitException;

import etablesaw.io.FileFormatSupport;
import etablesaw.ui.Activator;
import etablesaw.ui.SimpleTableProvider;
import tech.tablesaw.api.Table;

public class ResourceTableProvider extends SimpleTableProvider {

    private ResourceChangeHelper resourceChangeHelper;
    
    public ResourceTableProvider(final IFile file) {
        super(load(file));
        resourceChangeHelper = new ResourceChangeHelper(path -> setTable(load(file))) {
            @Override
            protected boolean isPath(IPath path) {
                return file.getFullPath().equals(path);
            }
        };
    }

    public void dispose() {
        resourceChangeHelper.dispose();
    }

    public static boolean supportsFile(IFile file) {
        String fileFormat = file.getFileExtension();
        FileFormatSupport ffs = Activator.getInstance().getFileFormatSupport(fileFormat);
        return (ffs != null);
    }
    
    private static Table load(final IFile file) throws RuntimeException {
        try {
            String fileFormat = file.getFileExtension();
            FileFormatSupport ffs = Activator.getInstance().getFileFormatSupport(fileFormat);
            if (ffs == null || Boolean.FALSE.equals(ffs.supportsFormat(fileFormat))) {
                throw new PartInitException("Unsupported file format: " + file.getName());
            }
            Table[] tables = ffs.read(file.getName(), () -> {
                try {
                    return file.getContents();
                } catch (CoreException e) {
                }
                return null;
            });
            if (tables == null || tables.length == 0) {
                throw new RuntimeException("Couldn't read table from: " + file.getName());
            }
            return tables[0];
        } catch (final Exception e) {
            return null;
        }
    }    
}

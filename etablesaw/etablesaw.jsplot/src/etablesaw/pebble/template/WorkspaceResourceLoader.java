package etablesaw.pebble.template;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

import com.mitchellbosecke.pebble.error.LoaderException;
import com.mitchellbosecke.pebble.loader.Loader;

public class WorkspaceResourceLoader implements Loader<String> {

    private String prefix;
    private String suffix;
    private String charset = "UTF-8";

    public String getPrefix() {
        return prefix;
    }

    @Override
    public void setPrefix(String prefix) {
        this.prefix = prefix;
    }
    
    public String getSuffix() {
        return suffix;
    }

    @Override
    public void setSuffix(String suffix) {
        this.suffix = suffix;
    }
    
    public String getCharset() {
        return charset;
    }
    
    @Override
    public void setCharset(String charset) {
        this.charset = charset;
    }

    @Override
    public String createCacheKey(String path) {
        return path;
    }

    private IFile getExistingFile(String path) {
        IPath filePath = (getPrefix() != null ? new Path(getPrefix()).append(path) : new Path(path));
        if (getSuffix() != null) {
            filePath = filePath.append(getSuffix());            
        }
        IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(filePath);
        if (file != null && file.exists()) {
            return file;
        }
        return null;
    }
    
    @Override
    public boolean resourceExists(String path) {
        return getExistingFile(path) != null;
    }

    @Override
    public Reader getReader(String path) {
        IFile file = getExistingFile(path);
        Exception ex = null;
        if (file != null) {
            try {
                return new BufferedReader(new InputStreamReader(file.getContents(false), getCharset()));
            } catch (CoreException | UnsupportedEncodingException e) {
                ex = e;
            }
        } else {
            ex = new FileNotFoundException(path + " not found");
        }
        throw new LoaderException(ex, "Could not find template \"" + path + "\"");
    }

    @Override
    public String resolveRelativePath(String relativePath, String anchorPath) {
        IPath basePath = new Path(anchorPath).removeLastSegments(1);
        return basePath.append(relativePath).toString();
    }
}

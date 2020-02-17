package etablesaw.pebble.template;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import com.mitchellbosecke.pebble.PebbleEngine;
import com.mitchellbosecke.pebble.attributes.AttributeResolver;
import com.mitchellbosecke.pebble.error.PebbleException;
import com.mitchellbosecke.pebble.extension.AbstractExtension;
import com.mitchellbosecke.pebble.extension.Function;
import com.mitchellbosecke.pebble.loader.ClasspathLoader;
import com.mitchellbosecke.pebble.loader.DelegatingLoader;
import com.mitchellbosecke.pebble.loader.FileLoader;
import com.mitchellbosecke.pebble.loader.Loader;
import com.mitchellbosecke.pebble.template.PebbleTemplate;

import etablesaw.template.TemplateRenderer;

public class PebbleTemplateRenderer implements TemplateRenderer {

    private ClassLoader templateClassLoader = null;
    
    public void setTemplateClassLoader(ClassLoader templateClassLoader) {
        this.templateClassLoader = templateClassLoader;
    }
    
    public String render(String resource, Map<String, Object> properties) {
        List<Loader<?>> loaders = new ArrayList<Loader<?>>(Arrays.asList(
                new WorkspaceResourceLoader(),
                new FileLoader()
                ));
        if (templateClassLoader != null) {
            loaders.add(new ClasspathLoader(templateClassLoader));
        }
        PebbleEngine engine = new PebbleEngine.Builder()
                .extension(new AbstractExtension() {
                    @Override
                    public List<AttributeResolver> getAttributeResolver() {
                        return TablesawAttributeResolvers.INSTANCES;
                    }
                    @Override
                    public Map<String, Function> getFunctions() {
                        return TablesawFunctions.INSTANCES;
                    }
                })
                .loader(new DelegatingLoader(loaders))
                .build();
        try {
            Writer writer = new StringWriter();
            PebbleTemplate template = engine.getTemplate(resource);
            template.evaluate(writer, properties);
            return writer.toString();
        } catch (PebbleException | IOException e) {
            return "<h2>Exception during rendering of template @ " + resource + "</h2>\n" +
                    "<verbatim>" + e.getMessage() + "</verbatim>";
        }
    }
}

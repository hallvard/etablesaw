package etablesaw.ui.plots;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.graphics.Point;

import etablesaw.pebble.template.PebbleTemplateRenderer;
import etablesaw.template.TemplateRenderer;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;

public abstract class AbstractTemplateBrowserTableView extends AbstractBrowserTableView {

    private TemplateRenderer templateRenderer = new PebbleTemplateRenderer();
    
    protected abstract String getTemplateResource();
    protected abstract Map<String, Object> getTemplateProperties();
    
    @Override
    protected String computeBrowserContents(Point size) {
        String html = null;
        try {
            Map<String, Object> properties = new HashMap<>();
            Table table = getViewTable();
            properties.put("table", table);

            List<Column<?>> columns = table.columns();
            properties.put("columns", columns);

            for (Column<?> col : columns) {
                properties.put(col.name(), col);                
            }
            properties.put(table.name(), table);
            Map<String, Object> xtraProperties = getTemplateProperties();
            if (xtraProperties != null) {
                properties.putAll(xtraProperties);
            }
            html = templateRenderer.render(getTemplateResource(), properties);
        } catch (Exception e) {
            html = "<h2>No contents</h2><p>" + e.getMessage() + "</p>";
        }
        return html;
    }
}

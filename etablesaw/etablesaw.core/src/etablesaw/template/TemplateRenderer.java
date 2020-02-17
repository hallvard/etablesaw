package etablesaw.template;

import java.util.Map;

public interface TemplateRenderer {

    public String render(String resource, Map<String, Object> properties);

}

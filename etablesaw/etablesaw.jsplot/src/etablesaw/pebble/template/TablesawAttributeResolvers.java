package etablesaw.pebble.template;

import java.util.Arrays;
import java.util.List;

import com.mitchellbosecke.pebble.attributes.AttributeResolver;
import com.mitchellbosecke.pebble.attributes.ResolvedAttribute;
import com.mitchellbosecke.pebble.node.ArgumentsNode;
import com.mitchellbosecke.pebble.template.EvaluationContextImpl;

import tech.tablesaw.api.Row;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;

public class TablesawAttributeResolvers {

    public static List<AttributeResolver> INSTANCES = Arrays.asList(
            new RowAttributeResolver(),
            new ColumnAttributeResolver(),
            new TableAttributeResolver()
            );
            
    private static class RowAttributeResolver implements AttributeResolver {

        @Override
        public ResolvedAttribute resolve(Object object, Object name, Object[] values, ArgumentsNode args,
                EvaluationContextImpl context, String filename, int lineNum) {

            String attributeName = String.valueOf(name);

            if (object instanceof Row) {
                Row row = (Row) object;
                Object value = null;
                try {
                    int colNum = Integer.parseInt(attributeName);
                    value = row.getObject(colNum);
                } catch (NumberFormatException e) {
                    value = row.getObject(attributeName);
                }
                return new ResolvedAttribute(value);
            }
            return null;
        }
    }

    private static class ColumnAttributeResolver implements AttributeResolver {
        
        @Override
        public ResolvedAttribute resolve(Object object, Object name, Object[] values, ArgumentsNode args,
                EvaluationContextImpl context, String filename, int lineNum) {
            
            String attributeName = String.valueOf(name);
            
            if (object instanceof Column<?>) {
                Column<?> col = (Column<?>) object;
                try {
                    int rowNum = Integer.parseInt(attributeName);
                    return new ResolvedAttribute(col.getString(rowNum));
                } catch (NumberFormatException e) {
                }
            }
            return null;
        }
    }
    
    public final static Column<?> column(Table table, Object column) {
        String columnName = String.valueOf(column);
        Column<?> col = null;
        try {
            int colNum = Integer.parseInt(columnName);
            col = table.column(colNum);
        } catch (NumberFormatException e) {
            col = table.column(columnName);
        }
        return col;
    }
    
    private static class TableAttributeResolver implements AttributeResolver {

        @Override
        public ResolvedAttribute resolve(Object object, Object name, Object[] values, ArgumentsNode args,
                EvaluationContextImpl context, String filename, int lineNum) {

            String attributeName = String.valueOf(name);

            if (object instanceof Table) {
                Table table = (Table) object;
                Column<?> col = column(table, attributeName);
                return (col != null ? new ResolvedAttribute(col) : null);
            }
            return null;
        }
    }
}

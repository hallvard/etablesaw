package etablesaw.pebble.template;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.mitchellbosecke.pebble.error.PebbleException;
import com.mitchellbosecke.pebble.extension.Function;
import com.mitchellbosecke.pebble.template.EvaluationContext;
import com.mitchellbosecke.pebble.template.PebbleTemplate;

import tech.tablesaw.api.CategoricalColumn;
import tech.tablesaw.api.NumericColumn;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;

public class TablesawFunctions {

    private static final String TABLE_ARG = "table";
    private static final String COLUMN_NAMES_ARG = "columnNames";
    private static final String COLUMN_ARG = "column";
    private static final String TABLE_OR_COLUMNS_ARG = "tableOrColumns";

    private static boolean isOfType(Object object, Class<?> clazz) {
        return (clazz == null || clazz.isInstance(object));
    }

    public static List<Column<?>> columns(Object arg, Class<?> colClass) {
        List<Column<?>> columns = arg instanceof Table ? ((Table) arg).columns() : (List<Column<?>>) arg;
        return columns.stream().filter(col -> isOfType(col, colClass)).collect(Collectors.toList());
    }

    public static Map<String, Function> INSTANCES = new HashMap<String, Function>();
    {
        INSTANCES.put("columns", new TablesawFunctions.Columns());
        INSTANCES.put("isCategorical", new BaseFunction(args -> isOfType(args.get(COLUMN_ARG), CategoricalColumn.class), COLUMN_ARG));
        INSTANCES.put("categoricalColumns", new BaseFunction(args -> columns(args.get(TABLE_OR_COLUMNS_ARG), CategoricalColumn.class), TABLE_OR_COLUMNS_ARG));
        INSTANCES.put("isNumeric", new BaseFunction(args -> isOfType(args.get(COLUMN_ARG), NumericColumn.class), COLUMN_ARG));
        INSTANCES.put("numericColumns", new BaseFunction(args -> columns(args.get(TABLE_OR_COLUMNS_ARG), NumericColumn.class), TABLE_OR_COLUMNS_ARG));
    }

    private static class BaseFunction implements Function {

        private java.util.function.Function<Map<String, Object>, Object> fun;
        private List<String> argumentNames;
        
        protected BaseFunction(java.util.function.Function<Map<String, Object>, Object> fun, String... argumentNames) {
            this.fun = fun;
            this.argumentNames = Arrays.asList(argumentNames);
        }
        protected BaseFunction(String... argumentNames) {
            this(null, argumentNames);
        }
        
        @Override
        public List<String> getArgumentNames() {
            return argumentNames;
        }
        
        private PebbleTemplate template;
        private int lineNum = -1;

        protected RuntimeException pebbleException(Exception cause) {
            throw new PebbleException(cause, "Problem in line " + lineNum + " of " + template.getName() + " template");
        }
        
        @Override
        public Object execute(Map<String, Object> arguments, PebbleTemplate template, EvaluationContext context, int lineNum) {
            this.template = template;
            this.lineNum = lineNum;
            try {
                return execute(arguments);
            } catch (RuntimeException re) {
                if (! (re instanceof PebbleException)) {
                    re = pebbleException(re);
                }
                throw re;
            } finally {
                this.template = null;
                this.lineNum = -1;
            }
        }
        
        protected Object execute(Map<String, Object> arguments) {
            return fun.apply(arguments);
        }
    }

    private static class Columns extends BaseFunction {
        
        public Columns() {
            super(COLUMN_NAMES_ARG);
        }
        
        @Override
        protected Object execute(Map<String, Object> arguments) {
            Table table = (Table) arguments.get(TABLE_ARG);
            List<Column<?>> columns = new ArrayList<>();
            for (Object columnName : (Iterable<?>) arguments.get(COLUMN_NAMES_ARG)) {
                Column<?> col = TablesawAttributeResolvers.column(table, columnName);
                if (col != null) {
                    columns.add(col);
                }
            }
            return columns;
        }
    }
}

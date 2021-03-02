package etablesaw.janino.expr;

import java.lang.reflect.InvocationTargetException;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.codehaus.commons.compiler.CompileException;
import org.codehaus.janino.ExpressionEvaluator;

import etablesaw.ui.expr.AbstractPreparedStringExpr;
import etablesaw.ui.expr.ExprSupport;
import etablesaw.ui.expr.PreparedExpr;
import tech.tablesaw.api.BooleanColumn;
import tech.tablesaw.api.ColumnType;
import tech.tablesaw.api.DateColumn;
import tech.tablesaw.api.DateTimeColumn;
import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.FloatColumn;
import tech.tablesaw.api.InstantColumn;
import tech.tablesaw.api.IntColumn;
import tech.tablesaw.api.LongColumn;
import tech.tablesaw.api.NumberColumn;
import tech.tablesaw.api.ShortColumn;
import tech.tablesaw.api.StringColumn;
import tech.tablesaw.api.TextColumn;
import tech.tablesaw.api.TimeColumn;

public class PreparedJaninoExpr extends AbstractPreparedStringExpr implements PreparedExpr {

	private List<String> paramNames;
	private Collection<Class<?>> paramTypes;
    private Map<String, String> varNameMap;

    private final ExpressionEvaluator ee;

	public PreparedJaninoExpr(String expr, Map<String, ColumnType> varTypes, String colVar) {
		super(expr);

		ee = new ExpressionEvaluator();

		paramNames = new ArrayList<>();
		paramTypes = new ArrayList<>();
		varNameMap = new HashMap<>();
		for (String name : varTypes.keySet()) {
			Class<?> paramClass = ExprSupport.getClassForColumnType(varTypes.get(name));
			if (paramClass == null) {
				addDiagnostics(name + " has type " + varTypes.get(name) + ", which is not supported");
				paramClass = Object.class;
			}
			String altName = fixName(name.toUpperCase());
			if (altName == null) {
				altName = name.toUpperCase();
			}
			if (altName != null && (! altName.equals(name))) {
				varNameMap.put(name, altName);
				varNameMap.put(altName, name);
				name = altName;
			}
			paramNames.add(name);
			paramTypes.add(paramClass);
		}
        ee.setParameters(paramNames.toArray(new String[0]), paramTypes.toArray(new Class[0]));
        ee.setExpressionType(Object.class);
 
        try {
			ee.cook(expr);
		} catch (CompileException e) {
			addDiagnostics(e.getMessage());
		}
	}

	public Object eval(Map<String, Object> varValues) {
		Object[] args = new Object[paramNames.size()];
		for (int argNum = 0; argNum < args.length; argNum++) {
			String name = paramNames.get(argNum);
			String altName = varNameMap.get(name);
			args[argNum] = varValues.get(altName != null ? altName : name);
		}
		try {
			return ee.evaluate(args);
		} catch (InvocationTargetException e) {
			throw new RuntimeException(e);
		}
	}
	
	private final static char SPECIAL_CHAR_REPLACEMENT = '_';

	private int fixedNameCount = 0;
	
	protected String fixName(String varName) {
        StringBuilder fixedName = new StringBuilder();
        boolean fixed = false;
        if (! Character.isJavaIdentifierStart(varName.charAt(0))) {
            fixedName.append(SPECIAL_CHAR_REPLACEMENT);
            fixed = true;
        }
        for (int i = 0; i < varName.length(); i++) {
            char c = varName.charAt(i);
            if (Character.isJavaIdentifierPart(c)) {
                fixedName.append(c);
            } else {
                fixedName.append(SPECIAL_CHAR_REPLACEMENT);
                fixed = true;
            }
        }
        if (fixed) {
            if (fixedNameCount > 0) {
                fixedName.append(fixedNameCount);
            }
            fixedNameCount++;
        }
        return fixedName.toString();
    }
}

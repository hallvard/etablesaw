package etablesaw.ui.expr;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import tech.tablesaw.api.BooleanColumn;
import tech.tablesaw.api.ColumnType;
import tech.tablesaw.api.DateColumn;
import tech.tablesaw.api.DateTimeColumn;
import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.FloatColumn;
import tech.tablesaw.api.InstantColumn;
import tech.tablesaw.api.IntColumn;
import tech.tablesaw.api.LongColumn;
import tech.tablesaw.api.ShortColumn;
import tech.tablesaw.api.StringColumn;
import tech.tablesaw.api.Table;
import tech.tablesaw.api.TextColumn;
import tech.tablesaw.api.TimeColumn;
import tech.tablesaw.columns.Column;

public abstract class ExprSupport {

	private String lang;

	public String getLang() {
		return lang;
	}

	public void setLang(final String lang) {
		this.lang = lang;
	}

	public abstract PreparedExpr prepareExpr(String expr, Map<String, ColumnType> varTypes, String colVar);
	public abstract Object evalExpr(PreparedExpr expr, Map<String, Object> varValues);

	public Map<String, ColumnType> getVarTypes(final Table table) {
		final Map<String, ColumnType> varTypes = new HashMap<String, ColumnType>();
		for (int colNum = 0; colNum < table.columnCount(); colNum++) {
			final Column<?> column = table.column(colNum);
			final ColumnType colType = column.type();
			varTypes.put(column.name(), colType);
		}
		return varTypes;
	}

	public Map<String, Object> getVarValues(final Table table, final int rowNum, final Map<String, Object> varValues) {
		for (int colNum = 0; colNum < table.columnCount(); colNum++) {
			final Column<?> column = table.column(colNum);
			final Object value = column.get(rowNum);
			varValues.put(column.name(), value);
		}
		return varValues;
	}

	public Map<String, Object> getVarValues(final Table table, final int rowNum) {
		return getVarValues(table, rowNum, new HashMap<String, Object>());
	}
	
	// rewrite simplified expressions


	private List<Pattern> patterns = null;
	private List<String> formats = null;
	
	public void addRewritePattern(String pattern, String format) {
		if (patterns == null) {
			patterns = new ArrayList<>();
			formats = new ArrayList<>();
		}
		patterns.add(Pattern.compile(pattern));
		formats.add(format);
	}

	public void addRewritePatterns(String... patterns) {
		for (int i = 0; i < patterns.length; i += 2) {
			addRewritePattern(patterns[i], patterns[i + 1]);
		}
	}

	/**
	 * Rewrites expr to alternative form, e.g. from shortcut to full form 
	 * @param expr
	 * @return
	 */
	public String rewriteExpr(String expr) {
		if (patterns != null) {
			for (int i = 0; i < patterns.size(); i++) {
				Matcher matcher = patterns.get(i).matcher(expr);
				if (matcher.matches()) {
					Object[] groups = new Object[matcher.groupCount()];
					for (int j = 0; j < groups.length; j++) {
						groups[j] = matcher.group(j + 1);
					}
					return String.format(formats.get(i), groups);
				}
			}
		}
		return null;
	}

	protected String getPossiblyRewrittenExpr(String expr) {
		String rewritten = rewriteExpr(expr);
		return (rewritten != null ? rewritten : expr);
	}
	
	//

	private static Map<ColumnType, Class<?>> columnTypeClasses = new HashMap<>();
	static {
		columnTypeClasses.put(ColumnType.BOOLEAN, Boolean.TYPE);
		
		columnTypeClasses.put(ColumnType.INTEGER, Integer.TYPE);
		columnTypeClasses.put(ColumnType.LONG, Long.TYPE);
		columnTypeClasses.put(ColumnType.SHORT, Short.TYPE);
		
		columnTypeClasses.put(ColumnType.DOUBLE, Double.TYPE);
		columnTypeClasses.put(ColumnType.FLOAT, Float.TYPE);
		
		columnTypeClasses.put(ColumnType.STRING, String.class);
		columnTypeClasses.put(ColumnType.TEXT, String.class);

		columnTypeClasses.put(ColumnType.LOCAL_TIME, LocalTime.class);
		columnTypeClasses.put(ColumnType.LOCAL_DATE_TIME, LocalDateTime.class);
		columnTypeClasses.put(ColumnType.LOCAL_DATE, LocalDate.class);
		columnTypeClasses.put(ColumnType.INSTANT, Instant.class);
	}

	public static Class<?> getClassForColumnType(ColumnType columnType) {
		return columnTypeClasses.get(columnType);
	}
	
	public static ColumnType getColumnTypeForClass(Class<?> columnClass) {
		for (ColumnType columnType : columnTypeClasses.keySet()) {
			if (columnTypeClasses.get(columnType) == columnClass) {
				return columnType;
			}
		}
		return null;
	}
}

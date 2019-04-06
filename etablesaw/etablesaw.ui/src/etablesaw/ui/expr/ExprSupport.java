package etablesaw.ui.expr;

import java.util.HashMap;
import java.util.Map;

import tech.tablesaw.api.ColumnType;
import tech.tablesaw.api.Table;
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
}

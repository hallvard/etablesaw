package etablesaw.ui.editor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import etablesaw.ui.expr.ExprSupport;
import etablesaw.ui.expr.PreparedExpr;
import tech.tablesaw.api.ColumnType;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;

public abstract class ExprSupportHelper {

	protected final TablesawDataProvider dataProvider;
	protected final ExprSupport exprSupport;

	public ExprSupportHelper(final TablesawDataProvider dataProvider, final ExprSupport exprSupport) {
		super();
		this.dataProvider = dataProvider;
		this.exprSupport = exprSupport;
	}

	protected abstract String getExpr(int columnIndex);
	protected abstract boolean handleResult(int rowIndex, int columnIndex, Object result);
	
	protected void applyExprs(Collection<Integer> indices) {
		final Table table = dataProvider.getTable();
		final Map<String,  ColumnType> varTypes = exprSupport.getVarTypes(table);
		final List<PreparedExpr> preparedExprs = new ArrayList<PreparedExpr>();
		for (final int columnIndex : indices) {
			final String exprString = getExpr(columnIndex);
			if (exprString != null) {
				Column<?> column = table.column(columnIndex);
				final PreparedExpr expr = exprSupport.prepareExpr(exprString, varTypes, column.name());
				if (expr.getDiagnostics().isEmpty()) {
					while (preparedExprs.size() <= columnIndex) {
						preparedExprs.add(null);
					}
					preparedExprs.set(columnIndex, expr);
				}
			}
		}
		if (! preparedExprs.isEmpty()) {
			final int rowCount = table.rowCount();
			final Map<String, Object> varValues = new HashMap<String, Object>();
			outer: for (int rowNum = 0; rowNum < rowCount; rowNum++) {
				exprSupport.getVarValues(table, rowNum, varValues);
				for (int colNum = 0; colNum < preparedExprs.size(); colNum++) {
					final PreparedExpr expr = preparedExprs.get(colNum);
					if (expr != null) {
						Object result;
                        try {
                            result = exprSupport.evalExpr(expr, varValues);
                            if (! handleResult(rowNum, colNum, result)) {
                                continue outer;
                            }
                        } catch (Exception e) {
                            if (! handleResult(rowNum, colNum, e)) {
                                continue outer;
                            }
                        }
					}
				}
			}
		}
	}
}

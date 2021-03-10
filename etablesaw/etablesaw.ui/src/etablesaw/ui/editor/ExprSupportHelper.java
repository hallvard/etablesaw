package etablesaw.ui.editor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
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

	private Collection<Integer> tableIndices(Collection<Integer> indices) {
	    Collection<String> columnNames = dataProvider.getColumnNames(indices);
        Collection<Integer> tableIndices = new ArrayList<Integer>();
        final Table table = dataProvider.getTable();
        for (String columnName : columnNames) {
            tableIndices.add(table.columnIndex(columnName));
        }
        return tableIndices;
	}
	
	protected void applyExprs(Collection<Integer> indices) {
	    final Table table = dataProvider.getTable();
		final Map<String,  ColumnType> varTypes = exprSupport.getVarTypes(table);
		final List<PreparedExpr> preparedExprs = new ArrayList<PreparedExpr>();
		Iterator<Integer> tableIndices = tableIndices(indices).iterator();
		for (int columnIndex : indices) {
			final String exprString = getExpr(columnIndex);
			if (exprString != null) {
				int tableColumnIndex = tableIndices.next();
                Column<?> column = table.column(tableColumnIndex);
				final PreparedExpr expr = exprSupport.prepareExpr(exprString, varTypes, column.name());
				if (expr.getDiagnostics().isEmpty()) {
					while (preparedExprs.size() <= tableColumnIndex) {
						preparedExprs.add(null);
					}
					preparedExprs.set(tableColumnIndex, expr);
				} else {
					System.err.println("Problems preparing " + expr.getExpr() + ":");
					for (String diag : expr.getDiagnostics()) {
						System.err.println(" - " + diag);
					}
				}
			}
		}
		if (! preparedExprs.isEmpty()) {
			final int rowCount = table.rowCount();
			final Map<String, Object> varValues = new HashMap<String, Object>();
			long startTime = System.nanoTime();
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
            long endTime = System.nanoTime();
            long ms = (endTime - startTime) / 1_000L;
            System.out.println(String.format("Took %s microseconds for %s rows, %s microseconds pr. row", ms, rowCount, ms / rowCount));
		}
	}
}

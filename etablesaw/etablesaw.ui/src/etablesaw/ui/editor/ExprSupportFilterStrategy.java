package etablesaw.ui.editor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.nebula.widgets.nattable.filterrow.IFilterStrategy;

import etablesaw.ui.expr.ExprSupport;
import etablesaw.ui.expr.PreparedExpr;
import tech.tablesaw.api.ColumnType;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;
import tech.tablesaw.selection.BitmapBackedSelection;
import tech.tablesaw.selection.Selection;

public class ExprSupportFilterStrategy<T> implements IFilterStrategy<T> {

	private final TablesawDataProvider dataProvider;
	private final ExprSupport exprSupport;

	public ExprSupportFilterStrategy(final TablesawDataProvider dataProvider, final ExprSupport exprSupport) {
		super();
		this.dataProvider = dataProvider;
		this.exprSupport = exprSupport;
	}

	@Override
	public void applyFilter(final Map<Integer, Object> filterIndexToObjectMap) {
		final Table table = dataProvider.getTable();
		final Map<String,  ColumnType> varTypes = exprSupport.getVarTypes(table);
		final List<PreparedExpr> preparedExprs = new ArrayList<PreparedExpr>();
		for (final int columnIndex : filterIndexToObjectMap.keySet()) {
			final Object filter = filterIndexToObjectMap.get(columnIndex);
			if (filter != null) {
				Column<?> column = table.column(columnIndex);
				final PreparedExpr expr = exprSupport.prepareExpr(String.valueOf(filter), varTypes, column.name());
				if (expr.getDiagnostics().isEmpty()) {
					while (preparedExprs.size() <= columnIndex) {
						preparedExprs.add(null);
					}
					preparedExprs.set(columnIndex, expr);
				}
			}
		}
		Selection selection = null;
		if (! preparedExprs.isEmpty()) {
			final int rowCount = table.rowCount();
			selection = new BitmapBackedSelection(rowCount);
			final Map<String, Object> varValues = new HashMap<String, Object>();
			outer: for (int rowNum = 0; rowNum < rowCount; rowNum++) {
				exprSupport.getVarValues(table, rowNum, varValues);
				for (int colNum = 0; colNum < preparedExprs.size(); colNum++) {
					final PreparedExpr expr = preparedExprs.get(colNum);
					if (expr != null) {
						varValues.put("$", table.column(colNum).get(rowNum));
						final Object result = exprSupport.evalExpr(expr, varValues);
						if (! Boolean.TRUE.equals(result)) {
							selection.removeRange(rowNum, rowNum + 1);
							continue outer;
						}
					}
				}
				//				selection.add(rowNum);
			}
		}
		dataProvider.applyFilter(selection);
	}
}

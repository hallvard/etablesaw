package etablesaw.ui.aql;

import java.util.Map;

import etablesaw.ui.expr.ExprSupport;
import etablesaw.ui.expr.PreparedExpr;
import tech.tablesaw.api.ColumnType;

public class AqlExprSupport extends ExprSupport {

	@Override
	public PreparedExpr prepareExpr(final String expr, final Map<String, ColumnType> varTypes, final String thisVar) {
		return new PreparedAqlExpr(expr, varTypes, thisVar);
	}

	@Override
	public Object evalExpr(final PreparedExpr expr, final Map<String, Object> varValues) {
		final PreparedAqlExpr aqlExpr = (PreparedAqlExpr) expr;
		return aqlExpr.eval(varValues);
	}
}

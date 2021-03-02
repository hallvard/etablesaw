package etablesaw.janino.expr;

import java.util.Map;

import etablesaw.ui.expr.ExprSupport;
import etablesaw.ui.expr.PreparedExpr;
import tech.tablesaw.api.ColumnType;

public class JaninoExprSupport extends ExprSupport {

	@Override
	public PreparedExpr prepareExpr(String expr, Map<String, ColumnType> varTypes, String colVar) {
		return new PreparedJaninoExpr(expr, varTypes, colVar);
	}

	@Override
	public Object evalExpr(PreparedExpr expr, Map<String, Object> varValues) {
		return ((PreparedJaninoExpr) expr).eval(varValues);
	}
}

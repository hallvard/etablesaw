package etablesaw.xtext.ui.expr;

import java.util.Map;

import etablesaw.ui.expr.ExprSupport;
import etablesaw.ui.expr.PreparedExpr;
import tech.tablesaw.api.ColumnType;

public class XawExprSupport extends ExprSupport {

	@Override
	public PreparedExpr prepareExpr(final String expr, final Map<String, ColumnType> varTypes, String colVar) {
		return new PreparedXawExpr(expr, varTypes, colVar);
	}

	@Override
	public Object evalExpr(final PreparedExpr expr, final Map<String, Object> varValues) {
		final PreparedXawExpr xbaseExpr = (PreparedXawExpr) expr;
		return xbaseExpr.eval(varValues);
	}
}

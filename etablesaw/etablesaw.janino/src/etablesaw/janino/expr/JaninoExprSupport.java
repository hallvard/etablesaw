package etablesaw.janino.expr;

import java.util.Map;

import etablesaw.ui.expr.ExprSupport;
import etablesaw.ui.expr.PreparedExpr;
import tech.tablesaw.api.ColumnType;

public class JaninoExprSupport extends ExprSupport {

	private static String INT_REGEX = "\\d+"; 
	private static String STRING_REGEX = "\".*\"";
	private static String ANY_REGEX = ".+";
	
	public JaninoExprSupport() {
		// numeric comparison
		addRewritePattern("([<>]=?)(" + INT_REGEX + ")", "it%s%s");
		addRewritePattern("==(" + INT_REGEX + ")", "it==%s");
		// equals
		addRewritePattern("==(" + ANY_REGEX + ")", "it.equals(%s)");
		addRewritePattern("!=(" + ANY_REGEX + ")", "!it.equals(%s)");
		// compareTo
		addRewritePattern("<>(" + ANY_REGEX + ")", "it.compareTo(%s)!=0");
		addRewritePattern("<(" + ANY_REGEX + ")", "it.compareTo(%s)<0");
		addRewritePattern(">(" + ANY_REGEX + ")", "it.compareTo(%s)>0");
		addRewritePattern("<=(" + ANY_REGEX + ")", "it.compareTo(%s)<=0");
		addRewritePattern(">=(" + ANY_REGEX + ")", "it.compareTo(%s)>=0");
		// regex match
		addRewritePattern("~=/(" + ANY_REGEX + ")/", "it.matches(\"%s\")");
		addRewritePattern("~~/(" + ANY_REGEX + ")/", "it.matches(\"(?:.*)%s(?:.*)\")");
	}
	
	@Override
	public PreparedExpr prepareExpr(String expr, Map<String, ColumnType> varTypes, String colVar) {
		return new PreparedJaninoExpr(getPossiblyRewrittenExpr(expr), varTypes, colVar);
	}

	@Override
	public Object evalExpr(PreparedExpr expr, Map<String, Object> varValues) {
		return ((PreparedJaninoExpr) expr).eval(varValues);
	}
}

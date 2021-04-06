package etablesaw.janino.expr;

import java.util.Map;

import etablesaw.ui.expr.ExprSupport;
import etablesaw.ui.expr.PreparedExpr;
import tech.tablesaw.api.ColumnType;

public class JaninoExprSupport extends ExprSupport {

	private static String INT_REGEX = "\\d+"; 
	private static String STRING_REGEX = "\".*\"";
	private static String ANY_REGEX = ".+";

	private static String[] REWRITE_PATTERNS = {
		// numeric comparison
		"([<>]=?)(" + INT_REGEX + ")", 	"it%s%s",
		"==(" + INT_REGEX + ")", 		"it==%s",
		// equals
		"==(" + ANY_REGEX + ")", 		"it.equals(%s)",
		"!=(" + ANY_REGEX + ")", 		"!it.equals(%s)",
		// compareTo
		"<>(" + ANY_REGEX + ")", 		"it.compareTo(%s)!=0",
		"<(" + ANY_REGEX + ")", 		"it.compareTo(%s)<0",
		">(" + ANY_REGEX + ")", 		"it.compareTo(%s)>0",
		"<=(" + ANY_REGEX + ")",		 "it.compareTo(%s)<=0",
		">=(" + ANY_REGEX + ")", 		"it.compareTo(%s)>=0",
		// regex match
		"~=/(" + ANY_REGEX + ")/",		"it.matches(\"%s\")",
		"~~/(" + ANY_REGEX + ")/", 		"it.matches(\"(?:.*)%s(?:.*)\")"
	};
	
	public JaninoExprSupport() {
		addRewritePatterns(REWRITE_PATTERNS);
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

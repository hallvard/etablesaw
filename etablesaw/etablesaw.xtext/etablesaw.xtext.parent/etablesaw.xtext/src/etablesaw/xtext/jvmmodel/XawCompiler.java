package etablesaw.xtext.jvmmodel;

import java.net.URI;
import java.net.URISyntaxException;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.Month;
import java.util.Iterator;

import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.compiler.XbaseCompiler;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;

import com.google.inject.Inject;

import etablesaw.xtext.validation.XawValidator;
import etablesaw.xtext.xaw.InlineTableRow;
import etablesaw.xtext.xaw.TableColumn;
import etablesaw.xtext.xaw.TableLiteral;
import etablesaw.xtext.xaw.XCastedColumnExpression;
import etablesaw.xtext.xaw.XLocalDateLiteral;
import etablesaw.xtext.xaw.XLocalTimeLiteral;
import etablesaw.xtext.xaw.XURLLiteral;

@SuppressWarnings("restriction")
public class XawCompiler extends XbaseCompiler {

	@Override
	protected void internalToConvertedExpression(final XExpression obj, final ITreeAppendable appendable) {
		if (obj instanceof TableLiteral) {
			_toJavaExpression((TableLiteral) obj, appendable);
		} else if (obj instanceof XCastedColumnExpression) {
			_toJavaExpression((XCastedColumnExpression) obj, appendable);
		} else if (obj instanceof XLocalTimeLiteral) {
			_toJavaExpression((XLocalTimeLiteral) obj, appendable);
		} else if (obj instanceof XLocalDateLiteral) {
			_toJavaExpression((XLocalDateLiteral) obj, appendable);
		} else if (obj instanceof XURLLiteral) {
			_toJavaExpression((XURLLiteral) obj, appendable);
		} else {
			super.internalToConvertedExpression(obj, appendable);
		}
	}

	@Override
	protected void doInternalToJavaStatement(final XExpression expr, final ITreeAppendable appendable, final boolean isReferenced) {
		if (expr instanceof TableLiteral) {
			_toJavaStatement((TableLiteral) expr, appendable, isReferenced);
		} else if (expr instanceof TableColumn) {
			_toJavaStatement(expr, appendable, isReferenced);
		} else if (expr instanceof InlineTableRow) {
			_toJavaStatement((InlineTableRow) expr, appendable, isReferenced);
		} else if (expr instanceof XCastedColumnExpression) {
			_toJavaStatement((XCastedColumnExpression) expr, appendable, isReferenced);
		} else if (expr instanceof XURLLiteral) {
			generateComment(expr, appendable, isReferenced);
		} else if (expr instanceof XLocalDateLiteral || expr instanceof XLocalTimeLiteral) {
			generateComment(expr, appendable, isReferenced);
		} else {
			super.doInternalToJavaStatement(expr, appendable, isReferenced);
		}
	}

	@Override
	protected boolean internalCanCompileToJavaExpression(final XExpression expression, final ITreeAppendable appendable) {
		if (expression instanceof XCastedColumnExpression) {
			final XCastedColumnExpression castedExpression = (XCastedColumnExpression) expression;
			return internalCanCompileToJavaExpression(castedExpression.getTarget(), appendable);
		} else if (expression instanceof XLocalDateLiteral || expression instanceof XLocalTimeLiteral) {
			return true;
		} else if (expression instanceof XURLLiteral) {
			return true;
		}
		return super.internalCanCompileToJavaExpression(expression, appendable);
	}

	@Override
	protected boolean isVariableDeclarationRequired(final XExpression expr, final ITreeAppendable b, final boolean recursive) {
		if (expr instanceof XCastedColumnExpression) {
			return false;
		}
		return super.isVariableDeclarationRequired(expr, b, recursive);
	}

	final static String defaultTableName = "A table";

	@Inject
	private XawUtil xawUtil;
	
	protected void _toJavaExpression(final TableLiteral literal, final ITreeAppendable b) {
	    if (xawUtil.isColumnTable(literal)) {
	        TableColumn column = xawUtil.getTableColumns(literal).iterator().next();
            if (b.hasName(column)) {
                b.append(getVarName(column, b));
            } else {
                internalToJavaExpression(column.getExpression(), b);
            }
	    } else {
    		final String name = literal.getName();
    		String tableTypeName = xawUtil.getTableTypeName(literal);
    		b.append(tableTypeName != null ? "new " + tableTypeName : "Table.create");
    		b.append("(" + "\"" + (name != null ? name : defaultTableName) + "\"");
    		for (final TableColumn column : xawUtil.getTableColumns(literal)) {
    	        b.append(", ");
    			if (b.hasName(column)) {
    				b.append(getVarName(column, b));
    			} else {
    				internalToJavaExpression(column.getExpression(), b);
    			}
    		}
		    b.append(")");
	    }
	}

	@Inject
	private IColumnTypeProvider columnTypeProvider;

	protected void _toJavaStatement(final TableLiteral literal, final ITreeAppendable b, final boolean isReferenced) {
		int colNum = 0;
		final Iterable<TableColumn> tableColumns = xawUtil.getTableColumns(literal);
		for (final TableColumn column : tableColumns) {
			final String name = column.getColumnDef().getName();
			final JvmTypeReference columnType = columnTypeProvider.getColumnTypeReference(column.getColumnDef().getType());
			final XExpression colExpr = column.getExpression();
			if (colExpr != null) {
				internalToJavaStatement(colExpr, b, true);
			} else {
				final String varName = b.declareSyntheticVariable(column, makeJavaIdentifier(name != null ? name + "Column" : "column" + colNum));
				b.newLine();
				serialize(columnType, literal, b);
				b.append(" ").append(varName).append(" = ");
				serialize(columnType, literal, b);
				b.append(".create(\"" + name + "\")").append(";");
			}
			colNum++;
		}
		for (final InlineTableRow row : xawUtil.getTableRows(literal)) {
			final Iterator<TableColumn> columnIt = tableColumns.iterator();
			for (final XExpression column : row.getExpressions()) {
				internalToJavaStatement(column, b, true);
				final TableColumn tableColumn = columnIt.next();
				if (isReferenced) {
					final XExpression colExpr = tableColumn.getExpression();
					final String colVar = getVarName(colExpr != null ? colExpr : tableColumn, b);
					b.newLine().append(colVar).append(".append(");
					internalToJavaExpression(column, b);
					b.append(");");
				}
			}
		}
	}

	// almost copied from XBaseCompiler
	protected void _toJavaExpression(final XCastedColumnExpression expr, final ITreeAppendable b) {
		b.append("((");
		serialize(columnTypeProvider.getColumnTypeReference(expr.getType()), expr, b);
		b.append(") ");
		internalToConvertedExpression(expr.getTarget(), b, getLightweightType(expr));
		b.append(")");
	}

	// almost copied from XBaseCompiler
	protected void _toJavaStatement(final XCastedColumnExpression expr, final ITreeAppendable b, final boolean isReferenced) {
		internalToJavaStatement(expr.getTarget(), b, isReferenced);
	}

	//

	protected void _toJavaExpression(final XLocalTimeLiteral time, final ITreeAppendable b) {
		b.append("java.time.LocalTime.of(");
		b.append(String.valueOf(time.getHour()));
		b.append(", ");
		b.append(String.valueOf(time.getMin()));
		b.append(", ");
		b.append(String.valueOf(time.getSecond()));
		b.append(")");
	}

	protected void _toJavaExpression(final XLocalDateLiteral date, final ITreeAppendable b) {
		b.append("java.time.LocalDate.of(");
		b.append(String.valueOf(date.getYear()));
		b.append(", ");
		try {
			final int month = Integer.valueOf(date.getMonth());
			b.append(String.valueOf(month));
		} catch (final NumberFormatException e) {
			b.append("java.time.Month.");
			b.append(date.getMonth().toUpperCase());
		}
		b.append(", ");
		b.append(String.valueOf(date.getDay()));
		b.append(")");
	}

	@Inject
	private XawValidator xawValidator;
	
	protected void _toJavaExpression(final XURLLiteral url, final ITreeAppendable b) {
		b.append("java.net.URI.create(\"");
		try {
			b.append(createURI(url).toString());
		} catch (final URISyntaxException e) {
			b.append(e.getMessage());
		}
		b.append("\")");
	}

	//	protected void _toJavaExpression(final XURLLiteral url, final ITreeAppendable b) {
	//		b.append(getVarName(url, b));
	//	}
	//
	//	protected void _toJavaStatement(final XURLLiteral url, final ITreeAppendable b, final boolean isReferenced) {
	//		final String varName = b.declareSyntheticVariable(url, makeJavaIdentifier("url"));
	//		b.newLine();
	//		b.append("java.net.URI ");
	//		b.append(varName);
	//		b.append(" = null;");
	//		b.newLine();
	//		b.append("try {");
	//		b.increaseIndentation();
	//		b.newLine();
	//		b.append(varName);
	//		b.append(" = new java.net.URI(\"");
	//		try {
	//			b.append(XawInterpreter.createURI(url).toString());
	//		} catch (final URISyntaxException e) {
	//			b.append(e.getMessage());
	//		}
	//		b.append("\");");
	//		b.decreaseIndentation();
	//		b.newLine();
	//		b.append("} catch (java.net.URISyntaxException ");
	//		b.append(varName + "Exc");
	//		b.append(") {");
	//		b.newLine();
	//		b.append("}");
	//	}
	
    public static LocalTime createLocalTime(final XLocalTimeLiteral time) {
        return LocalTime.of(time.getHour(), time.getMin(), time.getSecond());
    }
    
    public static LocalDate createLocalDate(final XLocalDateLiteral date) {
        try {
            final int month = Integer.valueOf(date.getMonth());
            return LocalDate.of(date.getYear(), month, date.getDay());
        } catch (final NumberFormatException e) {
            return LocalDate.of(date.getYear(), Month.valueOf(date.getMonth().toUpperCase()), date.getDay());
        }
    }
    
    public static URI createURI(final XURLLiteral url) throws URISyntaxException {
        if (url.getUrl() != null) {
            return new URI(url.getUrl());
        } else {
            String urlString = url.getPath();
            if (url.getHost() != null) {
                if (url.getPort() != 0) {
                    urlString = ":" + url.getPort() + urlString;
                }
                urlString = "//" + url.getHost() + urlString;
            }
            String scheme = url.getScheme();
            if (scheme == null) {
                scheme = "file";
            }
            urlString = scheme + ":" + urlString;
            if (! url.getParams().isEmpty()) {
                String params = "";
                for (final String param : url.getParams()) {
                    params = (params.length() == 0 ? "?" : "&") + param;
                }
                urlString = urlString + params;
            }
            if (url.getFrag() != null) {
                urlString = urlString + "#" + url.getFrag();
            }
            return new URI(urlString);
        }
    }

}

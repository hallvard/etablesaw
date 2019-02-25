package etablesaw.xtext.jvmmodel;

import java.net.URI;
import java.net.URISyntaxException;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.Month;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XVariableDeclaration;
import org.eclipse.xtext.xbase.interpreter.IEvaluationContext;
import org.eclipse.xtext.xbase.interpreter.IEvaluationResult;
import org.eclipse.xtext.xbase.interpreter.impl.EvaluationException;
import org.eclipse.xtext.xbase.interpreter.impl.XbaseInterpreter;
import org.eclipse.xtext.xbase.jvmmodel.IJvmModelAssociations;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.references.StandardTypeReferenceOwner;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;

import com.google.inject.Inject;
import com.google.inject.Provider;

import etablesaw.xtext.xaw.InlineTableRow;
import etablesaw.xtext.xaw.TableColumn;
import etablesaw.xtext.xaw.TableLiteral;
import etablesaw.xtext.xaw.XCastedColumnExpression;
import etablesaw.xtext.xaw.XLocalDateLiteral;
import etablesaw.xtext.xaw.XLocalTimeLiteral;
import etablesaw.xtext.xaw.XMethod;
import etablesaw.xtext.xaw.XURLLiteral;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;

public class XawInterpreter extends XbaseInterpreter {

	private final Map<String, Table> topLevelTables = new HashMap<>();

	@Override
	public IEvaluationResult evaluate(final XExpression expression) {
		topLevelTables.clear();
		return super.evaluate(expression);
	}

	public Map<String, Table> getTopLevelTables() {
		return topLevelTables;
	}

	@Inject
	private IColumnTypeProvider columnTypeProvider;

	protected Object _doEvaluate(final TableLiteral tableLiteral, final IEvaluationContext context, final CancelIndicator indicator) {
		final List<Column<?>> columns = new ArrayList<Column<?>>();
		for (final TableColumn column : XawCompiler.getTableColumns(tableLiteral)) {
			if (column.getExpression() != null) {
				final Object result = internalEvaluate(column.getExpression(), context, indicator);
				columns.add((Column<?>) result);
			} else {
				final JvmTypeReference columnType = columnTypeProvider.getColumnTypeReference(column.getType());
				try {
					final Class<?> colClass = getJavaReflectAccess().getClassFinder().forName(columnType.getQualifiedName());
					final Object result = colClass.getMethod("create", new Class[]{ String.class }).invoke(null, new Object[]{ column.getName() });
					columns.add((Column<?>) result);
				} catch (final ClassCastException e) {
					throw e;
				} catch (final Exception e) {
					throw new RuntimeException("Exception when creating instance of " + columnType.getQualifiedName(), e);
				}
			}
		}
		for (final InlineTableRow row : XawCompiler.getTableRows(tableLiteral)) {
			int colNum = 0;
			for (final XExpression column : row.getExpressions()) {
				final Object result = internalEvaluate(column, context, indicator);
				final Column<?> column2 = columns.get(colNum);
				try {
					column2.getClass().getMethod("append", new Class[]{ result.getClass()}).invoke(column2, new Object[]{ result });
				} catch (final Exception e) {
					throw new RuntimeException("Exception when appending " + result + " to " + column2, e);
				}
				colNum++;
			}
		}
		final String name = tableLiteral.getName();
		final Table table = Table.create(name != null ? name : XawCompiler.defaultTableName, columns.toArray(new Column<?>[columns.size()]));
		return table;
	}

	@Inject
	private CommonTypeComputationServices typeComputationServices;

	// almost copied from XBaseInterpreter
	protected Object _doEvaluate(final XCastedColumnExpression castedExpression, final IEvaluationContext context, final CancelIndicator indicator) {
		final Object result = internalEvaluate(castedExpression.getTarget(), context, indicator);
		final StandardTypeReferenceOwner owner = new StandardTypeReferenceOwner(typeComputationServices, castedExpression);
		final JvmTypeReference castedType = columnTypeProvider.getColumnTypeReference(castedExpression.getType());
		final LightweightTypeReference targetType = owner.toLightweightTypeReference(castedType);
		final JvmType castType = castedType.getType();
		final String typeName = castType.getQualifiedName();
		Class<?> expectedType = null;
		try {
			expectedType = getJavaType(castType);
		} catch (final ClassNotFoundException e) {
			throw new EvaluationException(new NoClassDefFoundError(typeName));
		}
		try {
			expectedType.cast(result);
		} catch (final ClassCastException e) {
			throw new EvaluationException(new ClassCastException(typeName));
		}
		return result;
	}

	protected Object _doEvaluate(final XLocalTimeLiteral time, final IEvaluationContext context, final CancelIndicator indicator) {
		return createLocalTime(time);
	}

	public static LocalTime createLocalTime(final XLocalTimeLiteral time) {
		return LocalTime.of(time.getHour(), time.getMin(), time.getSecond());
	}

	protected Object _doEvaluate(final XLocalDateLiteral date, final IEvaluationContext context, final CancelIndicator indicator) {
		return createLocalDate(date);
	}

	public static LocalDate createLocalDate(final XLocalDateLiteral date) {
		try {
			final int month = Integer.valueOf(date.getMonth());
			return LocalDate.of(date.getYear(), month, date.getDay());
		} catch (final NumberFormatException e) {
			return LocalDate.of(date.getYear(), Month.valueOf(date.getMonth().toUpperCase()), date.getDay());
		}
	}

	protected Object _doEvaluate(final XURLLiteral url, final IEvaluationContext context, final CancelIndicator indicator) {
		try {
			return createURI(url);
		} catch (final URISyntaxException e) {
			throw new EvaluationException(e);
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

	@Override
	protected Object doEvaluate(final XExpression expression, final IEvaluationContext context, final CancelIndicator indicator) {
		if (expression instanceof TableLiteral) {
			return _doEvaluate((TableLiteral) expression, context, indicator);
		} else if (expression instanceof XCastedColumnExpression) {
			return _doEvaluate((XCastedColumnExpression) expression, context, indicator);
		} else if (expression instanceof XLocalTimeLiteral) {
			return _doEvaluate((XLocalTimeLiteral) expression, context, indicator);
		} else if (expression instanceof XLocalDateLiteral) {
			return _doEvaluate((XLocalDateLiteral) expression, context, indicator);
		} else if (expression instanceof XVariableDeclaration /*  && expression.eContainer() instanceof Xaw */) {
			final Object result = super.doEvaluate(expression, context, indicator);
			final String qualifiedName = ((XVariableDeclaration) expression).getQualifiedName();
			final Object value = context.getValue(QualifiedName.create(qualifiedName));
			if (value instanceof Table) {
				final Table table = (Table) value;
				if (XawCompiler.defaultTableName != table.name()) {
					topLevelTables.put(qualifiedName, table);
				}
			}
			return result;
		} else {
			return super.doEvaluate(expression, context, indicator);
		}
	}

	@Inject
	private IJvmModelAssociations jvmModelAssociations;

	@Inject
	private Provider<IEvaluationContext> contextProvider;

	@Override
	protected Object invokeOperation(final JvmOperation operation, final Object receiver, final List<Object> argumentValues) {
		final Collection<EObject> sourceElements = jvmModelAssociations.getSourceElements(operation);
		if (sourceElements != null && sourceElements.size() > 0) {
			final Object sourceElement = sourceElements.iterator().next();
			if (sourceElement instanceof XMethod) {
				final XMethod xMethod = (XMethod) sourceElement;
				final IEvaluationContext context = contextProvider.get();
				// context.newValue(QualifiedName.create("this"), null);
				int pos = 0;
				for (final JvmFormalParameter parameter : operation.getParameters()) {
					context.newValue(QualifiedName.create(parameter.getName()), argumentValues.get(pos));
					pos++;
				}
				IEvaluationResult result;
				try {
					result = evaluate(xMethod.getBody(), context, CancelIndicator.NullImpl);
					if (result.getException() != null) {
						throw new EvaluationException(result.getException());
					}
					return result.getResult();
					// catch clauses copied from super method
				} catch (final EvaluationException e) {
					throw e;
				}
			}
		}
		return super.invokeOperation(operation, receiver, argumentValues);
	}
}

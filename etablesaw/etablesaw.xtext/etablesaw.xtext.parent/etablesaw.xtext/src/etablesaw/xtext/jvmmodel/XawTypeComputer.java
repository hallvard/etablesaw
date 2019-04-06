package etablesaw.xtext.jvmmodel;

import java.net.URI;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.annotations.typesystem.XbaseWithAnnotationsTypeComputer;
import org.eclipse.xtext.xbase.typesystem.computation.ITypeComputationState;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

import com.google.inject.Inject;

import etablesaw.xtext.xaw.InlineTableRow;
import etablesaw.xtext.xaw.TableColumn;
import etablesaw.xtext.xaw.TableLiteral;
import etablesaw.xtext.xaw.XCastedColumnExpression;
import etablesaw.xtext.xaw.XLocalDateLiteral;
import etablesaw.xtext.xaw.XLocalTimeLiteral;
import etablesaw.xtext.xaw.XURLLiteral;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;

public class XawTypeComputer extends XbaseWithAnnotationsTypeComputer {

	@Inject
	private IColumnTypeProvider columnTypeProvider;

    @Inject
    private XawUtil xawUtil;

	@Override
	public void computeTypes(final XExpression expression, final ITypeComputationState state) {
		if (expression instanceof TableLiteral) {
			final TableLiteral literal = (TableLiteral) expression;
			final Collection<JvmTypeReference> colTypes = new ArrayList<JvmTypeReference>();
			for (final XExpression child : literal.getExpressions()) {
				if (child instanceof TableColumn) {
					final TableColumn column = (TableColumn) child;
					colTypes.add(column.getColumnDef().getType());
					final XExpression colExp = column.getExpression();
					if (colExp != null) {
						// TODO should expect a parameterized Column type
						final ITypeComputationState expressionState = state.withExpectation(getRawTypeForName(Column.class, state));
						expressionState.computeTypes(colExp);
					}
				} else if (child instanceof InlineTableRow) {
					final Iterator<JvmTypeReference> colTypesIt = colTypes.iterator();
					for (final XExpression column : ((InlineTableRow) child).getExpressions()) {
						final JvmTypeReference typeRef = colTypesIt.next();
						final ITypeComputationState expressionState = state.withExpectation(getTypeForName(typeRef.getQualifiedName(), state));
						expressionState.computeTypes(column);
					}
				}
			}
			String typeName = xawUtil.getTableTypeName(literal);
			final LightweightTypeReference result = (typeName != null ? getTypeForName(typeName, state) : getTypeForName(Table.class, state));
			state.acceptActualType(result);
		} else if (expression instanceof TableColumn) {
			super.computeTypes(expression, state);
		} else if (expression instanceof InlineTableRow) {
			super.computeTypes(expression, state);
		} else if (expression instanceof XCastedColumnExpression) {
			// almost copied from XBaseTypeCompiler
			final XCastedColumnExpression castedExpression = (XCastedColumnExpression) expression;
			final JvmTypeReference type = columnTypeProvider.getColumnTypeReference(castedExpression.getType());
			if (type != null) {
				state.withNonVoidExpectation().computeTypes(castedExpression.getTarget());
				state.acceptActualType(state.getReferenceOwner().toLightweightTypeReference(type));
			} else {
				state.computeTypes(castedExpression.getTarget());
			}
		} else if (expression instanceof XLocalTimeLiteral) {
			state.acceptActualType(getRawTypeForName(LocalTime.class, state));
		} else if (expression instanceof XLocalDateLiteral) {
			state.acceptActualType(getRawTypeForName(LocalDate.class, state));
		} else if (expression instanceof XURLLiteral) {
			state.acceptActualType(getRawTypeForName(URI.class, state));
		} else {
			super.computeTypes(expression, state);
		}
	}
}

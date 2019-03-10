package etablesaw.xtext.lib;

import java.util.function.Function;
import java.util.function.Predicate;

import org.eclipse.xtext.xbase.lib.IntegerRange;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Pure;

import tech.tablesaw.columns.Column;
import tech.tablesaw.selection.Selection;

public class ColumnExtensions {

	// selection/range

	public static <T> Column<T> operator_add(final Column<T> col1, final Column<T> col2) {
		return col1.append(col2);
	}

	public static <T> Column<T> operator_add(final Column<T> col1, final T item) {
		return col1.append(item);
	}

	public static <T> Column<T> operator_add(final Column<T> col1, final Pair<Column<T>, Integer> colRow) {
		return col1.append(colRow.getKey(), colRow.getValue());
	}

	public static <T> Column<T> operator_add(final Column<T> col1, final String s) {
		return col1.appendCell(s);
	}

	@Pure
	public static <T> Column<T> operator_minus(final Column<T> col, final Selection selection) {
		return col.where(SelectionExtensions.operator_not(selection));
	}

	@Pure
	public static <T> Column<T> operator_divide(final Column<T> col, final Selection selection) {
		return col.where(selection);
	}

	@Pure
	public static <T> Column<T> operator_divide(final Column<T> col, final IntegerRange range) {
		return col.inRange(range.getStart(), range.getEnd());
	}
	
	@Pure
    public static <T> Column<T> operator_divide(Column<T> numericColumn, Predicate<T> pred) {
        return numericColumn.filter(pred);
    }
	
	//

	public static <T> Column<T> mapInto(Column<T> column, Function<? super T, ? extends T> fun) {
	    return column.mapInto(fun, column);
	}
}

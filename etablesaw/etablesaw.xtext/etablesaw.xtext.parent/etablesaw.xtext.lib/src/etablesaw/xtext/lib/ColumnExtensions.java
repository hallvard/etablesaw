package etablesaw.xtext.lib;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;

import org.eclipse.xtext.xbase.lib.IntegerRange;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Pure;

import tech.tablesaw.columns.Column;
import tech.tablesaw.selection.Selection;

public class ColumnExtensions {

	public static <T1, T2> Map<T1, T2> toMap(Column<T1> col1, Column<T2> col2) {
		Map<T1, T2> newMap = new HashMap<>();
		Iterator<T1> it1 = col1.iterator();
		Iterator<T2> it2 = col2.iterator();
		while (it1.hasNext() && it2.hasNext()) {
			newMap.put(it1.next(), it2.next());
		}
		return newMap;
	}
	
	// selection/range

	public static <T> Column<T> operator_add(final Column<T> col1, final Column<T> col2) {
		return col1.append(col2);
	}

	public static <T> Column<T> operator_add(final Column<T> col1, final T item) {
		return item == null ? col1.appendMissing() : col1.append(item);
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
	public static <T> Column<T> operator_singleAnd(final Column<T> col, final Selection selection) {
		return col.where(selection);
	}

	@Pure
	public static <T> Column<T> operator_singleAnd(final Column<T> col, final IntegerRange range) {
		return col.inRange(range.getStart(), range.getEnd());
	}
	
	@Pure
    public static <T> Column<T> operator_singleAnd(Column<T> numericColumn, Predicate<T> pred) {
        return numericColumn.filter(pred);
    }

	// col1 -> fun => col2
	
    public static <T, R, C extends Column<R>> C operator_doubleArrow(Pair<Column<T>, Function<? super T, ? extends R>> columnFunctionPair, C into) {
        return columnFunctionPair.getKey().mapInto(columnFunctionPair.getValue(), into);
    }
}

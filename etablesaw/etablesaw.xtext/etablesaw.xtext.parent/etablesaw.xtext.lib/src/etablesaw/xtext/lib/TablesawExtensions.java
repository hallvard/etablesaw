package etablesaw.xtext.lib;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Collection;
import java.util.function.Predicate;

import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Functions.Function2;
import org.eclipse.xtext.xbase.lib.Functions.Function3;
import org.eclipse.xtext.xbase.lib.IntegerRange;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Pure;

import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;
import tech.tablesaw.selection.BitmapBackedSelection;
import tech.tablesaw.selection.Selection;

public class TablesawExtensions {

	public static <T1> Table1<T1> operator_add(final Table1<T1> table, final Table1<T1>.Row1 row) {
		table.append(row);
		return table;
	}
	public static <T1, T2> Table2<T1, T2> operator_add(final Table2<T1, T2> table, final Table2<T1, T2>.Row2 row) {
		table.append(row);
		return table;
	}
	public static <T1, T2, T3> Table3<T1, T2, T3> operator_add(final Table3<T1, T2, T3> table, final Table3<T1, T2, T3>.Row3 row) {
		table.append(row);
		return table;
	}
	public static <T1, T2, T3, T4> Table4<T1, T2, T3, T4> operator_add(final Table4<T1, T2, T3, T4> table, final Table4<T1, T2, T3, T4>.Row4 row) {
		table.append(row);
		return table;
	}
	public static <T1, T2, T3, T4, T5> Table5<T1, T2, T3, T4, T5> operator_add(final Table5<T1, T2, T3, T4, T5> table, final Table5<T1, T2, T3, T4, T5>.Row5 row) {
		table.append(row);
		return table;
	}
	public static <T1, T2, T3, T4, T5, T6> Table6<T1, T2, T3, T4, T5, T6> operator_add(final Table6<T1, T2, T3, T4, T5, T6> table, final Table6<T1, T2, T3, T4, T5, T6>.Row6 row) {
		table.append(row);
		return table;
	}

	// Selection

	@Pure
	public static <T> Selection eval(final Column<T> col, final Predicate<T> predicate) {
		final Selection selection = new BitmapBackedSelection();
		for (int idx = 0; idx < col.size(); idx++) {
			final T next = col.get(idx);
			if (predicate.test(next)) {
				selection.add(idx);
			}
		}
		return selection;
	}

	@Pure
	public static Selection operator_not(final Selection selection) {
		return selection.flip(0, selection.size());
	}

	public static Selection operator_divideAssign(final Selection sel1, final Selection sel2) {
		return sel1.and(sel2);
	}
	public static Selection operator_add(final Selection sel1, final Selection sel2) {
		return sel1.or(sel2);
	}
	public static Selection operator_remove(final Selection sel1, final Selection sel2) {
		return sel1.andNot(sel2);
	}

	public static Selection operator_add(final Selection sel1, final int row) {
		return sel1.add(row);
	}

	public static Selection operator_remove(final Selection sel1, final int row) {
		return sel1.removeRange(row, row);
	}

	public static Selection operator_add(final Selection sel1, final Collection<Integer> rows) {
		final int[] rowArray = new int[rows.size()];
		int pos = 0;
		for (final int row : rows) {
			rowArray[pos++] = row;
		}
		return sel1.add(rowArray);
	}

	public static Selection operator_add(final Selection sel1, final IntegerRange range) {
		return sel1.addRange(range.getStart(), range.getEnd());
	}

	public static Selection operator_remove(final Selection sel1, final IntegerRange range) {
		return sel1.removeRange(range.getStart(), range.getEnd());
	}

	// Tables and selection/range

	public static Table operator_add(final Table table1, final Table table2) {
		return table1.append(table2);
	}

	@Pure
	public static Table operator_divide(final Table table, final Selection selection) {
		return table.where(selection);
	}

	@Pure
	public static Table operator_minus(final Table table, final Selection selection) {
		return table.dropWhere(selection);
	}

	@Pure
	public static Table operator_divide(final Table table, final IntegerRange range) {
		return table.inRange(range.getStart(), range.getEnd());
	}

	// Columns and selection/range

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
		return col.where(operator_not(selection));
	}

	@Pure
	public static <T> Column<T> operator_divide(final Column<T> col, final Selection selection) {
		return col.where(selection);
	}

	@Pure
	public static <T> Column<T> operator_divide(final Column<T> col, final IntegerRange range) {
		return col.inRange(range.getStart(), range.getEnd());
	}

	//

	@Pure
	public static LocalDateTime operator_plus(final LocalDate date, final LocalTime time) {
		return LocalDateTime.of(date, time);
	}
	@Pure
	public static LocalDateTime operator_plus(final LocalTime time, final LocalDate date) {
		return LocalDateTime.of(date, time);
	}
	@Pure
	public static LocalDateTime operator_plus(final LocalDateTime dateTime, final LocalTime time) {
		LocalDateTime result = dateTime;
		if (time.getHour() > 0) {
			result = result.plusHours(time.getHour());
		}
		if (time.getMinute() > 0) {
			result = result.plusMinutes(time.getMinute());
		}
		if (time.getSecond() > 0) {
			result = result.plusSeconds(time.getSecond());
		}
		return result;
	}
	@Pure
	public static LocalDateTime operator_minus(final LocalDateTime dateTime, final LocalTime time) {
		LocalDateTime result = dateTime;
		if (time.getHour() > 0) {
			result = result.minusHours(time.getHour());
		}
		if (time.getMinute() > 0) {
			result = result.minusMinutes(time.getMinute());
		}
		if (time.getSecond() > 0) {
			result = result.minusSeconds(time.getSecond());
		}
		return result;
	}
	@Pure
	public static LocalDateTime operator_divide(final LocalDateTime dateTime, final LocalTime time) {
		LocalDateTime result = dateTime;
		if (dateTime.getHour() != time.getHour()) {
			result = result.withHour(time.getHour());
		}
		if (dateTime.getMinute() != time.getMinute()) {
			result = result.withMinute(time.getMinute());
		}
		if (dateTime.getSecond() != time.getSecond()) {
			result = result.withSecond(time.getSecond());
		}
		return result;
	}
	@Pure
	public static LocalDateTime operator_divide(final LocalDateTime dateTime, final LocalDate date) {
		LocalDateTime result = dateTime;
		if (dateTime.getYear() != date.getYear()) {
			result = result.withYear(date.getYear());
		}
		if (dateTime.getMonthValue() != date.getMonthValue()) {
			result = result.withMonth(date.getMonthValue());
		}
		if (dateTime.getDayOfMonth() != date.getDayOfMonth()) {
			result = result.withDayOfMonth(date.getDayOfMonth());
		}
		return result;
	}

	// map

	public <C1, R> Column<R> mapInto(final Column<C1> col1, final Column<R> into, final Function1<C1, R> fun) {
		for (int i = 0; i < col1.size(); i++) {
			try {
				into.set(i, fun.apply(col1.get(i)));
			} catch (final Exception e) {
				into.setMissing(i);
			}
		}
		return into;
	}

	public <C1, C2, R> Column<R> mapInto(final Column<C1> col1, final Column<C2> col2, final Column<R> into, final Function2<C1, C2, R> fun) {
		for (int i = 0; i < col1.size() && i < col2.size(); i++) {
			try {
				into.set(i, fun.apply(col1.get(i), col2.get(i)));
			} catch (final Exception e) {
				into.setMissing(i);
			}
		}
		return into;
	}

	public <C1, C2, C3, R> Column<R> mapInto(final Column<C1> col1, final Column<C2> col2, final Column<C3> col3, final Column<R> into, final Function3<C1, C2, C3, R> fun) {
		for (int i = 0; i < col1.size() && i < col2.size() && i < col3.size(); i++) {
			try {
				into.set(i, fun.apply(col1.get(i), col2.get(i), col3.get(i)));
			} catch (final Exception e) {
				into.setMissing(i);
			}
		}
		return into;
	}
}

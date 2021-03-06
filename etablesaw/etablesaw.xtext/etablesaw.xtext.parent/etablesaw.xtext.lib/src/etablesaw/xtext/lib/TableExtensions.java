package etablesaw.xtext.lib;

import java.util.function.Function;
import java.util.function.Predicate;

import org.eclipse.xtext.xbase.lib.IntegerRange;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Pure;

import etablesaw.xtext.lib.ExampleTable.RowData;
import tech.tablesaw.api.Row;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;
import tech.tablesaw.selection.BitmapBackedSelection;
import tech.tablesaw.selection.Selection;
import tech.tablesaw.table.Rows;

public class TableExtensions {

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

	// selection/range

	@Pure
    public static <R extends TypedRow<R>> TypedTable<R> operator_singleAnd(final TypedTable<R> table, final Predicate<R> predicate) {
        Selection selection = table.eval(predicate);
        TypedTable<R> newTable = table.emptyCopy(selection.size());
        Rows.copyRowsToTable(selection, table, newTable);
        return newTable;
    }

	public static <T extends Table> T operator_add(final T table1, final Table table2) {
		table1.append(table2);
		return table1;
	}

	public static <T extends Table> T operator_add(final T table, final Column<?> column) {
	    table.addColumns(column);
	    return table;
	}

	public static <R extends TypedRow<R>> R operator_add(final TypedTable<R> table, final R row) {
    	R newRow = table.appendEmptyRow();
    	row.copyInto(newRow);
    	return newRow;
	}

	@Pure
	public static <T extends Table> T operator_singleAnd(final T table, final Selection selection) {
        T newTable = (T) table.emptyCopy(selection.size());
        Rows.copyRowsToTable(selection, table, newTable);
		return newTable;
	}

	@Pure
	public static <T extends Table> T  operator_minus(final T table, final Selection selection) {
        Selection opposite = new BitmapBackedSelection();
        opposite.addRange(0, table.rowCount());
        opposite.andNot(selection);
        T newTable = (T) table.emptyCopy(opposite.size());
        Rows.copyRowsToTable(opposite, table, newTable);
        return newTable;
	}

	@Pure
	public static Table operator_singleAnd(final Table table, final IntegerRange range) {
		return table.inRange(range.getStart(), range.getEnd());
	}
	
	@Pure
	public static Table operator_minus(final Table table, final IntegerRange range) {
	    return table.dropRange(range.getStart(), range.getEnd());
	}
	
	// table1 => table2

    public static <T extends Table> T operator_doubleArrow(final Table table1, final T table2) {
        table2.append(table1);
        return table2;
    }
    
    // table -> fun => column

    public static <R extends TypedRow<R>, T, C extends Column<T>> C operator_doubleArrow(Pair<TypedTable<R>, Function<? super R, ? extends T>> tableFunctionPair, C into) {
        int rowNum = 0;
        for (R row : tableFunctionPair.getKey().rows()) {
            into.set(rowNum, tableFunctionPair.getValue().apply(row));
        }
        return into;
    }
}

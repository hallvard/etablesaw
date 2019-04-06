package etablesaw.xtext.lib;

import org.eclipse.xtext.xbase.lib.IntegerRange;
import org.eclipse.xtext.xbase.lib.Pure;

import tech.tablesaw.api.Table;
import tech.tablesaw.selection.Selection;

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

	public static <T extends Table> T operator_add(final T table1, final Table table2) {
		table1.append(table2);
		return table1;
	}

	@Pure
	public static Table operator_singleAnd(final Table table, final Selection selection) {
		return table.where(selection);
	}

	@Pure
	public static Table operator_minus(final Table table, final Selection selection) {
		return table.dropWhere(selection);
	}

	@Pure
	public static Table operator_singleAnd(final Table table, final IntegerRange range) {
		return table.inRange(range.getStart(), range.getEnd());
	}
	
	@Pure
	public static Table operator_minus(final Table table, final IntegerRange range) {
	    return table.dropRange(range.getStart(), range.getEnd());
	}
	
	// =>

    public static <T extends Table> T operator_doubleArrow(final Table table1, final T table2) {
        table2.append(table1);
        return table2;
    }
}

package etablesaw.xtext.lib;

import tech.tablesaw.columns.Column;

public class Table6<T1, T2, T3, T4, T5, T6> extends Table5<T1, T2, T3, T4, T5> {

	public Table6(final String name, final Column<T1> col1, final Column<T2> col2, final Column<T3> col3, final Column<T4> col4, final Column<T5> col5, final Column<T6> col6) {
		super(name, col1, col2, col3, col4, col5);
		addColumns(col6);
	}

	@Override
	public Row6 row() {
		return new Row6(this);
	}

	public Column<T6> column6() {
		return (Column<T6>) column(5);
	}

	public class Row6 extends Row5 {

		public Row6(final Table6<T1, T2, T3, T4, T5, T6> table) {
			super(table);
		}

		public T6 value6() {
			return column6().get(getRowNumber());
		}
	}

	public void append(final Row6 row) {
		super.append(row);
		column6().append(row.value6());
	}
}

package etablesaw.xtext.lib;

import tech.tablesaw.columns.Column;

public class Table5<T1, T2, T3, T4, T5> extends Table4<T1, T2, T3, T4> {

	public Table5(final String name, final Column<T1> col1, final Column<T2> col2, final Column<T3> col3, final Column<T4> col4, final Column<T5> col5) {
		super(name, col1, col2, col3, col4);
		addColumns(col5);
	}

	@Override
	public Row5 row() {
		return new Row5(this);
	}

	public Column<T5> column5() {
		return (Column<T5>) column(4);
	}

	public class Row5 extends Row4 {

		protected Row5(final Table5<T1, T2, T3, T4, T5> table) {
			super(table);
		}

		public T5 value5() {
			return column5().get(getRowNumber());
		}
	}

	public void append(final Row5 row) {
		super.append(row);
		column5().append(row.value5());
	}
}

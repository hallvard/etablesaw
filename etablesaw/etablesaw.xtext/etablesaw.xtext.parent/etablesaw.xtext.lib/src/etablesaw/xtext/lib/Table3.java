package etablesaw.xtext.lib;

import tech.tablesaw.columns.Column;

public class Table3<T1, T2, T3> extends Table2<T1, T2> {

	public Table3(final String name, final Column<T1> col1, final Column<T2> col2, final Column<T3> col3) {
		super(name, col1, col2);
		addColumns(col3);
	}

	@Override
	public Row3 row() {
		return new Row3(this);
	}

	public Column<T3> column3() {
		return (Column<T3>) column(2);
	}

	public class Row3 extends Row2 {

		protected Row3(final Table3<T1, T2, T3> table) {
			super(table);
		}

		public T3 value3() {
			return column3().get(getRowNumber());
		}
	}

	public void append(final Row3 row) {
		super.append(row);
		column3().append(row.value3());
	}
}

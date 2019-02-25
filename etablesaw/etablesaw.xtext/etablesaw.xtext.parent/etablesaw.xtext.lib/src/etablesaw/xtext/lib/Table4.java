package etablesaw.xtext.lib;

import tech.tablesaw.columns.Column;

public class Table4<T1, T2, T3, T4> extends Table3<T1, T2, T3> {

	public Table4(final String name, final Column<T1> col1, final Column<T2> col2, final Column<T3> col3, final Column<T4> col4) {
		super(name, col1, col2, col3);
		addColumns(col4);
	}

	@Override
	public Row4 row() {
		return new Row4(this);
	}

	public Column<T4> column4() {
		return (Column<T4>) column(3);
	}

	public class Row4 extends Row3 {

		protected Row4(final Table4<T1, T2, T3, T4> table) {
			super(table);
		}

		public T4 value4() {
			return column4().get(getRowNumber());
		}
	}

	public void append(final Row4 row) {
		super.append(row);
		column4().append(row.value4());
	}
}

package etablesaw.xtext.lib;

import tech.tablesaw.columns.Column;

public class Table2<T1, T2> extends Table1<T1> {

	public Table2(final String name, final Column<T1> col1, final Column<T2> col2) {
		super(name, col1);
		addColumns(col2);
	}

	@Override
	public Row2 row() {
		return new Row2(this);
	}

	public Column<T2> column2() {
		return (Column<T2>) column(1);
	}

	public class Row2 extends Row1 {

		protected Row2(final Table2<T1, T2> table) {
			super(table);
		}

		public T2 value2() {
			return column2().get(getRowNumber());
		}
	}

	public void append(final Row2 row) {
		super.append(row);
		column2().append(row.value2());
	}
}

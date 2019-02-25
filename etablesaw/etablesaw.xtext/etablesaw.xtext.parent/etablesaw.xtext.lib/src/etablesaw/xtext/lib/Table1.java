package etablesaw.xtext.lib;

import tech.tablesaw.api.Row;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;

public class Table1<T1> extends Table {

	public Table1(final String name, final Column<T1> col1) {
		super(name);
		addColumns(col1);
	}

	public Row1 row() {
		return new Row1(this);
	}

	public Column<T1> column1() {
		return (Column<T1>) column(0);
	}

	public class Row1 extends Row {

		protected Row1(final Table1<T1> table) {
			super(table);
		}

		public T1 value1() {
			return column1().get(getRowNumber());
		}
	}

	public void append(final Row1 row) {
		column1().append(row.value1());
	}
}

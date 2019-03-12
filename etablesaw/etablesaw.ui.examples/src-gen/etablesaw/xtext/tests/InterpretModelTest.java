package etablesaw.xtext.tests;

import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.StringColumn;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;

@SuppressWarnings("all")
public class InterpretModelTest implements Runnable {
  public void run() {
    final double halAge = 52.0;
    StringColumn nameColumn = StringColumn.create("name");
    DoubleColumn ageColumn = DoubleColumn.create("age");
    nameColumn.append("Hallvard");
    ageColumn.append(halAge);
    final Table table1 = Table.create("table2", nameColumn, ageColumn);
    StringColumn nameColumn_1 = StringColumn.create("name");
    DoubleColumn ageColumn_1 = DoubleColumn.create("age");
    nameColumn_1.append("Hallvard");
    ageColumn_1.append((halAge + 1));
    final Table table2 = Table.create("table3", nameColumn_1, ageColumn_1);
    Column<?> _column = table1.column("name");
    Column<?> _column_1 = table2.column("age");
    final Table table3 = Table.create("A table", _column, _column_1);
    /* table3; */
  }
  
  public static void main(final String[] args) {
    new InterpretModelTest().run();
  }
}

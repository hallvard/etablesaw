package etablesaw.xtext.tests;

import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.StringColumn;
import tech.tablesaw.api.Table;

@SuppressWarnings("all")
public class LoadModelTest implements Runnable {
  public void run() {
    final double halAge = 52.0;
    StringColumn nameColumn = StringColumn.create("name");
    DoubleColumn ageColumn = DoubleColumn.create("age");
    nameColumn.append("Hallvard");
    ageColumn.append(halAge);
    final Table table1 = Table.create("A table", nameColumn, ageColumn);
    this.helper1(table1.column("age").get(0));
  }
  
  public static void main(final String[] args) {
    new LoadModelTest().run();
  }
  
  private String helper1(final Object o) {
    String _valueOf = String.valueOf(o);
    return _valueOf;
  }
}

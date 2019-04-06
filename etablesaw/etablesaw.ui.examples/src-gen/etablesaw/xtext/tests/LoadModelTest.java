package etablesaw.xtext.tests;

import etablesaw.xtext.lib.XawBase;
import etablesaw.xtext.tests.LoadModelTest_table1;
import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.StringColumn;

@SuppressWarnings("all")
public class LoadModelTest extends XawBase implements Runnable {
  public void run() {
    final double halAge = 52.0;
    StringColumn nameColumn = StringColumn.create("name");
    DoubleColumn ageColumn = DoubleColumn.create("age");
    nameColumn.append("Hallvard");
    ageColumn.append(halAge);
    final LoadModelTest_table1 table1 = new etablesaw.xtext.tests.LoadModelTest_table1("A table", nameColumn, ageColumn);
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

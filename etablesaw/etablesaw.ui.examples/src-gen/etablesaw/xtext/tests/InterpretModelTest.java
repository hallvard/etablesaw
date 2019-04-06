package etablesaw.xtext.tests;

import etablesaw.xtext.lib.XawBase;
import etablesaw.xtext.tests.InterpretModelTest_table2;
import etablesaw.xtext.tests.InterpretModelTest_table3;
import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.StringColumn;

@SuppressWarnings("all")
public class InterpretModelTest extends XawBase implements Runnable {
  public void run() {
    final double halAge = 52.0;
    StringColumn nameColumn = StringColumn.create("name");
    DoubleColumn ageColumn = DoubleColumn.create("age");
    nameColumn.append("Hallvard");
    ageColumn.append(halAge);
    final InterpretModelTest_table2 table1 = new etablesaw.xtext.tests.InterpretModelTest_table2("table2", nameColumn, ageColumn);
    StringColumn nameColumn_1 = StringColumn.create("name");
    DoubleColumn ageColumn_1 = DoubleColumn.create("age");
    nameColumn_1.append("Hallvard");
    ageColumn_1.append((halAge + 1));
    final InterpretModelTest_table3 table2 = new etablesaw.xtext.tests.InterpretModelTest_table3("table3", nameColumn_1, ageColumn_1);
    StringColumn _nameColumn = table1.getNameColumn();
    DoubleColumn _ageColumn = table2.getAgeColumn();
    final InterpretModelTest_table3 table3 = new etablesaw.xtext.tests.InterpretModelTest_table3("A table", _nameColumn, _ageColumn);
    /* table3; */
  }
  
  public static void main(final String[] args) {
    new InterpretModelTest().run();
  }
}

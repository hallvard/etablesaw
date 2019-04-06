package tablesaw.ui.examples;

import etablesaw.xtext.lib.LocalDateTimeExtensions;
import etablesaw.xtext.lib.XawBase;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import tablesaw.ui.examples.Test1_tab1;
import tablesaw.ui.examples.Test1_tab2;
import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.IntColumn;
import tech.tablesaw.api.StringColumn;
import tech.tablesaw.api.Table;

@SuppressWarnings("all")
public class Test1 extends XawBase implements Runnable {
  public void run() {
    final String var1 = "Aud";
    final int var2 = 1;
    final LocalTime time = java.time.LocalTime.of(23, 53, 0);
    final LocalDate date1 = java.time.LocalDate.of(1966, java.time.Month.NOVEMBER, 16);
    final boolean later = (date1.compareTo(java.time.LocalDate.of(1966, java.time.Month.NOVEMBER, 17)) > 0);
    final LocalDate date2 = java.time.LocalDate.of(1966, 11, 16);
    final LocalDateTime dateTime1 = LocalDateTimeExtensions.operator_plus(date2, java.time.LocalTime.of(23, 53, 0));
    final LocalDateTime dateTime2 = LocalDateTimeExtensions.operator_plus(time, java.time.LocalDate.of(1966, java.time.Month.NOVEMBER, 16));
    StringColumn nameColumn = StringColumn.create("name");
    DoubleColumn ageColumn = DoubleColumn.create("age");
    nameColumn.append("Hallvard");
    ageColumn.append((50 + var2));
    nameColumn.append(var1);
    ageColumn.append(80);
    final Test1_tab1 tab1 = new tablesaw.ui.examples.Test1_tab1("A table", nameColumn, ageColumn);
    final StringColumn nameCol = tab1.getNameColumn();
    IntColumn _create = IntColumn.create("age", new int[] { 42, 48 });
    final Test1_tab2 tab2 = new tablesaw.ui.examples.Test1_tab2("tab2", nameCol, _create);
    this.helper(tab1);
  }
  
  public static void main(final String[] args) {
    new Test1().run();
  }
  
  private void helper(final Table table) {
    System.out.println(table);
  }
}

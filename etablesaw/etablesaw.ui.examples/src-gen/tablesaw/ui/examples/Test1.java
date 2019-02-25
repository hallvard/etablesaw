package tablesaw.ui.examples;

import etablesaw.xtext.lib.TablesawExtensions;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.function.Predicate;
import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.ShortColumn;
import tech.tablesaw.api.StringColumn;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;
import tech.tablesaw.selection.Selection;

@SuppressWarnings("all")
public class Test1 implements Runnable {
  public void run() {
    final String var1 = "Aud";
    final int var2 = 1;
    final LocalTime time = java.time.LocalTime.of(23, 53, 0);
    final LocalDate date1 = java.time.LocalDate.of(1966, java.time.Month.NOVEMBER, 16);
    final boolean later = (date1.compareTo(java.time.LocalDate.of(1966, java.time.Month.NOVEMBER, 17)) > 0);
    final LocalDate date2 = java.time.LocalDate.of(1966, 11, 16);
    final LocalDateTime dateTime1 = TablesawExtensions.operator_plus(date2, java.time.LocalTime.of(23, 53, 0));
    final LocalDateTime dateTime2 = TablesawExtensions.operator_plus(time, java.time.LocalDate.of(1966, java.time.Month.NOVEMBER, 16));
    StringColumn nameColumn = StringColumn.create("name");
    DoubleColumn ageColumn = DoubleColumn.create("age");
    nameColumn.append("Hallvard");
    ageColumn.append((50 + var2));
    nameColumn.append(var1);
    ageColumn.append(80);
    final Table tab1 = Table.create("A table", nameColumn, ageColumn);
    Column<?> _column = tab1.column("name");
    ShortColumn ageColumn_1 = ShortColumn.create("age");
    final Table tab2 = Table.create("tab2", ((StringColumn) _column), ageColumn_1);
    Column<?> _column_1 = tab2.column("age");
    ((ShortColumn) _column_1).append(((short) 42));
    Column<?> _column_2 = tab2.column("age");
    ((ShortColumn) _column_2).append(((short) 48));
    Column<?> _column_3 = tab1.column("name");
    final StringColumn stringCol = ((StringColumn) _column_3);
    final Predicate<String> _function = (String s) -> {
      return s.startsWith("H");
    };
    Selection _eval = stringCol.eval(_function);
    final Table tab3 = TablesawExtensions.operator_minus(tab2, _eval);
  }
  
  public static void main(final String[] args) {
    new Test1().run();
  }
  
  private void helper(final Table table) {
    System.out.println(table);
  }
}

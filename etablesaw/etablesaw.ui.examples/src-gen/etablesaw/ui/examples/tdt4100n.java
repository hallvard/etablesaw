package etablesaw.ui.examples;

import etablesaw.xtext.lib.ColumnExtensions;
import java.util.List;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Pair;
import tech.tablesaw.api.IntColumn;
import tech.tablesaw.api.ShortColumn;
import tech.tablesaw.api.StringColumn;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;
import tech.tablesaw.io.csv.CsvReadOptions;

@SuppressWarnings("all")
public class tdt4100n implements Runnable {
  public void run() {
    StringColumn programkodeColumn = StringColumn.create("programkode");
    IntColumn studentnrColumn = IntColumn.create("studentnr");
    StringColumn karakterColumn = StringColumn.create("karakter");
    StringColumn vurderingsstatusColumn = StringColumn.create("vurderingsstatus");
    ShortColumn yearColumn = ShortColumn.create("year");
    StringColumn semesterColumn = StringColumn.create("semester");
    final Table tdt4100 = Table.create("tdt4100", programkodeColumn, studentnrColumn, karakterColumn, vurderingsstatusColumn, yearColumn, semesterColumn);
    final String here = "/Users/hal/java/git/etablesaw/etablesaw/etablesaw.ui.examples/src/etablesaw/ui/examples/";
    final String nameFormat = "tdt4100-%2$s%1$s";
    for (int year = 2010; (year <= 2018); year++) {
      {
        final Table tdt4100n = this.readCsv(nameFormat, ((short) year), "v", here);
        this.appendColumns(tdt4100, tdt4100n, ((short) year), "v");
      }
    }
    for (int year = 2010; (year <= 2018); year++) {
      {
        final Table tdt4100n = this.readCsv(nameFormat, ((short) year), "s", here);
        this.appendColumns(tdt4100, tdt4100n, ((short) year), "s");
      }
    }
    final Table sorted = tdt4100.sortOn("programkode", "studentnr", "year", "-semester");
    int _size = IterableExtensions.size(sorted);
    List<Column<?>> _columns = sorted.columns();
    /* Pair.<Integer, List<Column<?>>>of(Integer.valueOf(_size), _columns); */
  }
  
  public static void main(final String[] args) {
    new tdt4100n().run();
  }
  
  private Table readCsv(final String nameFormat, final short year, final String semester, final String dir) {
    try {
      Table _xblockexpression = null;
      {
        final String name = String.format(nameFormat, Short.valueOf(year), semester);
        _xblockexpression = Table.read().csv(CsvReadOptions.builder(((dir + name) + ".csv")).tableName(name).separator(';').build());
      }
      return _xblockexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  private Column<String> appendColumns(final Table tdt4100, final Table tdt4100n, final short year, final String semester) {
    Column<String> _xblockexpression = null;
    {
      Column<?> _column = tdt4100.column("programkode");
      Column<?> _column_1 = tdt4100n.column("studieprogramkode");
      ColumnExtensions.<String>operator_add(
        ((StringColumn) _column), ((StringColumn) _column_1));
      Column<?> _column_2 = tdt4100n.column("studentnr");
      final IntColumn studnrCol = ((IntColumn) _column_2);
      Column<?> _column_3 = tdt4100.column("studentnr");
      ColumnExtensions.<Integer>operator_add(
        ((IntColumn) _column_3), studnrCol);
      Column<?> _column_4 = tdt4100.column("karakter");
      Column<?> _column_5 = tdt4100n.column("karakter");
      ColumnExtensions.<String>operator_add(
        ((StringColumn) _column_4), ((StringColumn) _column_5));
      Column<?> _column_6 = tdt4100.column("vurderingsstatus");
      Column<?> _column_7 = tdt4100n.column("vurdresstatnavn");
      ColumnExtensions.<String>operator_add(
        ((StringColumn) _column_6), ((StringColumn) _column_7));
      final ShortColumn yearCol = ShortColumn.create("year", studnrCol.size());
      yearCol.set(studnrCol.isNotMissing(), Short.valueOf(year));
      Column<?> _column_8 = tdt4100.column("year");
      ColumnExtensions.<Short>operator_add(
        ((ShortColumn) _column_8), yearCol);
      final StringColumn semesterCol = StringColumn.create("semester", studnrCol.size());
      semesterCol.set(studnrCol.isNotMissing(), semester);
      Column<?> _column_9 = tdt4100.column("semester");
      _xblockexpression = ColumnExtensions.<String>operator_add(
        ((StringColumn) _column_9), semesterCol);
    }
    return _xblockexpression;
  }
}

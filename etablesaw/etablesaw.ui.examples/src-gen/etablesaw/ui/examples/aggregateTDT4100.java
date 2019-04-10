package etablesaw.ui.examples;

import com.google.common.base.Objects;
import etablesaw.ui.examples.aggregateTDT4100_AggregateTable;
import etablesaw.ui.examples.aggregateTDT4100_Tdt4100Table;
import etablesaw.xtext.lib.TableExtensions;
import etablesaw.xtext.lib.XawBase;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import tech.tablesaw.api.Table;

@SuppressWarnings("all")
public class aggregateTDT4100 extends XawBase {
  public void run() {
    try {
      final String here = "/Users/hal/java/git/etablesaw/etablesaw/etablesaw.ui.examples/src/etablesaw/ui/examples/";
      Table _csv = Table.read().csv((here + "tdt4100-2010-2018.csv"));
      final aggregateTDT4100_Tdt4100Table tdt4100 = TableExtensions.<aggregateTDT4100_Tdt4100Table>operator_add(new Function0<aggregateTDT4100_Tdt4100Table>() {
        public aggregateTDT4100_Tdt4100Table apply() {
          return new etablesaw.ui.examples.aggregateTDT4100_Tdt4100Table("Tdt4100Table");
        }
      }.apply(), _csv);
      final aggregateTDT4100_AggregateTable aggregate = new etablesaw.ui.examples.aggregateTDT4100_AggregateTable("AggregateTable");
      int currentStud = 0;
      aggregateTDT4100_Tdt4100Table currentRows = null;
      Iterable<aggregateTDT4100_Tdt4100Table.Row> _rows = tdt4100.rows();
      for (final aggregateTDT4100_Tdt4100Table.Row row : _rows) {
        {
          int _studentnr = row.getStudentnr();
          boolean _tripleNotEquals = (_studentnr != currentStud);
          if (_tripleNotEquals) {
            if (((currentRows != null) && (currentRows.rowCount() > 0))) {
              this.aggregate(currentRows, aggregate);
            }
            currentRows = tdt4100.emptyCopy();
          }
          currentRows.append(row);
          currentStud = row.getStudentnr();
        }
      }
      if (((currentRows != null) && (currentRows.rowCount() > 0))) {
        this.aggregate(currentRows, aggregate);
      }
      int _rowCount = tdt4100.rowCount();
      String _plus = ("Reduced from " + Integer.valueOf(_rowCount));
      String _plus_1 = (_plus + " to ");
      int _rowCount_1 = aggregate.rowCount();
      String _plus_2 = (_plus_1 + Integer.valueOf(_rowCount_1));
      String _plus_3 = (_plus_2 + " rows");
      System.out.println(_plus_3);
      this.exportTable(aggregate, "aggregate");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public static void main(final String[] args) {
    new aggregateTDT4100().run();
  }
  
  private void aggregate(final aggregateTDT4100_Tdt4100Table exams, final aggregateTDT4100_AggregateTable aggregate) {
    int gradeYear = 0;
    final aggregateTDT4100_AggregateTable.Row aggreateRow = aggregate.appendEmptyRow();
    aggreateRow.setFailCount(0);
    aggreateRow.setGrade("");
    Iterable<aggregateTDT4100_Tdt4100Table.Row> _rows = exams.rows();
    for (final aggregateTDT4100_Tdt4100Table.Row row : _rows) {
      {
        aggreateRow.setProgramkode(row.getProgramkode());
        aggreateRow.setStudentnr(row.getStudentnr());
        int _year = aggreateRow.getYear();
        boolean _lessEqualsThan = (_year <= 0);
        if (_lessEqualsThan) {
          aggreateRow.setYear(row.getYear());
        }
        boolean _isFail = this.isFail(row.getKarakter());
        if (_isFail) {
          int _failCount = aggreateRow.getFailCount();
          int _plus = (_failCount + 1);
          aggreateRow.setFailCount(_plus);
        }
        if ((aggreateRow.getGrade().isEmpty() || ((row.getKarakter().length() > 0) && (row.getKarakter().compareTo(aggreateRow.getGrade()) < 0)))) {
          if ((this.isPass(aggreateRow.getGrade()) && this.isPass(row.getKarakter()))) {
            int _year_1 = row.getYear();
            int _minus = (_year_1 - gradeYear);
            aggreateRow.setTimeToRetake(_minus);
          }
          aggreateRow.setGrade(row.getKarakter());
          gradeYear = row.getYear();
        }
      }
    }
  }
  
  private boolean isPass(final String karakter) {
    return ((karakter.length() > 0) && ("ABCDE".indexOf(karakter) >= 0));
  }
  
  private boolean isNotPass(final String karakter) {
    int _indexOf = "ABCDE".indexOf(karakter);
    return (_indexOf < 0);
  }
  
  private boolean isFail(final String karakter) {
    return Objects.equal(karakter, "F");
  }
}

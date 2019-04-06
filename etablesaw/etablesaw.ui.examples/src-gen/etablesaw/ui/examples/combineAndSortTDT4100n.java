package etablesaw.ui.examples;

import etablesaw.ui.examples.combineAndSortTDT4100n_tdt4100;
import etablesaw.ui.examples.combineAndSortTDT4100n_tdt4100n;
import etablesaw.xtext.lib.ColumnExtensions;
import etablesaw.xtext.lib.TableExtensions;
import etablesaw.xtext.lib.XawBase;
import java.util.Collections;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Exceptions;
import tech.tablesaw.api.IntColumn;
import tech.tablesaw.api.StringColumn;
import tech.tablesaw.api.Table;
import tech.tablesaw.io.csv.CsvReadOptions;
import tech.tablesaw.io.csv.CsvWriteOptions;

@SuppressWarnings("all")
public class combineAndSortTDT4100n extends XawBase implements Runnable {
  public void run() {
    try {
      StringColumn programkodeColumn = StringColumn.create("programkode");
      IntColumn studentnrColumn = IntColumn.create("studentnr");
      StringColumn karakterColumn = StringColumn.create("karakter");
      StringColumn vurderingsstatusColumn = StringColumn.create("vurderingsstatus");
      IntColumn yearColumn = IntColumn.create("year");
      StringColumn semesterColumn = StringColumn.create("semester");
      final combineAndSortTDT4100n_tdt4100 tdt4100 = new etablesaw.ui.examples.combineAndSortTDT4100n_tdt4100("A table", programkodeColumn, studentnrColumn, karakterColumn, vurderingsstatusColumn, yearColumn, semesterColumn);
      final String here = "/Users/hal/java/git/etablesaw/etablesaw/etablesaw.ui.examples/src/etablesaw/ui/examples/";
      final String nameFormat = "tdt4100-%2$s%1$s";
      for (final String semester : Collections.<String>unmodifiableList(CollectionLiterals.<String>newArrayList("v", "s"))) {
        for (int year = 2010; (year <= 2018); year++) {
          {
            final String name = String.format(nameFormat, Integer.valueOf(year), semester);
            StringColumn studieprogramkodeColumn = StringColumn.create("studieprogramkode");
            IntColumn studentnrColumn_1 = IntColumn.create("studentnr");
            StringColumn karakterColumn_1 = StringColumn.create("karakter");
            StringColumn vurdresstatnavnColumn = StringColumn.create("vurdresstatnavn");
            final combineAndSortTDT4100n_tdt4100n tdt4100n = new etablesaw.ui.examples.combineAndSortTDT4100n_tdt4100n("A table", studieprogramkodeColumn, studentnrColumn_1, karakterColumn_1, vurdresstatnavnColumn);
            Table _csv = Table.read().csv(CsvReadOptions.builder(((here + name) + ".csv")).separator(Character.valueOf(';')).build());
            TableExtensions.<combineAndSortTDT4100n_tdt4100n>operator_add(tdt4100n, _csv);
            StringColumn _programkodeColumn = tdt4100.getProgramkodeColumn();
            StringColumn _studieprogramkodeColumn = tdt4100n.getStudieprogramkodeColumn();
            ColumnExtensions.<String>operator_add(_programkodeColumn, _studieprogramkodeColumn);
            IntColumn _studentnrColumn = tdt4100.getStudentnrColumn();
            IntColumn _studentnrColumn_1 = tdt4100n.getStudentnrColumn();
            ColumnExtensions.<Integer>operator_add(_studentnrColumn, _studentnrColumn_1);
            StringColumn _karakterColumn = tdt4100.getKarakterColumn();
            StringColumn _karakterColumn_1 = tdt4100n.getKarakterColumn();
            ColumnExtensions.<String>operator_add(_karakterColumn, _karakterColumn_1);
            StringColumn _vurderingsstatusColumn = tdt4100.getVurderingsstatusColumn();
            StringColumn _vurdresstatnavnColumn = tdt4100n.getVurdresstatnavnColumn();
            ColumnExtensions.<String>operator_add(_vurderingsstatusColumn, _vurdresstatnavnColumn);
            IntColumn _yearColumn = tdt4100.getYearColumn();
            IntColumn _set = IntColumn.create("year", tdt4100n.getStudentnrColumn().size()).set(tdt4100n.getStudentnrColumn().isNotMissing(), Integer.valueOf(year));
            ColumnExtensions.<Integer>operator_add(_yearColumn, _set);
            final StringColumn semesterCol = StringColumn.create("semester", tdt4100n.getStudentnrColumn().size());
            semesterCol.set(tdt4100n.getStudentnrColumn().isNotMissing(), semester);
            StringColumn _semesterColumn = tdt4100.getSemesterColumn();
            ColumnExtensions.<String>operator_add(_semesterColumn, semesterCol);
          }
        }
      }
      final Table sorted = tdt4100.sortOn("programkode", "studentnr", "year", "-semester");
      sorted.write().csv(CsvWriteOptions.builder((here + "tdt4100-2010-2018.csv")).build());
      this.exportTable(sorted, "sorted");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public static void main(final String[] args) {
    new combineAndSortTDT4100n().run();
  }
}

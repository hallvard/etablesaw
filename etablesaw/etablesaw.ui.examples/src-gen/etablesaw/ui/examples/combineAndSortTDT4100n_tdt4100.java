package etablesaw.ui.examples;

import etablesaw.xtext.lib.TableDef;
import etablesaw.xtext.lib.TypedTable;
import tech.tablesaw.api.IntColumn;
import tech.tablesaw.api.StringColumn;

@TableDef(columnNames = { "programkode", "studentnr", "karakter", "vurderingsstatus", "year", "semester" }, columnTypes = { String.class, int.class, String.class, String.class, int.class, String.class })
@SuppressWarnings("all")
public class combineAndSortTDT4100n_tdt4100 extends TypedTable<combineAndSortTDT4100n_tdt4100.Row> {
  private final StringColumn programkodeColumn;
  
  private final IntColumn studentnrColumn;
  
  private final StringColumn karakterColumn;
  
  private final StringColumn vurderingsstatusColumn;
  
  private final IntColumn yearColumn;
  
  private final StringColumn semesterColumn;
  
  public combineAndSortTDT4100n_tdt4100(final String tableName, final StringColumn programkodeColumn, final IntColumn studentnrColumn, final StringColumn karakterColumn, final StringColumn vurderingsstatusColumn, final IntColumn yearColumn, final StringColumn semesterColumn) {
    super(tableName, programkodeColumn, studentnrColumn, karakterColumn, vurderingsstatusColumn, yearColumn, semesterColumn);
    this.programkodeColumn = programkodeColumn;
    this.studentnrColumn = studentnrColumn;
    this.karakterColumn = karakterColumn;
    this.vurderingsstatusColumn = vurderingsstatusColumn;
    this.yearColumn = yearColumn;
    this.semesterColumn = semesterColumn;
    
  }
  
  public combineAndSortTDT4100n_tdt4100(final String tableName) {
    this(tableName, StringColumn.create("programkode"), IntColumn.create("studentnr"), StringColumn.create("karakter"), StringColumn.create("vurderingsstatus"), IntColumn.create("year"), StringColumn.create("semester"));
  }
  
  public StringColumn getProgramkodeColumn() {
    return programkodeColumn;
  }
  
  public IntColumn getStudentnrColumn() {
    return studentnrColumn;
  }
  
  public StringColumn getKarakterColumn() {
    return karakterColumn;
  }
  
  public StringColumn getVurderingsstatusColumn() {
    return vurderingsstatusColumn;
  }
  
  public IntColumn getYearColumn() {
    return yearColumn;
  }
  
  public StringColumn getSemesterColumn() {
    return semesterColumn;
  }
  
  public combineAndSortTDT4100n_tdt4100 emptyCopy() {
    return new combineAndSortTDT4100n_tdt4100(name(), getProgramkodeColumn().emptyCopy(), getStudentnrColumn().emptyCopy(), getKarakterColumn().emptyCopy(), getVurderingsstatusColumn().emptyCopy(), getYearColumn().emptyCopy(), getSemesterColumn().emptyCopy());
  }
  
  public interface RowData {
    public abstract String getProgramkode();
    
    public abstract int getStudentnr();
    
    public abstract String getKarakter();
    
    public abstract String getVurderingsstatus();
    
    public abstract int getYear();
    
    public abstract String getSemester();
  }
  
  public static class Row extends tech.tablesaw.api.Row implements combineAndSortTDT4100n_tdt4100.RowData {
    public String getProgramkode() {
      return table.getProgramkodeColumn().get(getRowNumber());
    }
    
    public int getStudentnr() {
      return table.getStudentnrColumn().get(getRowNumber());
    }
    
    public String getKarakter() {
      return table.getKarakterColumn().get(getRowNumber());
    }
    
    public String getVurderingsstatus() {
      return table.getVurderingsstatusColumn().get(getRowNumber());
    }
    
    public int getYear() {
      return table.getYearColumn().get(getRowNumber());
    }
    
    public String getSemester() {
      return table.getSemesterColumn().get(getRowNumber());
    }
    
    private final combineAndSortTDT4100n_tdt4100 table;
    
    public Row(final combineAndSortTDT4100n_tdt4100 table) {
      super(table);
      this.table = table;
    }
    
    @Override
    public combineAndSortTDT4100n_tdt4100.Row next() {
      super.next();
      return this;
    }
    
    public combineAndSortTDT4100n_tdt4100.Row setProgramkode(final String programkode) {
      table.getProgramkodeColumn().set(getRowNumber(), programkode);
      return this;
    }
    
    public combineAndSortTDT4100n_tdt4100.Row setStudentnr(final int studentnr) {
      table.getStudentnrColumn().set(getRowNumber(), studentnr);
      return this;
    }
    
    public combineAndSortTDT4100n_tdt4100.Row setKarakter(final String karakter) {
      table.getKarakterColumn().set(getRowNumber(), karakter);
      return this;
    }
    
    public combineAndSortTDT4100n_tdt4100.Row setVurderingsstatus(final String vurderingsstatus) {
      table.getVurderingsstatusColumn().set(getRowNumber(), vurderingsstatus);
      return this;
    }
    
    public combineAndSortTDT4100n_tdt4100.Row setYear(final int year) {
      table.getYearColumn().set(getRowNumber(), year);
      return this;
    }
    
    public combineAndSortTDT4100n_tdt4100.Row setSemester(final String semester) {
      table.getSemesterColumn().set(getRowNumber(), semester);
      return this;
    }
  }
  
  protected combineAndSortTDT4100n_tdt4100.Row row() {
    return new Row(this);
  }
  
  public combineAndSortTDT4100n_tdt4100 append(final combineAndSortTDT4100n_tdt4100.Row row) {
    getProgramkodeColumn().append(row.getProgramkode());
    getStudentnrColumn().append(row.getStudentnr());
    getKarakterColumn().append(row.getKarakter());
    getVurderingsstatusColumn().append(row.getVurderingsstatus());
    getYearColumn().append(row.getYear());
    getSemesterColumn().append(row.getSemester());
    return this;
  }
}

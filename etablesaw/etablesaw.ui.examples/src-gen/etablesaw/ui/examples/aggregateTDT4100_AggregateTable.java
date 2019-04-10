package etablesaw.ui.examples;

import etablesaw.xtext.lib.TableDef;
import etablesaw.xtext.lib.TypedTable;
import tech.tablesaw.api.IntColumn;
import tech.tablesaw.api.StringColumn;

@TableDef(columnNames = { "programkode", "studentnr", "failCount", "timeToRetake", "grade", "year" }, columnTypes = { String.class, int.class, int.class, int.class, String.class, int.class })
@SuppressWarnings("all")
public class aggregateTDT4100_AggregateTable extends TypedTable<aggregateTDT4100_AggregateTable.Row> {
  private final StringColumn programkodeColumn;
  
  private final IntColumn studentnrColumn;
  
  private final IntColumn failCountColumn;
  
  private final IntColumn timeToRetakeColumn;
  
  private final StringColumn gradeColumn;
  
  private final IntColumn yearColumn;
  
  public aggregateTDT4100_AggregateTable(final String tableName, final StringColumn programkodeColumn, final IntColumn studentnrColumn, final IntColumn failCountColumn, final IntColumn timeToRetakeColumn, final StringColumn gradeColumn, final IntColumn yearColumn) {
    super(tableName, programkodeColumn, studentnrColumn, failCountColumn, timeToRetakeColumn, gradeColumn, yearColumn);
    this.programkodeColumn = programkodeColumn;
    this.studentnrColumn = studentnrColumn;
    this.failCountColumn = failCountColumn;
    this.timeToRetakeColumn = timeToRetakeColumn;
    this.gradeColumn = gradeColumn;
    this.yearColumn = yearColumn;
    
  }
  
  public aggregateTDT4100_AggregateTable(final String tableName) {
    this(tableName, StringColumn.create("programkode"), IntColumn.create("studentnr"), IntColumn.create("failCount"), IntColumn.create("timeToRetake"), StringColumn.create("grade"), IntColumn.create("year"));
  }
  
  public StringColumn getProgramkodeColumn() {
    return programkodeColumn;
  }
  
  public IntColumn getStudentnrColumn() {
    return studentnrColumn;
  }
  
  public IntColumn getFailCountColumn() {
    return failCountColumn;
  }
  
  public IntColumn getTimeToRetakeColumn() {
    return timeToRetakeColumn;
  }
  
  public StringColumn getGradeColumn() {
    return gradeColumn;
  }
  
  public IntColumn getYearColumn() {
    return yearColumn;
  }
  
  public aggregateTDT4100_AggregateTable emptyCopy() {
    return new aggregateTDT4100_AggregateTable(name(), getProgramkodeColumn().emptyCopy(), getStudentnrColumn().emptyCopy(), getFailCountColumn().emptyCopy(), getTimeToRetakeColumn().emptyCopy(), getGradeColumn().emptyCopy(), getYearColumn().emptyCopy());
  }
  
  public interface RowData {
    public abstract String getProgramkode();
    
    public abstract int getStudentnr();
    
    public abstract int getFailCount();
    
    public abstract int getTimeToRetake();
    
    public abstract String getGrade();
    
    public abstract int getYear();
  }
  
  public static class Row extends tech.tablesaw.api.Row implements aggregateTDT4100_AggregateTable.RowData {
    public String getProgramkode() {
      return table.getProgramkodeColumn().get(getRowNumber());
    }
    
    public int getStudentnr() {
      return table.getStudentnrColumn().get(getRowNumber());
    }
    
    public int getFailCount() {
      return table.getFailCountColumn().get(getRowNumber());
    }
    
    public int getTimeToRetake() {
      return table.getTimeToRetakeColumn().get(getRowNumber());
    }
    
    public String getGrade() {
      return table.getGradeColumn().get(getRowNumber());
    }
    
    public int getYear() {
      return table.getYearColumn().get(getRowNumber());
    }
    
    private final aggregateTDT4100_AggregateTable table;
    
    public Row(final aggregateTDT4100_AggregateTable table) {
      super(table);
      this.table = table;
    }
    
    @Override
    public aggregateTDT4100_AggregateTable.Row next() {
      super.next();
      return this;
    }
    
    public aggregateTDT4100_AggregateTable.Row setProgramkode(final String programkode) {
      table.getProgramkodeColumn().set(getRowNumber(), programkode);
      return this;
    }
    
    public aggregateTDT4100_AggregateTable.Row setStudentnr(final int studentnr) {
      table.getStudentnrColumn().set(getRowNumber(), studentnr);
      return this;
    }
    
    public aggregateTDT4100_AggregateTable.Row setFailCount(final int failCount) {
      table.getFailCountColumn().set(getRowNumber(), failCount);
      return this;
    }
    
    public aggregateTDT4100_AggregateTable.Row setTimeToRetake(final int timeToRetake) {
      table.getTimeToRetakeColumn().set(getRowNumber(), timeToRetake);
      return this;
    }
    
    public aggregateTDT4100_AggregateTable.Row setGrade(final String grade) {
      table.getGradeColumn().set(getRowNumber(), grade);
      return this;
    }
    
    public aggregateTDT4100_AggregateTable.Row setYear(final int year) {
      table.getYearColumn().set(getRowNumber(), year);
      return this;
    }
  }
  
  protected aggregateTDT4100_AggregateTable.Row row() {
    return new Row(this);
  }
  
  public aggregateTDT4100_AggregateTable append(final aggregateTDT4100_AggregateTable.RowData row) {
    getProgramkodeColumn().append(row.getProgramkode());
    getStudentnrColumn().append(row.getStudentnr());
    getFailCountColumn().append(row.getFailCount());
    getTimeToRetakeColumn().append(row.getTimeToRetake());
    getGradeColumn().append(row.getGrade());
    getYearColumn().append(row.getYear());
    return this;
  }
}

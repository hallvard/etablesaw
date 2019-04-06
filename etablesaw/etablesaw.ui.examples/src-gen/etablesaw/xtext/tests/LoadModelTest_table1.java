package etablesaw.xtext.tests;

import etablesaw.xtext.lib.TableDef;
import etablesaw.xtext.lib.TypedTable;
import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.StringColumn;

@TableDef(columnNames = { "name", "age" }, columnTypes = { String.class, double.class })
@SuppressWarnings("all")
public class LoadModelTest_table1 extends TypedTable<LoadModelTest_table1.Row> {
  private final StringColumn nameColumn;
  
  private final DoubleColumn ageColumn;
  
  public LoadModelTest_table1(final String tableName, final StringColumn nameColumn, final DoubleColumn ageColumn) {
    super(tableName, nameColumn, ageColumn);
    this.nameColumn = nameColumn;
    this.ageColumn = ageColumn;
    
  }
  
  public LoadModelTest_table1(final String tableName) {
    this(tableName, StringColumn.create("name"), DoubleColumn.create("age"));
  }
  
  public StringColumn getNameColumn() {
    return nameColumn;
  }
  
  public DoubleColumn getAgeColumn() {
    return ageColumn;
  }
  
  public LoadModelTest_table1 emptyCopy() {
    return new LoadModelTest_table1(name(), getNameColumn().emptyCopy(), getAgeColumn().emptyCopy());
  }
  
  public interface RowData {
    public abstract String getName();
    
    public abstract double getAge();
  }
  
  public static class Row extends tech.tablesaw.api.Row implements LoadModelTest_table1.RowData {
    public String getName() {
      return table.getNameColumn().get(getRowNumber());
    }
    
    public double getAge() {
      return table.getAgeColumn().get(getRowNumber());
    }
    
    private final LoadModelTest_table1 table;
    
    public Row(final LoadModelTest_table1 table) {
      super(table);
      this.table = table;
    }
    
    @Override
    public LoadModelTest_table1.Row next() {
      super.next();
      return this;
    }
    
    public LoadModelTest_table1.Row setName(final String name) {
      table.getNameColumn().set(getRowNumber(), name);
      return this;
    }
    
    public LoadModelTest_table1.Row setAge(final double age) {
      table.getAgeColumn().set(getRowNumber(), age);
      return this;
    }
  }
  
  protected LoadModelTest_table1.Row row() {
    return new Row(this);
  }
  
  public LoadModelTest_table1 append(final LoadModelTest_table1.Row row) {
    getNameColumn().append(row.getName());
    getAgeColumn().append(row.getAge());
    return this;
  }
}

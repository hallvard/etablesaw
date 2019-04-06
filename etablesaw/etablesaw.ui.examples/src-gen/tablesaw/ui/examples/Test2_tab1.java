package tablesaw.ui.examples;

import etablesaw.xtext.lib.TableDef;
import etablesaw.xtext.lib.TypedTable;
import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.StringColumn;

@TableDef(columnNames = { "name", "age" }, columnTypes = { String.class, double.class })
@SuppressWarnings("all")
public class Test2_tab1 extends TypedTable<Test2_tab1.Row> {
  private final StringColumn nameColumn;
  
  private final DoubleColumn ageColumn;
  
  public Test2_tab1(final String tableName, final StringColumn nameColumn, final DoubleColumn ageColumn) {
    super(tableName, nameColumn, ageColumn);
    this.nameColumn = nameColumn;
    this.ageColumn = ageColumn;
    
  }
  
  public Test2_tab1(final String tableName) {
    this(tableName, StringColumn.create("name"), DoubleColumn.create("age"));
  }
  
  public StringColumn getNameColumn() {
    return nameColumn;
  }
  
  public DoubleColumn getAgeColumn() {
    return ageColumn;
  }
  
  public Test2_tab1 emptyCopy() {
    return new Test2_tab1(name(), getNameColumn().emptyCopy(), getAgeColumn().emptyCopy());
  }
  
  public interface RowData {
    public abstract String getName();
    
    public abstract double getAge();
  }
  
  public static class Row extends tech.tablesaw.api.Row implements Test2_tab1.RowData {
    public String getName() {
      return table.getNameColumn().get(getRowNumber());
    }
    
    public double getAge() {
      return table.getAgeColumn().get(getRowNumber());
    }
    
    private final Test2_tab1 table;
    
    public Row(final Test2_tab1 table) {
      super(table);
      this.table = table;
    }
    
    @Override
    public Test2_tab1.Row next() {
      super.next();
      return this;
    }
    
    public Test2_tab1.Row setName(final String name) {
      table.getNameColumn().set(getRowNumber(), name);
      return this;
    }
    
    public Test2_tab1.Row setAge(final double age) {
      table.getAgeColumn().set(getRowNumber(), age);
      return this;
    }
  }
  
  protected Test2_tab1.Row row() {
    return new Row(this);
  }
  
  public Test2_tab1 append(final Test2_tab1.Row row) {
    getNameColumn().append(row.getName());
    getAgeColumn().append(row.getAge());
    return this;
  }
}

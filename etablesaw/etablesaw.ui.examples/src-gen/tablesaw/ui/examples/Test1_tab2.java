package tablesaw.ui.examples;

import etablesaw.xtext.lib.TableDef;
import etablesaw.xtext.lib.TypedTable;
import tech.tablesaw.api.IntColumn;
import tech.tablesaw.api.StringColumn;

@TableDef(columnNames = { "name", "age" }, columnTypes = { String.class, int.class })
@SuppressWarnings("all")
public class Test1_tab2 extends TypedTable<Test1_tab2.Row> {
  private final StringColumn nameColumn;
  
  private final IntColumn ageColumn;
  
  public Test1_tab2(final String tableName, final StringColumn nameColumn, final IntColumn ageColumn) {
    super(tableName, nameColumn, ageColumn);
    this.nameColumn = nameColumn;
    this.ageColumn = ageColumn;
    
  }
  
  public Test1_tab2(final String tableName) {
    this(tableName, StringColumn.create("name"), IntColumn.create("age"));
  }
  
  public StringColumn getNameColumn() {
    return nameColumn;
  }
  
  public IntColumn getAgeColumn() {
    return ageColumn;
  }
  
  public Test1_tab2 emptyCopy() {
    return new Test1_tab2(name(), getNameColumn().emptyCopy(), getAgeColumn().emptyCopy());
  }
  
  public interface RowData {
    public abstract String getName();
    
    public abstract int getAge();
  }
  
  public static class Row extends tech.tablesaw.api.Row implements Test1_tab2.RowData {
    public String getName() {
      return table.getNameColumn().get(getRowNumber());
    }
    
    public int getAge() {
      return table.getAgeColumn().get(getRowNumber());
    }
    
    private final Test1_tab2 table;
    
    public Row(final Test1_tab2 table) {
      super(table);
      this.table = table;
    }
    
    @Override
    public Test1_tab2.Row next() {
      super.next();
      return this;
    }
    
    public Test1_tab2.Row setName(final String name) {
      table.getNameColumn().set(getRowNumber(), name);
      return this;
    }
    
    public Test1_tab2.Row setAge(final int age) {
      table.getAgeColumn().set(getRowNumber(), age);
      return this;
    }
  }
  
  protected Test1_tab2.Row row() {
    return new Row(this);
  }
  
  public Test1_tab2 append(final Test1_tab2.Row row) {
    getNameColumn().append(row.getName());
    getAgeColumn().append(row.getAge());
    return this;
  }
}

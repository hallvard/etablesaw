package etablesaw.xtext.tests;

import etablesaw.xtext.lib.TableDef;
import etablesaw.xtext.lib.TypedTable;
import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.StringColumn;

@TableDef(columnNames = { "name", "age" }, columnTypes = { String.class, double.class })
@SuppressWarnings("all")
public class InterpretModelTest_table3 extends TypedTable<InterpretModelTest_table3.Row> {
  private final StringColumn nameColumn;
  
  private final DoubleColumn ageColumn;
  
  public InterpretModelTest_table3(final String tableName, final StringColumn nameColumn, final DoubleColumn ageColumn) {
    super(tableName, nameColumn, ageColumn);
    this.nameColumn = nameColumn;
    this.ageColumn = ageColumn;
    
  }
  
  public InterpretModelTest_table3(final String tableName) {
    this(tableName, StringColumn.create("name"), DoubleColumn.create("age"));
  }
  
  public StringColumn getNameColumn() {
    return nameColumn;
  }
  
  public DoubleColumn getAgeColumn() {
    return ageColumn;
  }
  
  public InterpretModelTest_table3 emptyCopy() {
    return new InterpretModelTest_table3(name(), getNameColumn().emptyCopy(), getAgeColumn().emptyCopy());
  }
  
  public interface RowData {
    public abstract String getName();
    
    public abstract double getAge();
  }
  
  public static class Row extends tech.tablesaw.api.Row implements InterpretModelTest_table3.RowData {
    public String getName() {
      return table.getNameColumn().get(getRowNumber());
    }
    
    public double getAge() {
      return table.getAgeColumn().get(getRowNumber());
    }
    
    private final InterpretModelTest_table3 table;
    
    public Row(final InterpretModelTest_table3 table) {
      super(table);
      this.table = table;
    }
    
    @Override
    public InterpretModelTest_table3.Row next() {
      super.next();
      return this;
    }
    
    public InterpretModelTest_table3.Row setName(final String name) {
      table.getNameColumn().set(getRowNumber(), name);
      return this;
    }
    
    public InterpretModelTest_table3.Row setAge(final double age) {
      table.getAgeColumn().set(getRowNumber(), age);
      return this;
    }
  }
  
  protected InterpretModelTest_table3.Row row() {
    return new Row(this);
  }
  
  public InterpretModelTest_table3 append(final InterpretModelTest_table3.Row row) {
    getNameColumn().append(row.getName());
    getAgeColumn().append(row.getAge());
    return this;
  }
}

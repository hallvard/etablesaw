package etablesaw.xtext.lib;

import tech.tablesaw.api.IntColumn;
import tech.tablesaw.api.StringColumn;

@TableDef(columnNames = {"name", "count"}, columnTypes = {String.class, int.class})
public class ExampleTable extends TypedTable<ExampleTable.Row> {

    private final StringColumn nameColumn;
    private final IntColumn countColumn;
    
    public ExampleTable(String name, StringColumn nameColumn, IntColumn countColumn) {
        super(name, nameColumn, countColumn);
        this.nameColumn = nameColumn;
        this.countColumn = countColumn;
    }
    
    @Override
    public TypedTable<Row> emptyCopy() {
        return new ExampleTable(name(), nameColumn.emptyCopy(), countColumn.emptyCopy());
    }
    
    @Override
    public TypedTable<Row> emptyCopy(int rowSize) {
        return new ExampleTable(name(), nameColumn.emptyCopy(rowSize), countColumn.emptyCopy(rowSize));
    }

    public interface RowData {
    	public String getName();
    	public int getCount();
    	
    	default public void copyInto(Row row) {
    		row.setName(getName());
    		row.setCount(getCount());
    	}
    }

    public static class Row extends TypedRow<Row> implements RowData {

        private final ExampleTable table;
        
        protected Row(final ExampleTable table) {
            super(table);
            this.table = table;
        }

        public String getName() {
            return table.nameColumn.get(getRowNumber());
        }
        
        public int getCount() {
            return table.countColumn.get(getRowNumber());
        }

        public Row setName(String name) {
            table.nameColumn.set(getRowNumber(), name);
            return this;
        }
        
        public Row setCount(int count) {
            table.countColumn.set(getRowNumber(), count);
            return this;
        }
    }
    
    @Override
    public Row row() {
        return new Row(this);
    }

    public Row appendRowCopy(RowData row) {
    	Row newRow = appendEmptyRow();
    	row.copyInto(newRow);
    	return newRow;
    }

    public void append(final Row row) {
        nameColumn.append(row.getName());
        countColumn.append(row.getCount());
    }
    
    public static void main(String[] args) {
        ExampleTable tab = new ExampleTable("tab", StringColumn.create("s"), IntColumn.create("i"));
        for (ExampleTable.Row row : tab.rows()) {
            row.getCount();
        }
    }

}

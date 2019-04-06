package etablesaw.xtext.lib;

import java.util.Iterator;

import tech.tablesaw.api.Row;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;

public abstract class TypedTable<R extends Row> extends Table {

    protected TypedTable(String name, Column<?>... columns) {
        super(name, columns);
    }
    
    public Iterable<R> rows() {
        return new Iterable<R>() {
            public Iterator<R> iterator() {
                return new Iterator<R>() {
                    R row = row();
                    @Override
                    public boolean hasNext() {
                        return row.hasNext();
                    }
                    @Override
                    public R next() {
                        row.next();
                        return row;
                    }
                };
            }
        };        
    }

    protected abstract R row();
    
    public R appendEmptyRow() {
        R row = row();
        row.at(this.rowCount());
        for (Column<?> column : columns()) {
            column.appendMissing();
        }
        return row;
      }

}

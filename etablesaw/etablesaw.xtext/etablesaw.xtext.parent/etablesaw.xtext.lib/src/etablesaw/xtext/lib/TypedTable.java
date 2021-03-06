package etablesaw.xtext.lib;

import java.util.Iterator;
import java.util.function.Predicate;

import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;
import tech.tablesaw.selection.BitmapBackedSelection;
import tech.tablesaw.selection.Selection;

public abstract class TypedTable<R extends TypedRow<R>> extends Table {

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

    public abstract TypedTable<R> emptyCopy();
    public abstract TypedTable<R> emptyCopy(int rowSize);
    public abstract R row();

    public R appendEmptyRow() {
        for (Column<?> column : columns()) {
            column.appendMissing();
        }
        R row = row();
        row.at(this.rowCount());
        return row;
    }

    public Selection eval(Predicate<R> predicate) {
        Selection selection = new BitmapBackedSelection();
        R row = row();
        while (row.hasNext()) {
            row.next();
            if (predicate.test(row)) {
                selection.add(row.getRowNumber());
            }
        }
        return selection;
    }
}

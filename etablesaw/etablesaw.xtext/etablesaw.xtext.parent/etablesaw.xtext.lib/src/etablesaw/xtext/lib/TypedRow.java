package etablesaw.xtext.lib;

import tech.tablesaw.api.Row;

public abstract class TypedRow<R extends TypedRow<R>> extends Row {

    protected TypedRow(TypedTable<R> table) {
        super(table);
    }
    
    @Override
    public TypedRow<R> next() {
        super.next();
        return this;
    }
    
	public abstract void copyInto(R row);
}

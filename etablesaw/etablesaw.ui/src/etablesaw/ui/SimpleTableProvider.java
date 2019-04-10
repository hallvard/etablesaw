package etablesaw.ui;

import tech.tablesaw.api.Table;

public class SimpleTableProvider extends TableProviderHelper implements TableProvider {

    private Table table;
    
    public SimpleTableProvider(Table table) {
        this.table = table;
    }

    public void setTable(Table table) {
        this.table = table;
        fireTableChanged();
    }
    
    @Override
    public Table getTable() {
        return table;
    }

    public void fireTableDataChanged() {
        fireTableDataChanged(this);
    }
    
    public void fireTableChanged() {
        fireTableChanged(this);
    }
}

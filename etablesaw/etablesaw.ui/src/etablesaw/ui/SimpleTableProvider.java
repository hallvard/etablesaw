package etablesaw.ui;

import java.util.ArrayList;
import java.util.Collection;

import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;

public class SimpleTableProvider extends TableProviderHelper implements TableProvider {

    private Table table;
    
    public static Object computeTableSignature(Table table) {
        Collection<Object> signature = new ArrayList<Object>();
        if (table != null) {
            for (Column<?> column : table.columns()) {
                signature.add(column.name());
                signature.add(column.type());
            }
        }
        return signature;
    }
    
    public static boolean onlyTableDataChanged(Object table1Signature, Object table2Signature) {
        return table1Signature.equals(table2Signature);
    }
    public static boolean onlyTableDataChanged(Table table1, Table table2) {
        return table1 == table2 || onlyTableDataChanged(computeTableSignature(table1), computeTableSignature(table2));
    }
    
    public SimpleTableProvider(Table table) {
        setTable(table);
    }

    public void setTable(Table table) {
        Table oldTable = this.table;
        this.table = table;
        if (onlyTableDataChanged(oldTable, table)) {
            fireTableDataChanged();
        } else {
            fireTableChanged();
        }
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

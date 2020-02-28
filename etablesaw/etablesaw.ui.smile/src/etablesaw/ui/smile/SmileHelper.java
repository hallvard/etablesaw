package etablesaw.ui.smile;

import smile.data.DataFrame;
import tech.tablesaw.api.Table;

public class SmileHelper {

    public static Table createDataFrameTable(Table table, String... columnNames) {
        Table dataFrameTable = Table.create("dataFrame");
        for (int i = 0; i < columnNames.length; i++) {
            dataFrameTable.addColumns(table.column(columnNames[i]));
        }
        return dataFrameTable;
    }
    public static Table createDataFrameTable(Table table, String columnName, String... columnNames) {
        Table dataFrameTable = Table.create("dataFrame", table.column(columnName));
        for (int i = 0; i < columnNames.length; i++) {
            dataFrameTable.addColumns(table.column(columnNames[i]));
        }
        return dataFrameTable;
    }
    
    public static DataFrame createDataFrame(Table table, String... columnNames) {
        return createDataFrameTable(table, columnNames).smile().toDataFrame();
    }

    public static DataFrame createDataFrame(Table table, String columnName, String... columnNames) {
        return createDataFrameTable(table, columnName, columnNames).smile().toDataFrame();
    }
}

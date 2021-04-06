package etablesaw.xtext.jvmmodel;

import org.eclipse.xtext.xbase.lib.StringExtensions;

public class DefaultTableTypeNameProvider implements ITableTypeNameProvider {

    public String getTableTypeName(String qName, String tableName) {
        return qName + "_" + tableName;
    }

    @Override
    public String getColumnName(String columnName) {
        return columnName + "Column";
    }

    @Override
    public String getColumnGetterName(String columnName) {
        return "get" + StringExtensions.toFirstUpper(getColumnName(columnName));
    }

    @Override
    public String getColumnValueGetterName(String columnName) {
        return "get" + StringExtensions.toFirstUpper(columnName);
    }
    
    @Override
    public String getColumnValueSetterName(String columnName) {
        return "set" + StringExtensions.toFirstUpper(columnName);
    }

	@Override
	public String getTableRowDataClassName(String tableName) {
		return "RowData";
	}
	
	@Override
	public String getTableRowDataImplClassName(String tableName) {
		return "RowDataImpl";
	}

	@Override
	public String getTableRowClassName(String tableName) {
		return "Row";
	}
}

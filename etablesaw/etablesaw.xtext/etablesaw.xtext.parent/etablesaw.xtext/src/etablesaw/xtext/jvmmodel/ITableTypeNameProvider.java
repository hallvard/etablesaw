package etablesaw.xtext.jvmmodel;

public interface ITableTypeNameProvider {
    public String getTableTypeName(String qName, String tableName);
    public String getColumnName(String columnName);
    public String getColumnGetterName(String columnName);
    public String getColumnValueGetterName(String columnName);
    public String getColumnValueSetterName(String columnName);
}

package etablesaw.xtext.lib;

import java.lang.reflect.Method;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

import tech.tablesaw.api.BooleanColumn;
import tech.tablesaw.api.ColumnType;
import tech.tablesaw.api.DateColumn;
import tech.tablesaw.api.DateTimeColumn;
import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.FloatColumn;
import tech.tablesaw.api.IntColumn;
import tech.tablesaw.api.LongColumn;
import tech.tablesaw.api.ShortColumn;
import tech.tablesaw.api.StringColumn;
import tech.tablesaw.api.TimeColumn;
import tech.tablesaw.columns.Column;

public class ColumnTypeUtil {

	public static <T> Column<T> createColumn(final Class<T> elementClass) {
		return createColumn(elementClass, getDefaultColumnName(elementClass));
	}

	private static <T> String getDefaultColumnName(final Class<T> elementClass) {
		return elementClass.getSimpleName();
	}

	public static <T> Column<T> createColumn(final Class<T> elementClass, int size) {
		return createColumn(elementClass, elementClass.getSimpleName(), size);
	}

	public static <T> Column<T> createColumn(final Class<T> elementClass, String name) {
		Class<?> columnType = getColumnType(elementClass);
		try {
			Method creationMethod = columnType.getMethod("create", new Class<?>[] { String.class });
			Column<T> column = (Column<T>) creationMethod.invoke(null, new Object[] { name });
			return column;
		} catch (Exception e) {
			throw new IllegalArgumentException("Cannot create column for " + elementClass, e);
		}
	}

	public static <T> Column<T> createColumn(final Class<T> elementClass, String name, int size) {
		Class<?> columnType = getColumnType(elementClass);
		try {
			Method creationMethod = columnType.getMethod("create", new Class<?>[] { String.class, int.class });
			Column<T> column = (Column<T>) creationMethod.invoke(null, new Object[] { name, size });
			return column;
		} catch (Exception e) {
			throw new IllegalArgumentException("Cannot create column for " + elementClass, e);
		}
	}

	public static Class<?> getColumnType(final Class<?> elementClass) {
		return getColumnType(elementClass.getName());
	}

    public static Class<?> getColumnType(final String elementClassName) {
        switch (elementClassName) {
        case "java.lang.String":            		return StringColumn.class;
        case "int": case "java.lang.Integer" :      return IntColumn.class;
        case "long": case "java.lang.Long" :        return LongColumn.class;
        case "float": case "java.lang.Float" : 		return FloatColumn.class;
        case "double": case "java.lang.Double" :    return DoubleColumn.class;
        case "boolean": case "java.lang.Boolean" :  return BooleanColumn.class;
        case "short": case "java.lang.Short" :      return ShortColumn.class;

        case "java.time.LocalTime": 	return TimeColumn.class;
        case "java.time.LocalDateTime": return DateTimeColumn.class;
        case "java.time.LocalDate": 	return DateColumn.class;
        default: return null;
        }
    }
    
    public static Class<?> getElementType(final Class<? extends Column<?>> columnClass) {
        return getElementType(columnClass.getName());
    }
    
    public static Class<?> getElementType(final String columnClassName) {
        switch (columnClassName) {
        case "tech.tablesaw.api.StringColumn": 	return String.class;
        case "tech.tablesaw.api.IntColumn": 	return int.class;
        case "tech.tablesaw.api.LongColumn": 	return long.class;
        case "tech.tablesaw.api.DoubleColumn": 	return double.class;
        case "tech.tablesaw.api.FloatColumn": 	return float.class;
        case "tech.tablesaw.api.BooleanColumn": return boolean.class;
        case "tech.tablesaw.api.ShortColumn": 	return short.class;

        case "tech.tablesaw.api.TimeColumnn": 	return LocalTime.class;
        case "tech.tablesaw.api.DateTimeColumnn": return LocalDateTime.class;
        case "tech.tablesaw.api.DateColumnn": 	return LocalDate.class;
        default: return null;
        }
    }

    public static Class<?> getElementType(final ColumnType columnType) {
        if (columnType.equals(ColumnType.STRING) || columnType.equals(ColumnType.TEXT)) {
            return String.class;
        } else if (columnType.equals(ColumnType.INTEGER)) {
            return int.class;
        } else if (columnType.equals(ColumnType.DOUBLE)) {
            return double.class;
        } else if (columnType.equals(ColumnType.FLOAT)) {
        	return float.class;
        } else if (columnType.equals(ColumnType.BOOLEAN)) {
            return boolean.class;
        } else if (columnType.equals(ColumnType.LONG)) {
            return long.class;
        } else if (columnType.equals(ColumnType.SHORT)) {
            return short.class;
        } else
            if (columnType.equals(ColumnType.LOCAL_TIME)) {
            return LocalTime.class;
        } else if (columnType.equals(ColumnType.LOCAL_DATE_TIME)) {
            return LocalDateTime.class;
        } else if (columnType.equals(ColumnType.LOCAL_DATE)) {
            return LocalDate.class;
        }
        return Object.class;
    }
}

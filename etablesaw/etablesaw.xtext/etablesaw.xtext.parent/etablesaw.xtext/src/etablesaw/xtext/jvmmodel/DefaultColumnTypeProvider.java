package etablesaw.xtext.jvmmodel;

import com.google.inject.Inject;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder;
import tech.tablesaw.api.BooleanColumn;
import tech.tablesaw.api.ColumnType;
import tech.tablesaw.api.DateColumn;
import tech.tablesaw.api.DateTimeColumn;
import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.IntColumn;
import tech.tablesaw.api.ShortColumn;
import tech.tablesaw.api.StringColumn;
import tech.tablesaw.api.TimeColumn;
import tech.tablesaw.columns.Column;

public class DefaultColumnTypeProvider implements IColumnTypeProvider {

    public static Class<?> getColumnType(final String elementClassName) {
        switch (elementClassName) {
        case "java.lang.String":            return StringColumn.class;
        case "int": case "Integer" :        return IntColumn.class;
        case "double": case "Double" :      return DoubleColumn.class;
        case "boolean": case "Boolean" :    return BooleanColumn.class;
        case "short": case "Short" :        return ShortColumn.class;

        case "java.time.LocalTime": return TimeColumn.class;
        case "java.time.LocalDateTime": return DateTimeColumn.class;
        case "java.time.LocalDate": return DateColumn.class;
        default: return null;
        }
    }
    
    public static Class<?> getElementType(final Class<? extends Column<?>> columnClass) {
        return getElementType(columnClass.getName());
    }
    
    public static Class<?> getElementType(final String columnClassName) {
        switch (columnClassName) {
        case "tech.tablesaw.api.StringColumn": return String.class;
        case "tech.tablesaw.api.IntColumn": return int.class;
        case "tech.tablesaw.api.DoubleColumn": return double.class;
        case "tech.tablesaw.api.BooleanColumn": return boolean.class;
        case "tech.tablesaw.api.ShortColumn": return short.class;

        case "tech.tablesaw.api.TimeColumnn": return LocalTime.class;
        case "tech.tablesaw.api.DateTimeColumnn": return LocalDateTime.class;
        case "tech.tablesaw.api.DateColumnn": return LocalDate.class;
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
    
	@Inject
	private JvmTypesBuilder typeRefBuilder;

	@Override
	public JvmTypeReference getColumnTypeReference(final JvmTypeReference elementType) {
	    String qName = elementType.getQualifiedName();
        Class<?> columnType = getColumnType(qName);
	    if (columnType == null) {
	        throw new RuntimeException("Unsupported column type: " + qName);	        
	    }
        return typeRefBuilder.newTypeRef(elementType, columnType);
	}

	protected boolean isSame(final Class<?> clazz, final JvmTypeReference type) {
		return clazz.getName().equals(type.getQualifiedName());
	}
}

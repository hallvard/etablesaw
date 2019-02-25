package etablesaw.xtext.jvmmodel;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder;

import com.google.inject.Inject;

import tech.tablesaw.api.BooleanColumn;
import tech.tablesaw.api.DateColumn;
import tech.tablesaw.api.DateTimeColumn;
import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.IntColumn;
import tech.tablesaw.api.ShortColumn;
import tech.tablesaw.api.StringColumn;
import tech.tablesaw.api.TimeColumn;

public class DefaultColumnTypeProvider implements IColumnTypeProvider {

	@Inject
	private JvmTypesBuilder typeRefBuilder;

	@Override
	public JvmTypeReference getColumnTypeReference(final JvmTypeReference elementType) {
		if (isSame(String.class, elementType)) {
			return typeRefBuilder.newTypeRef(elementType, StringColumn.class);
		} else if (isSame(Boolean.TYPE, elementType) || isSame(Boolean.class, elementType)) {
			return typeRefBuilder.newTypeRef(elementType, BooleanColumn.class);
		} else if (isSame(Double.TYPE, elementType) || isSame(Double.class, elementType)) {
			return typeRefBuilder.newTypeRef(elementType, DoubleColumn.class);
		} else if (isSame(Integer.TYPE, elementType) || isSame(Integer.class, elementType)) {
			return typeRefBuilder.newTypeRef(elementType, IntColumn.class);
		} else if (isSame(Short.TYPE, elementType) || isSame(Short.class, elementType)) {
			return typeRefBuilder.newTypeRef(elementType, ShortColumn.class);
		} else if (isSame(LocalTime.class, elementType)) {
			return typeRefBuilder.newTypeRef(elementType, TimeColumn.class);
		} else if (isSame(LocalDate.class, elementType)) {
			return typeRefBuilder.newTypeRef(elementType, DateColumn.class);
		} else if (isSame(LocalDateTime.class, elementType)) {
			return typeRefBuilder.newTypeRef(elementType, DateTimeColumn.class);
		}
		throw new RuntimeException("Unsupported column type: " + elementType.getQualifiedName());
	}

	protected boolean isSame(final Class<?> clazz, final JvmTypeReference type) {
		return clazz.getName().equals(type.getQualifiedName());
	}
}

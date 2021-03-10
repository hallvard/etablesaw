package etablesaw.xtext.lib;

import org.eclipse.xtext.xbase.lib.Pure;

import tech.tablesaw.columns.Column;

public class ColumnLiterals {

	@Pure
	public static <T> Column<T> createColumn(final Class<T> elementClass) {
		return ColumnTypeUtil.createColumn(elementClass);
	}

	@Pure
	public static <T> Column<T> createColumn(final Class<T> elementClass, String name) {
		return ColumnTypeUtil.createColumn(elementClass, name);
	}
	
	@Pure
	public static <T> Column<T> createColumn(final Class<T> elementClass, String name, int size) {
		return ColumnTypeUtil.createColumn(elementClass, name, size);
	}
	
	@Pure
	public static <T> Column<T> createColumn(final Class<T> elementClass, int size) {
		return ColumnTypeUtil.createColumn(elementClass, size);
	}
}

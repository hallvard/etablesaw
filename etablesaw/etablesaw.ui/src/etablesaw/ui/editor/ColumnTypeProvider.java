package etablesaw.ui.editor;

import tech.tablesaw.api.ColumnType;

public interface ColumnTypeProvider {

	public ColumnType getColumnType(int columnIndex);
}

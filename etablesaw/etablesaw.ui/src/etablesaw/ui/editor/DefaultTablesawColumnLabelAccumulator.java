package etablesaw.ui.editor;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.nebula.widgets.nattable.layer.ILayer;
import org.eclipse.nebula.widgets.nattable.layer.cell.ColumnOverrideLabelAccumulator;

import tech.tablesaw.api.ColumnType;
import tech.tablesaw.api.Table;
import tech.tablesaw.columns.Column;

public class DefaultTablesawColumnLabelAccumulator extends ColumnOverrideLabelAccumulator {

	public static String NUMBER_COLUMN_TYPE_CLASS = "NUMBER";
	public static String DECIMAL_COLUMN_TYPE_CLASS = "DECIMAL";
	public static String TIME_COLUMN_TYPE_CLASS = "TIME";
	public static String TEXT_COLUMN_TYPE_CLASS = "TEXTUAL";

	private static String[] NUMBER_COLUMN_TYPE_CLASSES = { NUMBER_COLUMN_TYPE_CLASS };
	private static String[] DECIMAL_COLUMN_TYPE_CLASSES = { NUMBER_COLUMN_TYPE_CLASS, DECIMAL_COLUMN_TYPE_CLASS };
	private static String[] TIME_COLUMN_TYPE_CLASSES = { TIME_COLUMN_TYPE_CLASS };
	private static String[] TEXT_COLUMN_TYPE_CLASSES = { TEXT_COLUMN_TYPE_CLASS };

	private Map<ColumnType, String[]> columnTypeClassLabels = new HashMap<>();
	{
		columnTypeClassLabels.put(ColumnType.DOUBLE, DECIMAL_COLUMN_TYPE_CLASSES);
		columnTypeClassLabels.put(ColumnType.FLOAT, DECIMAL_COLUMN_TYPE_CLASSES);
		columnTypeClassLabels.put(ColumnType.INTEGER, NUMBER_COLUMN_TYPE_CLASSES);
		columnTypeClassLabels.put(ColumnType.SHORT, NUMBER_COLUMN_TYPE_CLASSES);

		columnTypeClassLabels.put(ColumnType.LOCAL_DATE, TIME_COLUMN_TYPE_CLASSES);
		columnTypeClassLabels.put(ColumnType.LOCAL_DATE_TIME, TIME_COLUMN_TYPE_CLASSES);
		columnTypeClassLabels.put(ColumnType.LOCAL_TIME, TIME_COLUMN_TYPE_CLASSES);
		columnTypeClassLabels.put(ColumnType.INSTANT, TIME_COLUMN_TYPE_CLASSES);

		columnTypeClassLabels.put(ColumnType.STRING, TEXT_COLUMN_TYPE_CLASSES);
		columnTypeClassLabels.put(ColumnType.TEXT, TEXT_COLUMN_TYPE_CLASSES);
	}

	public DefaultTablesawColumnLabelAccumulator(final ILayer layer, final Table table) {
		super(layer);
		if (table != null) {
			for (int colNum = 0; colNum < table.columnCount(); colNum++) {
				final ColumnType colType = table.column(colNum).type();
				final String[] labels = getLabelsFor(colType);
				if (labels != null) {
					super.registerColumnOverrides(colNum, labels);
				}
				final String label = getLabelFor(table.column(colNum).type());
				if (label != null) {
					super.registerColumnOverrides(colNum, label);
				}
			}
		}
	}
	
	protected String[] getLabelsFor(final Column<?> column) {
		return getLabelsFor(column.type());
	}
	
	protected String getLabelFor(final Column<?> column) {
		return getLabelFor(column.type());
	}

	protected String[] getLabelsFor(final ColumnType type) {
		return columnTypeClassLabels.get(type);
	}

	protected String getLabelFor(final ColumnType type) {
		return type.name();
	}
}

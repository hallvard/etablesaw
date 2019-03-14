package etablesaw.ui.editor;

import org.eclipse.nebula.widgets.nattable.config.IConfigRegistry;
import org.eclipse.nebula.widgets.nattable.data.convert.DefaultDisplayConverter;
import org.eclipse.nebula.widgets.nattable.layer.cell.ILayerCell;

import tech.tablesaw.api.ColumnType;
import tech.tablesaw.columns.booleans.BooleanColumnType;
import tech.tablesaw.columns.numbers.DoubleColumnType;
import tech.tablesaw.columns.numbers.IntColumnType;
import tech.tablesaw.columns.numbers.ShortColumnType;

public abstract class AbstractTablesawDisplayConverter extends DefaultDisplayConverter {

	private ColumnTypeProvider columnTypeProvider;

	public AbstractTablesawDisplayConverter(final ColumnTypeProvider columnTypeProvider) {
		setColumnTypeProvider(columnTypeProvider);
	}

	public void setColumnTypeProvider(final ColumnTypeProvider columnTypeProvider) {
		this.columnTypeProvider = columnTypeProvider;
	}

	protected ColumnType getColumnType(final ILayerCell cell) {
		return columnTypeProvider.getColumnType(cell.getColumnIndex());
	}

	@Override
	public Object canonicalToDisplayValue(final ILayerCell cell, final IConfigRegistry configRegistry, final Object canonicalValue) {
		final Object displayValue = canonicalToDisplayValue(canonicalValue, getColumnType(cell));
		return (displayValue != null ? displayValue : super.canonicalToDisplayValue(cell, configRegistry, canonicalValue));
	}

	@Override
	public Object displayToCanonicalValue(final ILayerCell cell, final IConfigRegistry configRegistry, final Object displayValue) {
		final Object canonicalValue = displayToCanonicalValue(displayValue, getColumnType(cell));
		return (canonicalValue != null ? canonicalValue : super.displayToCanonicalValue(cell, configRegistry, displayValue));
	}

	// to display value

	protected String missingDisplayValue() {
		return "";
	}

	protected Object canonicalToDisplayValue(final Object value, final ColumnType type) {
		if (type instanceof BooleanColumnType && value instanceof Boolean) {
			return booleanDisplayValue((Boolean) value, value == null);
		} else if (type instanceof DoubleColumnType && value instanceof Number) {
			final double doubleValue = ((Number) value).doubleValue();
			return doubleDisplayValue(doubleValue, DoubleColumnType.isMissingValue(doubleValue));
		} else if (type instanceof IntColumnType && value instanceof Number) {
		    final int intValue = ((Number) value).intValue();
		    return intDisplayValue(intValue, IntColumnType.isMissingValue(intValue));
		} else if (type instanceof ShortColumnType && value instanceof Number) {
			final short shortValue = ((Number) value).shortValue();
			return shortDisplayValue(shortValue, ShortColumnType.isMissingValue(shortValue));
		} else if (value == null) {
			return missingDisplayValue();
		}
		return null;
	}
	protected abstract Object booleanDisplayValue(final Boolean value, final boolean missing);
	protected abstract String missingBooleanDisplayValue();

	protected abstract Object doubleDisplayValue(final double doubleValue, final boolean missing);
	protected abstract String missingDoubleDisplayValue();

	protected abstract Object intDisplayValue(final int intValue, final boolean missing);
	protected abstract String missingIntDisplayValue();

	protected abstract Object shortDisplayValue(final short shortValue, final boolean missing);
	protected abstract String missingShortDisplayValue();

	// to canonical value

	protected Object displayToCanonicalValue(final Object value, final ColumnType type) {
		if (type instanceof BooleanColumnType) {
			return booleanCanonicalValue(value);
		} else if (type instanceof DoubleColumnType) {
			return doubleCanonicalValue(value);
		} else if (type instanceof IntColumnType) {
			return intCanonicalValue(value);
		} else if (type instanceof ShortColumnType) {
		    return shortCanonicalValue(value);
		}
		return null;
	}

	protected abstract Object booleanCanonicalValue(final Object value);
	protected abstract Object doubleCanonicalValue(final Object value);
	protected abstract Object intCanonicalValue(final Object value);
	protected abstract Object shortCanonicalValue(final Object value);
}

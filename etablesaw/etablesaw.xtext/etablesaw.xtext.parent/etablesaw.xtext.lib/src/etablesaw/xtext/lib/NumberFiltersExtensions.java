package etablesaw.xtext.lib;

import tech.tablesaw.api.NumericColumn;
import tech.tablesaw.columns.numbers.NumberFilters;
import tech.tablesaw.selection.Selection;

public class NumberFiltersExtensions {

	// >
	public static Selection operator_greaterThan(NumberFilters numbericColumn, double n) {
	    return numbericColumn.isGreaterThan(n);
	}
	public static Selection operator_greaterThan(NumberFilters numbericColumn, NumericColumn<?> numericColumn) {
	    return numbericColumn.isGreaterThan(numericColumn);
	}
	// >=
	public static Selection operator_greaterEqualsThan(NumberFilters numbericColumn, double n) {
	    return numbericColumn.isGreaterThanOrEqualTo(n);
	}
	public static Selection operator_greaterEqualsThan(NumberFilters numbericColumn, NumericColumn<?> numericColumn) {
	    return numbericColumn.isGreaterThanOrEqualTo(numericColumn);
	}
	// <
	public static Selection operator_lessThan(NumberFilters numbericColumn, double n) {
	    return numbericColumn.isLessThan(n);
	}
	public static Selection operator_lessThan(NumberFilters numbericColumn, NumericColumn<?> numericColumn) {
	    return numbericColumn.isLessThan(numericColumn);
	}
	// <=
	public static Selection operator_lessEqualsThan(NumberFilters numbericColumn, double n) {
	    return numbericColumn.isLessThanOrEqualTo(n);
	}
	public static Selection operator_lessEqualsThan(NumberFilters numbericColumn, NumericColumn<?> numericColumn) {
	    return numbericColumn.isLessThanOrEqualTo(numericColumn);
	}
}

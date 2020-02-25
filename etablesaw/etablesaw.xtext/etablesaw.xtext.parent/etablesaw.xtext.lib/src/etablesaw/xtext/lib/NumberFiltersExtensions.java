package etablesaw.xtext.lib;

import tech.tablesaw.api.NumericColumn;
import tech.tablesaw.columns.numbers.NumberFilters;
import tech.tablesaw.selection.Selection;

public class NumberFiltersExtensions {

	// >
	public static Selection operator_greaterThan(NumberFilters numericColumn, double n) {
	    return numericColumn.isGreaterThan(n);
	}
	public static Selection operator_greaterThan(NumberFilters numericColumn1, NumericColumn<?> numericColumn2) {
	    return numericColumn1.isGreaterThan(numericColumn2);
	}
	// >=
	public static Selection operator_greaterEqualsThan(NumberFilters numericColumn, double n) {
	    return numericColumn.isGreaterThanOrEqualTo(n);
	}
	public static Selection operator_greaterEqualsThan(NumberFilters numericColumn1, NumericColumn<?> numericColumn2) {
	    return numericColumn1.isGreaterThanOrEqualTo(numericColumn2);
	}
	// <
	public static Selection operator_lessThan(NumberFilters numericColumn, double n) {
	    return numericColumn.isLessThan(n);
	}
	public static Selection operator_lessThan(NumberFilters numericColumn1, NumericColumn<?> numericColumn2) {
	    return numericColumn1.isLessThan(numericColumn2);
	}
	// <=
	public static Selection operator_lessEqualsThan(NumberFilters numericColumn, double n) {
	    return numericColumn.isLessThanOrEqualTo(n);
	}
	public static Selection operator_lessEqualsThan(NumberFilters numericColumn1, NumericColumn<?> numericColumn2) {
	    return numericColumn1.isLessThanOrEqualTo(numericColumn2);
	}
}

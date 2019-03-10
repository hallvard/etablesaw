package etablesaw.xtext.lib;

import tech.tablesaw.api.DoubleColumn;
import tech.tablesaw.api.NumericColumn;

public class DoubleColumnExtensions {
	
	// +
	public static DoubleColumn operator_plus(DoubleColumn doubleColumn, Number n) {
	    return doubleColumn.add(n);
	}
	public static DoubleColumn operator_plus(DoubleColumn doubleColumn, NumericColumn<?> col2) {
	    return doubleColumn.add(col2);
	}

	// -
	public static DoubleColumn operator_minus(DoubleColumn doubleColumn, Number n) {
	    return doubleColumn.subtract(n);
	}
	public static DoubleColumn operator_minus(DoubleColumn doubleColumn, NumericColumn<?> col2) {
	    return doubleColumn.subtract(col2);
	}
	
	// *
	public static DoubleColumn operator_multiply(DoubleColumn doubleColumn, Number n) {
	    return doubleColumn.divide(n);
	}
	public static DoubleColumn operator_multiply(DoubleColumn doubleColumn, NumericColumn<?> col2) {
	    return doubleColumn.multiply(col2);
	}
	
	// /
	public static DoubleColumn operator_divide(DoubleColumn doubleColumn, Number n) {
	    return doubleColumn.divide(n);
	}
	public static DoubleColumn operator_divide(DoubleColumn doubleColumn, NumericColumn<?> col2) {
	    return doubleColumn.divide(col2);
	}
	
	// **
	public static DoubleColumn operator_power(DoubleColumn doubleColumn, double n) {
	    return doubleColumn.power(n);
	}
}

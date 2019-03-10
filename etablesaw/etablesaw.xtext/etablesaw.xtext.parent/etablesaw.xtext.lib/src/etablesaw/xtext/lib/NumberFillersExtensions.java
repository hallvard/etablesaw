package etablesaw.xtext.lib;

import java.util.function.DoubleSupplier;

import it.unimi.dsi.fastutil.doubles.DoubleIterator;
import tech.tablesaw.columns.numbers.NumberFillers;
import tech.tablesaw.columns.numbers.fillers.DoubleRangeIterable;

public class NumberFillersExtensions {
	
	// =>

	public static <T> T operator_doubleArrow(DoubleIterator doubles, NumberFillers<T> doubleColumn) {
	    return doubleColumn.fillWith(doubles);
	}
	public static <T> T operator_doubleArrow(DoubleSupplier doubles, NumberFillers<T> doubleColumn) {
	    return doubleColumn.fillWith(doubles);
	}
	public static <T> T operator_doubleArrow(DoubleRangeIterable doubles, NumberFillers<T> doubleColumn) {
	    return doubleColumn.fillWith(doubles);
	}
}

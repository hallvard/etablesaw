package etablesaw.xtext.lib;

import java.util.function.Predicate;

import org.eclipse.xtext.xbase.lib.IntegerRange;
import org.eclipse.xtext.xbase.lib.Pure;

import tech.tablesaw.columns.Column;
import tech.tablesaw.selection.BitmapBackedSelection;
import tech.tablesaw.selection.Selection;

public class SelectionExtensions {

	@Pure
	public static <T> Selection eval(final Column<T> col, final Predicate<T> predicate) {
		final Selection selection = new BitmapBackedSelection();
		for (int idx = 0; idx < col.size(); idx++) {
			final T next = col.get(idx);
			if (predicate.test(next)) {
				selection.add(idx);
			}
		}
		return selection;
	}

	@Pure
	public static Selection operator_not(final Selection selection) {
		return selection.flip(0, selection.size());
	}

	public static Selection operator_singleAndAssign(final Selection sel1, final Selection sel2) {
		return sel1.and(sel2);
	}
	public static Selection operator_singleOrAssign(final Selection sel1, final Selection sel2) {
		return sel1.or(sel2);
	}
	public static Selection operator_remove(final Selection sel1, final Selection sel2) {
		return sel1.andNot(sel2);
	}

	public static Selection operator_add(final Selection sel1, final int row) {
		return sel1.add(row);
	}

	public static Selection operator_remove(final Selection sel, final int row) {
		return sel.removeRange(row, row + 1);
	}

	public static Selection operator_add(final Selection sel, final Iterable<Integer> range) {
	    for (Integer i : range) {
	        sel.add(i);
	    }
	    return sel;
	}

	public static Selection operator_remove(final Selection sel, final Iterable<Integer> range) {
	    for (Integer i : range) {
	        sel.removeRange(i, i + 1);
	    }
	    return sel;
	}

	public static Selection operator_add(final Selection sel, final IntegerRange range) {
		return sel.addRange(range.getStart(), range.getEnd());
	}

	public static Selection operator_remove(final Selection sel1, final IntegerRange range) {
		return sel1.removeRange(range.getStart(), range.getEnd() + 1);
	}
}

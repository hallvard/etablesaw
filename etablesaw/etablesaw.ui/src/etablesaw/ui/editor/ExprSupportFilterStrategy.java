package etablesaw.ui.editor;

import java.util.Map;

import org.eclipse.nebula.widgets.nattable.filterrow.IFilterStrategy;

import etablesaw.ui.expr.ExprSupport;
import tech.tablesaw.selection.BitmapBackedSelection;
import tech.tablesaw.selection.Selection;

public class ExprSupportFilterStrategy<T> extends ExprSupportHelper implements IFilterStrategy<T> {

	public ExprSupportFilterStrategy(final TablesawDataProvider dataProvider, final ExprSupport exprSupport) {
		super(dataProvider, exprSupport);
	}

	private Selection selection = null;
	private Map<Integer, Object> filterIndexToObjectMap;
	
	@Override
	public void applyFilter(final Map<Integer, Object> filterIndexToObjectMap) {
	    selection = null;
	    this.filterIndexToObjectMap = filterIndexToObjectMap;
	    applyExprs(filterIndexToObjectMap.keySet());
		dataProvider.applyFilter(selection);
		selection = null;
		this.filterIndexToObjectMap = null;
	}

    @Override
    protected String getExpr(int columnIndex) {
        Object object = filterIndexToObjectMap.get(columnIndex);
        return (object != null ? String.valueOf(object) : null);
    }

    @Override
    protected boolean handleResult(int rowIndex, int columnIndex, Object result) {
        if (selection == null) {
            selection = new BitmapBackedSelection(dataProvider.getTable().rowCount());
        }
        if (! Boolean.TRUE.equals(result)) {
            selection.removeRange(rowIndex, rowIndex + 1);
            return false;
        }
        return true;
    }
}

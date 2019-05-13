package etablesaw.ui.editor;

import java.util.Collections;

import org.eclipse.nebula.widgets.nattable.command.ILayerCommandHandler;
import org.eclipse.nebula.widgets.nattable.edit.command.UpdateDataCommand;
import org.eclipse.nebula.widgets.nattable.layer.ILayer;

import etablesaw.ui.expr.ExprSupport;

public class UpdateDataExprCommandHandler extends ExprSupportHelper implements ILayerCommandHandler<UpdateDataCommand> {

    public UpdateDataExprCommandHandler(final ExprSupport exprSupport, TablesawDataProvider dataProvider) {
        super(dataProvider, exprSupport);
    }
    
    @Override
    public Class<UpdateDataCommand> getCommandClass() {
        return UpdateDataCommand.class;
    }

    @Override
    public boolean doCommand(ILayer targetLayer, UpdateDataCommand command) {
        String stringValue = (command.getNewValue() instanceof String ? (String) command.getNewValue() : null); 
        if (stringValue != null && stringValue.startsWith("=")) {
            updateColumnData(getColumnNum(targetLayer, command.getColumnPosition()), stringValue.substring(1));
            return true;
        } else {
            return false;
        }
    }

    protected int getColumnNum(ILayer targetLayer, int columnPos) {
        return targetLayer.getColumnIndexByPosition(columnPos);
    }

    private String exprString;

    private void updateColumnData(int columnNum, String exprString) {
        this.exprString = exprString;
        applyExprs(Collections.singleton(columnNum));
    }

    @Override
    protected String getExpr(int columnIndex) {
        return exprString;
    }

    @Override
    protected boolean handleResult(int rowIndex, int columnIndex, Object result) {
        updateDataValue(rowIndex, columnIndex, (result instanceof RuntimeException ? null : result));
        return true;
    }
    
    protected void updateDataValue(int rowNum, int columnNum, final Object result) {
        dataProvider.setDataValue(columnNum, rowNum, result);
    }
}

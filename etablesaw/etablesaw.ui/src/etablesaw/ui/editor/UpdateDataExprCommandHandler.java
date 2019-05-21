package etablesaw.ui.editor;

import java.util.Collections;
import java.util.function.Consumer;

import org.eclipse.nebula.widgets.nattable.command.ILayerCommandHandler;
import org.eclipse.nebula.widgets.nattable.edit.command.UpdateDataCommand;
import org.eclipse.nebula.widgets.nattable.layer.ILayer;

import etablesaw.ui.editor.commands.TableCellChangeRecorder;
import etablesaw.ui.editor.commands.TableCellChangeRecorderHelper;
import etablesaw.ui.expr.ExprSupport;

public class UpdateDataExprCommandHandler extends ExprSupportHelper implements ILayerCommandHandler<UpdateDataCommand> {

    private final TableCellChangeRecorderHelper tableCellChangeRecorderHelper;
    
    public UpdateDataExprCommandHandler(final ExprSupport exprSupport, TablesawDataProvider dataProvider, Consumer<TableCellChangeRecorder> recorderConsumer) {
       super(dataProvider, exprSupport);
       tableCellChangeRecorderHelper = new TableCellChangeRecorderHelper(); 
       tableCellChangeRecorderHelper.setDataProvider(dataProvider);
       tableCellChangeRecorderHelper.setRecorderConsumer(recorderConsumer);
    }

    @Override
    public Class<UpdateDataCommand> getCommandClass() {
        return UpdateDataCommand.class;
    }

    private String exprString;
    
    @Override
    public boolean doCommand(ILayer targetLayer, UpdateDataCommand command) {
        String stringValue = (command.getNewValue() instanceof String ? (String) command.getNewValue() : null); 
        if (stringValue != null && stringValue.startsWith("=")) {
            return tableCellChangeRecorderHelper.doWithRecording(() -> {
                this.exprString = stringValue.substring(1);
                applyExprs(Collections.singleton(getColumnNum(targetLayer, command.getColumnPosition())));
                return true;
            });
        } else {
            return false;
        }
    }

    protected int getColumnNum(ILayer targetLayer, int columnPos) {
        return targetLayer.getColumnIndexByPosition(columnPos);
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

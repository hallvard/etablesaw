package etablesaw.ui.editor.commands;

import org.eclipse.nebula.widgets.nattable.command.ILayerCommand;
import org.eclipse.nebula.widgets.nattable.command.ILayerCommandHandler;
import org.eclipse.nebula.widgets.nattable.layer.ILayer;

public class TableCellChangeRecorderCommandHandler<T extends ILayerCommand> extends TableCellChangeRecorderHelper implements ILayerCommandHandler<T> {

    private final Class<T> commandClass;
    private final ILayerCommandHandler<T> delegate;

    public TableCellChangeRecorderCommandHandler(Class<T> commandClass, ILayerCommandHandler<T> delegate) {
        this.commandClass = commandClass;
        this.delegate = delegate;
    }

    @Override
    public Class<T> getCommandClass() {
        return commandClass;
    }

    @Override
    public boolean doCommand(ILayer targetLayer, T command) {
        return doWithRecording(getRecorder() == null && acceptCommand(command),
                () -> delegate.doCommand(targetLayer, command));
    }
    
    protected boolean acceptCommand(T command) {
        return commandClass.isInstance(command);
    }
}

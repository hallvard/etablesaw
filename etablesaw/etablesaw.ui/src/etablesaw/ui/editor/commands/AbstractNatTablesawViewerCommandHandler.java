package etablesaw.ui.editor.commands;

import org.eclipse.nebula.widgets.nattable.command.ILayerCommand;
import org.eclipse.nebula.widgets.nattable.command.ILayerCommandHandler;

import etablesaw.ui.editor.NatTablesawViewer;

public abstract class AbstractNatTablesawViewerCommandHandler<T extends ILayerCommand> implements ILayerCommandHandler<T> {

    private final NatTablesawViewer natTablesawViewer;
    
    public AbstractNatTablesawViewerCommandHandler(NatTablesawViewer natTablesawViewer) {
        this.natTablesawViewer = natTablesawViewer;
    }
    
    public NatTablesawViewer getNatTablesawViewer() {
        return natTablesawViewer;
    }
}

package etablesaw.ui.editor.commands;

import org.eclipse.nebula.widgets.nattable.command.ILayerCommand;

import etablesaw.ui.editor.NatTablesawEditor;

public abstract class AbstractNatTablesawEditorCommandHandler<T extends ILayerCommand> extends AbstractNatTablesawViewerCommandHandler<T> {

    private final NatTablesawEditor natTablesawEditor;
    
    public AbstractNatTablesawEditorCommandHandler(NatTablesawEditor natTablesawEditor) {
        super(natTablesawEditor.getNatTablesawViewer());
        this.natTablesawEditor = natTablesawEditor;
    }
    
    public NatTablesawEditor getNatTablesawEditor() {
        return natTablesawEditor;
    }
}

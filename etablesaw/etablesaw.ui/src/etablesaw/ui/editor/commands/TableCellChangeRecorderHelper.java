package etablesaw.ui.editor.commands;

import java.util.function.Consumer;
import java.util.function.Supplier;

import etablesaw.ui.editor.TablesawDataProvider;

public class TableCellChangeRecorderHelper {

    private Supplier<TablesawDataProvider> dataProviderSupplier;
    private Consumer<TableCellChangeRecorder> recorderConsumer;
    
    public void setDataProvider(Supplier<TablesawDataProvider> dataProviderSupplier) {
        this.dataProviderSupplier = dataProviderSupplier;
    }

    public void setDataProvider(final TablesawDataProvider dataProvider) {
        setDataProvider(() -> dataProvider);
    }
    
    public void setRecorderConsumer(Consumer<TableCellChangeRecorder> recorderConsumer) {
        this.recorderConsumer = recorderConsumer;
    }

    private TableCellChangeRecorder recorder = null;

    public <R> R doWithRecording(Supplier<R> runnable) {
        return doWithRecording(true, runnable);
    }

    public <R> R doWithRecording(boolean record, Supplier<R> runnable) {
        if (record) {
            recorder = new TableCellChangeRecorder(dataProviderSupplier.get());
        }
        try {
            return runnable.get();
        } finally {
            if (record) {
                getRecorder().stopRecording();
                if (getRecorder().hasRecorded()) {
                    if (recorderConsumer != null) {
                        recorderConsumer.accept(getRecorder());
                    }
                }
                recorder = null;
            }
        }
    }

    public TableCellChangeRecorder getRecorder() {
        return recorder;
    }
}
